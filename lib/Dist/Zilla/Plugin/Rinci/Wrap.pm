package Dist::Zilla::Plugin::Rinci::Wrap;

use 5.010001;
use strict;
use warnings;

use Perinci::Sub::Wrapper qw(wrap_sub);

# VERSION

use Moose;
use experimental 'smartmatch';
use namespace::autoclean;

with (
    'Dist::Zilla::Role::FileMunger',
    'Dist::Zilla::Role::FileFinderUser' => {
        default_finders => [':InstallModules'],
    },
);

# the content will actually be eval'ed
has debug => (
    isa     => 'Bool',
    default => sub { 0 },
    is      => 'rw',
);

# the content will actually be eval'ed
has wrap_args => (
    isa     => 'Str',
    default => sub { '{}' },
    is      => 'rw',
);

has _wrap_args_compiled => (
    isa => 'Bool',
    is  => 'rw',
);

sub _squish_code {
    my ($self, $code) = @_;
    return $code if $self->debug;
    for ($code) {
        s/^\s*#.+//mg; # comment line
        s/^\s+//mg;    # indentation
        s/\n+/ /g;     # newline
    }
    $code;
}

sub munge_files {
    my $self = shift;

    $self->munge_file($_) for @{ $self->found_files };
    return;
}

sub munge_file {
    my ($self, $file) = @_;

    state $wrap_args;
    unless ($self->_wrap_args_compiled) {
        $self->log_debug("Compiling code in wrap_args option ...");
        my $val = $self->wrap_args;
        if ($val) {
            $val = eval $val;
            $self->log_fatal('wrap_args must evaluate to a hashref')
                unless ref($val) eq 'HASH';
            $wrap_args = $val;
        } else {
            $wrap_args = {};
        }
        $self->_wrap_args_compiled(1);
    }

    my $fname = $file->name;
    $self->log_debug("Processing file $fname ...");

    unless ($fname =~ m!lib/(.+\.pm)$!) {
        #$self->log_debug("Skipping: '$fname' not a module");
        return;
    }
    my $req_name = $1;

    my $pkg_name = $req_name;
    $pkg_name =~ s/\.pm$//;
    $pkg_name =~ s!/!::!g;

    # i do it this way (unshift @INC, "lib" + require "Foo/Bar.pm" instead of
    # unshift @INC, "." + require "lib/Foo/Bar.pm") in my all other Dist::Zilla
    # and Pod::Weaver plugin, so they can work together (require "Foo/Bar.pm"
    # and require "lib/Foo/Bar.pm" would cause Perl to load the same file twice
    # and generate redefine warnings).

    local @INC = ("lib", @INC);

    eval { require $req_name };
    if ($@) {
        $self->log_fatal("$fname: has compile errors: $@");
        return;
    }

    my @content = split /^/, $file->content;
    my %wres; # wrap results
    my $metas = do { no strict 'refs'; \%{"$pkg_name\::SPEC"} };

    my @requires; # list of requires that the wrapper code needs
    # generate wrapper for all subs
    for (keys %$metas) {
        next unless /\A\w+\z/; # skip non-functions
        $self->log_debug("Generating wrapper code for sub '$_' ...");
        my $res = wrap_sub(
            %{ $wrap_args },
            sub_name  => "$pkg_name\::$_",
            meta      => $metas->{$_},
            meta_name => "\$$pkg_name\::SPEC{$_}",
            _extra_sah_compiler_args => {comment=>0},
            embed=>1,
        );
        unless ($res->[0] == 200) {
            $self->log_fatal("Can't wrap $_: $res->[0] - $res->[1]");
            return;
        }
        $wres{$_} = $res->[2];
        my $src = $res->[2]{source};
        for (split /^/, $src->{presub1}) {
            push @requires, $_ unless $_ ~~ @requires;
        }
    }

    return unless keys %wres;

    my $i = 0; # line number
    my $in_pod;
    my $sub_name; # current subname
    my $sub_indent;

    my $has_put_preamble;
    my $has_postamble;
    my $has_put_postamble;

    # no package declaration found, deduce from the file name

  LINE:
    for (@content) {
        $i++;
        if (/^=cut\b/x) {
            $in_pod = 0;
            next;
        }
        next if $in_pod;
        if (/^=\w+/x) {
            $in_pod++;
            next;
        }

        if (/^(\s*)sub \s+ (\w+)/x) {
            $self->log_debug("Found sub declaration: $2");
            unless ($sub_name) {
                # this is the first sub, let's put all requires here
                $_ = $self->_squish_code(join "", @requires) . " " . $_;
            }
            ($sub_indent, $sub_name) = ($1, $2);
            next unless $wres{$sub_name};
            # put modify-meta code
            $_ = $self->_squish_code($wres{$sub_name}{source}{presub2}) . $_;
            $has_put_preamble  = 0 || !$wres{$sub_name}{source}{preamble};
            $has_postamble     = $wres{$sub_name}{source}{postamble} ? 1:0;
            $has_put_postamble = 0 || !$has_postamble;
            next;
        }

        next unless $sub_name;

        # 'my %args = @_' statement
        if (/^\s*my \s+ [%@$]args \s* = /x) {
            # put preamble code
            my $preamble = $wres{$sub_name}{source}{preamble};
            if ($has_postamble) {
                $preamble .= '$_w_res = do { ';
            }
            $_ .= $self->_squish_code($preamble);
            $has_put_preamble = 1;
            next;
        }

        # sub closing statement
        if (/^${sub_indent}\}/) {
            unless ($has_put_preamble) {
                $self->log_fatal("[sub $sub_name] hasn't put preamble wrapper code yet");
                return;
            }
            next unless $has_postamble;

            # put postamble code
            my $postamble = "}; " . # for closing of the do { block
                $wres{$sub_name}{source}{postamble};
            $_ = $self->_squish_code($postamble) . " " . $_;
            $has_put_postamble = 1;

            # mark sub done by deleting entry from %wres
            delete $wres{$sub_name};

            next;
        }
    }

    if (!$has_put_postamble && $sub_name) {
        $self->log_fatal("[sub $sub_name] hasn't put postamble wrapper code yet");
        return;
    }

    if (keys %wres) {
        $self->log_fatal("Some subs are not yet wrapped: ".
                             join(", ", sort keys %wres));
        return;
    }

    $self->log("Adding wrapper code to $fname ...");
    $file->content(join "", @content);
    return;
}

__PACKAGE__->meta->make_immutable;
1;
# ABSTRACT: Insert wrapper-generated code

=for Pod::Coverage ^(munge_file|munge_files)$

=head1 SYNOPSIS

In dist.ini:

 [Rinci::Wrap]
 ; optional, will be eval'ed as Perl code and passed to wrap_sub()
 wrap_args = { validate_result => 0, convert => {retry=>2} }

In your module:

 $SPEC{foo} = {
     v => 1.1,
     args => {
         arg1 => { schema => ['int*', default=>3] },
         arg2 => { },
     },
 };
 sub foo {
     my %args = @_;

     ... your code
     return [200, "OK", "some result"];
 }

output will be something like:

 $SPEC{foo} = {
     v => 1.1,
     args => {
         arg1 => { schema => ['int*', default=>3] },
         arg2 => { },
     },
 };
 require Scalar::Util; require Data::Dumper; { my $meta = $SPEC{foo}; $meta->{args}{arg1}{schema} = ["int", {req=>1, default=>3}, {}]; } # WRAP_PRESUB
 sub foo {
     my %args = @_;

     ... generated preamble code

     ... your code
     return [200, "OK", "some result"];

     ... generated postamble code
 }


=head1 DESCRIPTION

This plugin inserts code generated by L<Perinci::Sub::Wrapper> to your source
code during building. This lets you add functionalities like argument
validation, result validation, automatic retries, conversion of argument passing
style, currying, and so on.

Code is inserted in three places (see the above example in Synopsis):

=over

=item *

The first part (which is the part to load required modules and to modify
function metadata, e.g. normalize Sah schemas, etc) will be inserted right
before the opening of the subroutine (C<sub NAME {>).

=item *

The second part (which is the part to validate arguments and do stuffs before
performing the function) will be inserted at the start of subroutine body after
the C<my %args = @_;> (or C<my $args = $_[0] // {};> if you accept arguments
from a hashref, or C<my @args = @_;> if you accept arguments from an array, or
C<my $args = $_[0] // [];> if you accept arguments from an arrayref) statement.
This should be one of the first things you write after your sub declaration
before you do anything else.

=item *

The third part (which is the part to validate function result and do stuffs
after performing the function) will be inserted right before the closing of the
subroutine.

=back

Currently regexes are used to parse the code so things might be rather fragile.


=head1 RESTRICTIONS

There are some restrictions (hopefully not actually restricting) when writing
your code if you want to use this plugin.

=over

=item * Clash of variables

The generated wrapper code will declare some variables. You need to make sure
that the variables do not clash. This is rather simple: the variables used by
the wrapper code will all be prefixed with C<_w_> (e.g. C<$_w_res>) or C<_sahv_>
for variables generated by the L<Sah> schema compiler (e.g. <$_sahv_dpath>).

=item * Variable used to accept arguments

Currently the wrapper internally will perform argument validation on
C<$args{ARGNAME}> variables, even if you accept arguments from a
hashref/array/arrayref. Thus:

If you accept arguments from a hash (the default), you need to put the arguments
to C<%args>, i.e.:

 my %args = @_;

You can then get the validated arguments e.g.:

 my $name = $args{name};
 my $addr = $args{address};
 ...

If you accept arguments from a hashref (i.e. C<< func({ arg1=>1, arg2=>2 }) >>):

 my $args = $_[0] // {};

If you accept arguments from an array (e.g. C<<func(1, 2)>>:

 my @args = @_;

If you accept arguments from an arrayref C<<func([1, 2])>>:

 my $args = $_[0] // [];

=back


=head1 FAQ

=head2 Rationale for this plugin?

This plugin is an alternative to using L<Perinci::Sub::Wrapper> (PSW)
dynamically. During build, you generate the wrapper code and insert it to the
target code. The result is lower startup overhead (no need to generate the
wrapper code during runtime) and better guarantee that your wrapping code
(argument validation, etc) is always called when your subroutines are called,
even if your users do not use PSW and call your subroutines directly.

Another advantage/characteristic using this plugin is that, the wrapper code
does not introduce extra call level.

=head2 But why use PSW at all?

In short, adding L<Rinci> metadata to your subroutines allows various tools to
do useful stuffs, relieving you from coding those stuffs manually. Using L<Sah>
schema allows you to write validation code succintly, and gives you the ability
to automatically generate Perl/JavaScript/error messages from the schema.

PSW is one of the ways (currently the only way) to implement those
behaviours/functionalities.

=head2 But the generated code looks ugly!

Admittedly, yes. Wrapper-generated code is formatted as a single long line to
avoid modifying line numbers, which is desirable when debugging your modules. An
option to not compress everything as a single line might be added in the future.


=head1 TODO

=over

=item * Use L<PPI> instead of fragile regex.

=item * Option to not compress wrapper code to a single line.

=item * Option to reuse validation code for the same schema.

=item * Option to exclude some subroutines from being wrapped.

=item * Option to specify different wrap_args for different subroutines.

=back

=cut
