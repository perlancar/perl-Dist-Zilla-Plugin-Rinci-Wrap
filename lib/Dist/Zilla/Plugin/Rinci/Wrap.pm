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
    isa     => 'Bool',
    is      => 'rw',
);

has _prereqs => (
    is      => 'rw',
);
has _checked_modules => (
    is      => 'rw',
    default => sub { {} },
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

    my $is_lib;
    my $req_name = $1;
    if ($fname =~ m!lib/(.+\.pm)$!) {
        $is_lib = 1;
        $req_name = $1;
    } elsif ($fname =~ m!(?:bin|script)/.+$!) {
        # we need to just compile the script but not execute it, like 'perl -c',
        # but we also need to read its %SPEC. hopefully most apps put their code
        # in lib/App/Foo.pm instead of in the script bin/foo itself.
        $self->log("$fname: WARN: Embedding wrapper code on script ".
                       "not yet supported, skipped");
        return;
    } else {
        #$self->log_debug("$fname: not a module or a script, skipped");
        return;
    }

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

    my @requires; # list of require lines that the wrapper code needs
    my %mods; # list of mentioned modules
    # generate wrapper for all subs
    for (keys %$metas) {
        next unless /\A\w+\z/; # skip non-functions
        $self->log_debug("Generating wrapper code for sub '$_' ...");
        my $res = wrap_sub(
            %{ $wrap_args },
            %{ $metas->{$_}{"x.dist.zilla.plugin.rinci.wrap.wrap_args"} // {} },
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
            if (/^\s*(?:use|require) \s+ (\w+(?:::\w+)*)/x) {
                $mods{$1}++;
            }
        }
    }

    return unless keys %wres;

    # check dist.ini to make sure that non-core modules required by the wrapper
    # code is mentioned in Prereqs. XXX this should use proper method. XXX we
    # should not parse dist.ini for each file.
    {

        unless ($self->_prereqs) {
            require Module::CoreList;

            (-f "dist.ini") or do {
                $self->log_fatal("dist.ini not found, something's wrong");
            };
            my $in_prereqs;
            my %prereqs;
            $self->log_debug("Parsing prereqs in dist.ini ...");
            open my($fh), "<", "dist.ini";
            while (<$fh>) {
                next unless /\S/;
                next if /\s*;/;
                if (/^\s*\[\s*([^\]+]+)\s*\]/) {
                    $in_prereqs = $1 eq 'Prereqs';
                    next;
                }
                next unless $in_prereqs;
                /^\s*(\S+)\s*=\s*(\S+)/ or
                    $self->log_fatal("dist.ini:$.: syntax error");
                $prereqs{$1} = $2;
            }
            #use Data::Dump; dd \%prereqs;
            $self->_prereqs(\%prereqs);
        }

        my $perl_version = $self->_prereqs->{perl};

        for my $mod (sort keys %mods) {
            next if Module::CoreList::is_core($mod, undef, $perl_version);
            next if $self->_checked_modules->{$mod};
            $self->log_debug("Checking if dist.ini contains Prereqs for $mod");
            $self->log_fatal(
                "Wrapper code requires non-core module '$mod', ".
                    "please specify it in dist.ini's Prereqs section")
                unless defined $self->_prereqs->{$mod};
            $self->_checked_modules->{$mod}++;
        }
    }

    my $i = 0; # line number
    my $in_pod;
    my $sub_name; # current subname
    my $sub_indent;

    my $has_put_preamble;
    my $has_postamble;
    my $has_put_postamble;

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
            my $first_sub = !$sub_name;
            ($sub_indent, $sub_name) = ($1, $2);
            unless ($wres{$sub_name}) {
                $self->log_debug("Skipped wrapping sub $sub_name (no metadata)");
                next;
            }
            # put modify-meta code
            $_ = "\n$1# [Rinci::Wrap] END presub2\n$_" if $self->debug;
            $_ = $self->_squish_code($wres{$sub_name}{source}{presub2}). " $_";
            $_ = "\n$1# [Rinci::Wrap] BEGIN presub2\n$_" if $self->debug;
            $has_put_preamble  = 0 || !$wres{$sub_name}{source}{preamble};
            $has_postamble     = $wres{$sub_name}{source}{postamble} ? 1:0;
            $has_put_postamble = 0 || !$has_postamble;

            if ($first_sub) {
                # this is the first sub, let's put all requires here
                $_ = "\n$1# [Rinci::Wrap] END presub1\n$_" if $self->debug;
                $_ = $self->_squish_code(join "", @requires) . " $_";
                $_ = "\n$1# [Rinci::Wrap] BEGIN presub1\n$_" if $self->debug;
            }

            next;
        }

        next unless $sub_name;

        # 'my %args = @_' statement
        if (/^(\s*)(my \s+ [\%\@\$]args \s* = .+)/x) {
            $self->log_debug("[sub $sub_name] Found a place to insert preamble (after '$2' statement)");
            # put preamble code
            $_ = "$_\n$1# [Rinci::Wrap] BEGIN preamble\n" if $self->debug;
            my $preamble = $wres{$sub_name}{source}{preamble};
            if ($has_postamble) {
                $preamble .= $1 . '$_w_res = do {';
            }
            $_ .= " " . $self->_squish_code($preamble);
            $_ = "$_\n$1# [Rinci::Wrap] END preamble\n" if $self->debug;
            $has_put_preamble = 1;
            next;
        }

        # sub closing statement
        if (/^${sub_indent}\}/) {
            $self->log_debug("Found sub closing: $sub_name");
            next unless $wres{$sub_name};

            unless ($has_put_preamble) {
                $self->log_fatal("[sub $sub_name] hasn't put preamble ".
                                     "wrapper code yet");
            }
            goto DONE_POSTAMBLE unless $has_postamble;

            # put postamble code
            my $postamble = "}; " . # for closing of the do { block
                $wres{$sub_name}{source}{postamble};
            $_ = "\n$sub_indent# [Rinci::Wrap] END postamble\n$_"
                if $self->debug;
            $_ = $self->_squish_code($postamble) . " $_";
            $_ = "\n$sub_indent# [Rinci::Wrap] BEGIN postamble\n$_"
                if $self->debug;
            $has_put_postamble = 1;

          DONE_POSTAMBLE:
            # mark sub done by deleting entry from %wres
            delete $wres{$sub_name};

            next;
        }
    }

    if (!$has_put_postamble && $sub_name) {
        $self->log_fatal("[sub $sub_name] hasn't put postamble ".
                             "wrapper code yet");
    }

    if (keys %wres) {
        $self->log_fatal("Some subs are not yet wrapped (probably because I couldn't find sub declaration or a place to insert the preamble/postamble): ".
                             join(", ", sort keys %wres));
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
 ; optional, will not squish code and add marker comment
 debug=1

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
for variables generated by the L<Sah> schema compiler (e.g. C<$_sahv_dpath>).

=item * Variable used to accept arguments

Currently the wrapper internally will perform argument validation on
C<$args{ARGNAME}>> variables, even if you accept arguments from a
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

If you accept arguments from an array (e.g. C<< func(1, 2) >>:

 my @args = @_;

If you accept arguments from an arrayref C<< func([1, 2]) >>:

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
avoid modifying line numbers, which is desirable when debugging your modules. If
you don't want to compress everything as a single line, add C<debug=1> in your
C<dist.ini>.

=head2 How do I customize wrapping for my function

In the future there will be options you can specify in C<dist.ini>.

For now, you can put in your Rinci function metadata:

 "x.dist.zilla.plugin.rinci.wrap.wrap_args" => { validate_args => 0 },

This will be merged and will override C<wrap_args> keys specified in
C<dist.ini>.


=head1 TODO

=over

=item * Use L<PPI> instead of fragile regex.

=item * Option to reuse validation code for the same schema.

=item * Option to exclude some subroutines from being wrapped.

=item * Option to specify different wrap_args for different subroutines.

=back

=cut
