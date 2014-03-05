package Dist::Zilla::Plugin::Rinci::Wrap;

use 5.010001;
use strict;
use warnings;

use Data::Sah;
use Perinci::Access::Perl 0.54;

my $sah = Data::Sah->new();
my $plc = $sah->get_compiler("perl");
$plc->indent_character('');
my $pa  = Perinci::Access::Perl->new(
    load               => 0,
    cache_size         => 0,
    extra_wrapper_args => {remove_internal_properties=>0},
);

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

sub __squish_code {
    my $code = shift;
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

    my $fname = $file->name;
    $self->log_debug("Processing file $fname ...");

    unless ($fname =~ m!lib/(.+\.pm)$!) {
        #$self->log_debug("Skipping: '$fname' not a module");
        return;
    }
    my $reqname = $1;

    # i do it this way (unshift @INC, "lib" + require "Foo/Bar.pm" instead of
    # unshift @INC, "." + require "lib/Foo/Bar.pm") in my all other Dist::Zilla
    # and Pod::Weaver plugin, so they can work together (require "Foo/Bar.pm"
    # and require "lib/Foo/Bar.pm" would cause Perl to load the same file twice
    # and generate redefine warnings).

    local @INC = ("lib", @INC);

    eval { require $reqname };
    if ($@) {
        $self->log_fatal("$fname: has compile errors: $@");
        return;
    }

    my @content = split /^/, $file->content;
    my $munged;
    my $in_pod;
    my ($pkg_name, $sub_name, $metas, $meta, $arg, $var);
    my $sub_has_vargs; # VALIDATE_ARGS has been declared for current sub
    my %vargs; # list of validated args for current sub, val 2=skipped
    my %vsubs; # list of subs
    my %vars;    # list of variables that the generated validator needs
    my @modules; # list of modules that the generated validator needs

    my $i = 0; # line number

    my $check_prev_sub = sub {
        return unless $sub_name;
        return unless $meta;
        my %unvalidated;
        for (keys %{ $meta->{args} }) {
            next unless $meta->{args}{$_}{schema};
            $unvalidated{$_}++ unless $vargs{$_};
        }
        if (keys %unvalidated) {
            $self->log("NOTICE: $fname: Some argument(s) not validated ".
                           "for sub $sub_name: ".
                               join(", ", sort keys %unvalidated));
        } elsif ((grep {$_==1} values %vargs) &&
                     !defined($meta->{"_perinci.sub.wrapper.validate_args"})) {
            $self->log(
                "NOTICE: $fname: You might want to set ".
                    "_perinci.sub.wrapper.validate_args => 0 in metadata ".
                        "for sub $sub_name");
        }
    };

    my $gen_err = sub {
        my ($status, $msg, $cond) = @_;
        if ($meta->{result_naked}) {
            return qq[if ($cond) { die $msg } ];
        } else {
            return qq|if ($cond) { return [$status, $msg] } |;
        }
    };
    my $gen_merr = sub {
        my ($cond, $arg) = @_;
        $gen_err->(400, qq["Missing argument: $arg"], $cond);
    };
    my $gen_verr = sub {
        my ($cond, $arg) = @_;
        $gen_err->(400, qq["Invalid argument value for $arg: \$arg_err"],
                   $cond);
    };

    my $gen_arg = sub {
        my $meta = $metas->{$sub_name};
        my $dn = $arg; $dn =~ s/\W+/_/g;
        my $cd = $plc->compile(
            schema      => $meta->{args}{$arg}{schema},
            err_term    => '$arg_err',
            data_name   => $dn,
            data_term   => $var,
            return_type => 'str',
            comment     => 0,
        );
        my @code;
        for (@{$cd->{modules}}) {
            push @code, $plc->stmt_require_module($_, $cd) unless $_ ~~ @modules;
            push @modules, $_;
        }
        for (sort keys %{$cd->{vars}}) {
            push @code, "my \$$_ = ".$plc->literal($cd->{vars}{$_})."; "
                unless exists($vars{$_});
            $vars{$_}++;
        }
        push @code, 'my $arg_err; ' unless keys %vargs;
        push @code, __squish_code($cd->{result}), "; ";
        push @code, $gen_verr->('$arg_err', $arg);
        $vargs{$arg} = 1;
        join "", @code;
    };

    my $gen_args = sub {
        my @code;
        for my $arg (sort keys %{ $meta->{args} }) {
            my $as = $meta->{args}{$arg};
            my $s = $meta->{args}{$arg}{schema};
            my $sn;
            if ($s) {
                $sn = $sah->normalize_schema($s);
            }
            my $has_default = $sn && defined($sn->[1]{default});
            my $kvar; # var to access a hash key
            $kvar = $var; $kvar =~ s/.//;
            $kvar = join(
                "",
                "\$$kvar",
                (($meta->{args_as} // "hash") eq "hashref" ? "->" : ""),
                "{'$arg'}",
            );
            if ($as->{req}) {
                push @code, $gen_merr->("!exists($kvar)", $arg);
            }
            if ($sn) {
                my $dn = $arg; $dn =~ s/\W+/_/g;
                my $cd = $plc->compile(
                    schema      => $sn,
                    schema_is_normalized => 1,
                    err_term    => '$arg_err',
                    data_name   => $dn,
                    data_term   => $kvar,
                    return_type => 'str',
                    comment     => 0,
                );
                for (@{$cd->{modules}}) {
                    push @code, $plc->stmt_require_module($_, $cd) unless $_ ~~ @modules;
                    push @modules, $_;
                }
                for (sort keys %{$cd->{vars}}) {
                    push @code, "my \$$_ = ".$plc->literal($cd->{vars}{$_})."; "
                        unless exists($vars{$_});
                    $vars{$_}++;
                }
                push @code, 'my $arg_err; ' unless keys %vargs;
                $vargs{$arg} = 1;
                my $wrap = !$as->{req} && !$has_default;
                push @code, "if (exists($kvar)) { " if $wrap;
                push @code, __squish_code($cd->{result}), "; ";
                push @code, $gen_verr->('$arg_err', $arg);
                push @code, "}"                     if $wrap;
            }
        }
        join "", @code;
    };

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
        if (/^\s*package \s+ (\w+(?:::\w+)*)/x) {
            $pkg_name = $1;
            $self->log_debug("Found package declaration $pkg_name");
            my $uri = "pl:/$pkg_name/"; $uri =~ s!::!/!g;
            my $res = $pa->request(child_metas => $uri);
            unless ($res->[0] == 200) {
                $self->log_fatal(
                    "$fname: can't child_metas => $uri: ".
                        "$res->[0] - $res->[2]");
                return;
            }
            $metas = $res->[2];
            next;
        }
        if (/^\s*sub \s+ (\w+)/x) {
            $self->log_debug("Found sub declaration $1");
            unless ($pkg_name) {
                $self->log_fatal(
                    "$fname:$i: module does not have package definition");
            }
            $check_prev_sub->();
            $sub_name      = $1;
            $sub_has_vargs = 0;
            %vargs         = ();
            @modules       = ();
            %vars          = ();
            $meta          = $metas->{$sub_name};
            next;
        }
        if (/^\s*?
             (?<code>\s* my \s+ (?<sigil>[\$@%]) (?<var>\w+) \b .+)?
             (?<tag>\#\s*(?<no>NO_)?VALIDATE_ARG(?<s> S)?
                 (?: \s+ (?<var2>\w+))? \s*$)/x) {
            my %m = %+;
            $self->log_debug("Found line with tag $_, m=" .
                                 join(', ', map {"$_=>$m{$_}"} keys %m));
            next if !$m{no} && !$m{code};
            $arg = $m{var2} // $m{var};
            if ($m{no}) {
                if ($m{s}) {
                    %vargs = map {$_=>2} keys %{$meta->{args} // {}};
                } else {
                    $vargs{$arg} = 2;
                }
                next;
            }
            $var = $m{sigil} . $m{var};
            unless ($sub_name) {
                $self->log_fatal("$fname:$i: # VALIDATE_ARG$m{s} outside sub");
            }
            unless ($meta) {
                $self->log_fatal(
                    "$fname:$i: sub $sub_name does not have metadata");
            }
            if (($meta->{v} // 1.0) != 1.1) {
                $self->log_fatal(
                    "$fname:$i: metadata for sub $sub_name is not v1.1 ".
                        "(currently only v1.1 is supported)");
            }
            if (($meta->{args_as} // "hash") !~ /^hash(ref)?$/) {
                $self->log_fatal(
                    "$fname:$i: metadata for sub $sub_name: ".
                        "args_as=$meta->{args_as} (sorry, currently only ".
                            "args_as=hash/hashref supported)");
            }
            unless ($meta->{args}) {
                $self->log_fatal(
                    "$fname:$i: # metadata for sub $sub_name: ".
                        "no args property defined");
            }
            if ($m{s} && $sub_has_vargs) {
                $self->log_fatal(
                    "$fname:$i: multiple # VALIDATE_ARGS for sub $sub_name");
            }
            if (!$m{s}) {
                unless ($meta->{args}{$arg} && $meta->{args}{$arg}{schema}) {
                    $self->log_fatal(
                        "$fname:$i: metadata for sub $sub_name: ".
                            "no schema for argument $arg");
                }
            }
            if ($m{s} && $m{sigil} !~ /[\$%]/) {
                $self->log_fatal(
                    "$fname:$i: invalid variable $var ".
                        "for # VALIDATE_ARGS, must be hash/hashref");
            }
            if (!$m{s} && $m{sigil} ne '$') {
                $self->log_fatal(
                    "$fname:$i: invalid variable $var ".
                        "for # VALIDATE_ARG, must be scalar");
            }

            $munged++;
            if ($m{s}) {
                $_ = $m{code} . $gen_args->() . "" . $m{tag};
            } else {
                $_ = $m{code} . $gen_arg->() . "" . $m{tag};
            }
        }
    }
    $check_prev_sub->();

    if ($munged) {
        $self->log("Adding argument validation code for $fname");
        $file->content(join "", @content);
    }

    return;
}

__PACKAGE__->meta->make_immutable;
1;
# ABSTRACT: Insert wrapper-generated code

=for Pod::Coverage ^(munge_file|munge_files)$

=head1 SYNOPSIS

In dist.ini:

 [Rinci::Wrap]

In your module:

 $SPEC{foo} = {
     v => 1.1,
     args => {
         arg1 => { schema => ['int*', default=>3] },
         arg2 => { },
     },
 };
 # WRAP_PRESUB
 sub foo {
     my %args = @_;

     # WRAP_PREAMBLE

     ... your code
     return [200, "OK", "some result"];

     # WRAP_POSTAMBLE
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

     ... generated preamble code # WRAP_PREAMBLE

     ... your code
     return [200, "OK", "some result"];

     ... generated postamble code # WRAP_POSTAMBLE
 }


=head1 DESCRIPTION

This plugin inserts code generated by L<Perinci::Sub::Wrapper> to your source
code during building. This lets you add functionalities like argument
validation, result validation, automatic retries, conversion of argument passing
style, currying, and so on.

Code is inserted in three places (see the above example in Synopsis) in places
marked respectively by the C<#WRAP_PRESUB>, C<#WRAP_PREAMBLE>, and
C<#WRAP_POSTAMBLE>. If you do not supply the markers in the target code:

=over

=item *

The first part (which is the part to load required modules and to modify
function metadata, e.g. normalize Sah schemas, etc) will be inserted right
before the opening of the subroutine (C<sub NAME {>).

=item *

The second part (which is the part to validate arguments and do stuffs before
performing the function) will be inserted at the start of subroutine body after
the C<my %args = @_;> statement.

=item *

The third part (which is the part to validate function result and do stuffs
after performing the function)

=back


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

=back

=cut
