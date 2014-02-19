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
     args => {
         arg1 => { schema => ['int*', default=>3] },
         arg2 => { },
     },
 };
 # WRAP_META
 sub foo {
     my %args = @_;

     # WRAP_PREAMBLE

     ... your code
     return [200, "OK", "some result"];

     # WRAP_POSTAMBLE
 }

output will be something like:

 $SPEC{foo} = {
     args => {
         arg1 => { schema => ['int*', default=>3] },
         arg2 => { },
     },
 };
 $SPEC{foo} = { ... new Rinci metadata from wrapping result ... }; # WRAP_META
 sub foo {
     my %args = @_;

     ... generated preamble code # WRAP_PREAMBLE

     ... your code
     return [200, "OK", "some result"];

     ... generated postamble code # WRAP_POSTAMBLE
 }


=head1 DESCRIPTION

This plugin inserts code generated by L<Perinci::Sub::Wrapper> to your source
code during building. This lets you add functionalities like argument checking

argument validation code into your module source code, at
location marked with C<# VALIDATE_ARG> or C<# VALIDATE_ARGS>. Validation code is
compiled using C<Data::Sah> from Sah schemas specified in C<args> property in
C<Rinci> function metadata in the module.


=head2 USAGE

To validate a single argument, in your module:

 sub foo {
     my %args = @_;
     my $arg1 = $args{arg1}; # VALIDATE_ARG

The significant part that is interpreted by this module is C<my $arg1>. Argument
name is taken from the lexical variable's name (in this case, C<arg1>). Argument
must be defined in the C<args> property of the function metadata. If argument
name is different from lexical variable name, then you need to say:

 my $f = $args->{frobnicate}; # VALIDATE_ARG frobnicate

To validate all arguments of the subroutine, you can say:

 sub foo {
     my %args = @_; # VALIDATE_ARGS

There should only be one VALIDATE_ARGS per subroutine.

If you use this plugin, and you plan to wrap your functions too using
L<Perinci::Sub::Wrapper> (or through L<Perinci::Access>, L<Perinci::CmdLine>,
etc), you might also want to put C<< _perinci.sub.wrapper.validate_args => 0 >>
attribute into your function metadata, to instruct Perinci::Sub::Wrapper to skip
generating argument validation code when your function is wrapped, as argument
validation is already done by the generated code.

If there is an unvalidated argument, this plugin will emit a warning notice. To
skip validating an argument (silence the warning), you can use:

 sub foo {
     my %args = @_;
     my $arg1 = $args{arg1}; # NO_VALIDATE_ARG

or:

 sub foo {
     # NO_VALIDATE_ARGS


=head1 FAQ

=head2 Rationale for this plugin?

This plugin is an alternative to L<Perinci::Sub::Wrapper>, at least when it
comes to validating arguments. Perinci::Sub::Wrapper can also generate argument
validation code (among other things), but it is done during runtime and can add
to startup overhead (compiling complex schemas for several subroutines can take
up to 100ms or more, on my laptop). Using this plugin, argument validation code
is generated during building of your distribution.

Using this plugin also makes sure that argument is validated whether your
subroutine is wrapped or not. Using this plugin also avoids wrapping and adding
nest level, if that is not to your liking.

Instead of using this plugin, you can use wrapping either by using
L<Perinci::Exporter> or by calling Perinci::Sub::Wrapper's C<wrap_sub> directly.

=head2 But why use Rinci metadata or Sah schema?

In short, adding L<Rinci> metadata to your subroutines allows various tools to
do useful stuffs, relieving you from doing those stuffs manually. Using L<Sah>
schema allows you to write validation code succintly, and gives you the ability
to automatically generate Perl/JavaScript/error messages from the schema.

See their respective documentation for more details.

=head2 But the generated code looks ugly!

Admittedly, yes. Validation source code is formatted as a single long line to
avoid modifying line numbers, which is desirable when debugging your modules. An
option to not compress everything as a single line might be added in the future.


=head1 TODO

=over

=item * Use L<PPI> instead of fragile regex.

=item * Option to not compress validator code to a single line.

=item * Option to configure variable name to store validation (C<$arg_err>).

=item * Option to reuse code for the same schema.

=back

=cut
