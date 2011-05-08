=head1 NAME

Devel::PerlySense::Project - A Project root

=head1 SYNOPSIS




=head1 DESCRIPTION

A Project describes the root directory of a source tree.

A Project has configuration settings.

=cut





use strict;
use warnings;

package Devel::PerlySense::Project;
our $VERSION = '0.01';



use Spiffy -Base;
use Carp;
use Data::Dumper;
use File::Basename;
use Path::Class;
use File::Slurp;
use File::chdir;
use Perl::Critic;

use Devel::PerlySense;
use Devel::PerlySense::Util;
use Devel::PerlySense::Config::Project::Default;





=head1 PROPERTIES

=head2 dirProject

The effective project root dir.

Readonly.

=cut
sub dirProject {
    return
            $self->dirProjectExplicitDir ||
            $self->dirProjectImplicitUse ||
            $self->dirProjectImplicitDir;
}





=head2 dirProjectExplicitDir

If known, the root dir made explicit by the existance of a project
directory and config, else ""

Default: ""

=cut
field "dirProjectExplicitDir" => "";





=head2 dirProjectImplicitUse

If known, the root dir indicated by a found used module, else "".

Default: ""

=cut
field "dirProjectImplicitUse" => "";





=head2 dirProjectImplicitDir

If known, the root dir indicated by the presence of "lib" or "t", else "".

Default: ""

=cut
field "dirProjectImplicitDir" => "";





=head2 oConfig

A Config::Project object with the current Project Config. This gets
loaded if there is a config file available when the Project is
identified.

Default: An Config::Project::Default object

=cut
field "oConfig" => Devel::PerlySense::Config::Project::Default->new();





=head2 rhConfig

The actual hashref with config values.

Readonly (that includes the entire data structure).

=cut
sub rhConfig {
    return $self->oConfig->rhConfig;
}





=head2 oPerlySense

Devel::PerlySense object.

=cut
field "oPerlySense" => undef;





=head1 CLASS METHODS

=head2 newFromLocation(file => $file, dir => $dir, oPerlySense => $oPs)

Create new Project given either $file, or $dir.

First, search for an explicit project root directory, then try to find
any modules used in $file (if passed), then try to find any "lib" or
"t" directory upwards of $file or $dir.

$file takes precedence over $dir if both are specified.

If none if this works out, no Project can be created and undef is
returned.

Return the new object, or undef if no project could be found.

=cut
sub newFromLocation(@) {
    my $pkg = shift;
    my ($oPerlySense) = Devel::PerlySense::Util::aNamedArg(["oPerlySense"], @_);
    my %p = @_;
    my $file = $p{file};
    my $dir = $p{dir};
    $file and $dir = dirname($file);
    $dir and $dir = dir($dir)->absolute;

    my $oProject;

    $dir and $oProject = $pkg->newFindExplicit(
        dir => $dir,
        oPerlySense => $oPerlySense,
    ) and return $oProject;


    #If file, look for the dir from where any package in the file can
    #be used
    if($file and my $oDocument = $oPerlySense->oDocumentParseFile($file)) {
        for my $package ($oDocument->aNamePackage) {
            my $filePackage = $oPerlySense->fileFromModule($package);
            my $dirFound = $oPerlySense->dirFindLookingAround($filePackage, $dir);

            if($dirFound) {
                my $dirProject = dir($dirFound)->parent . ""; ###TODO: if lib or bin
                return $pkg->new(
                    oPerlySense => $oPerlySense,
                    dirProjectImplicitUse => $dirProject,
                );
            }
        }
    }

    #If dir, look for dirs
    if($dir) {
        my $dirFound =
                   $oPerlySense->fileFindLookingAround("lib", $dir) ||
                   $oPerlySense->fileFindLookingAround("t", $dir);

        if($dirFound) {
            my $dirProject = dir($dirFound)->parent . ""; ###TODO: if lib or bin

            #Special case the Unix style root dir. It's never a
            #Project dir, but if often contains a lib dir and so will
            #get identified as a Project.
            #
            #If it _is_ a root dir, this can be overriden with an
            #explicit .PerlySenseProject dir.
            #
            #On Windows, the root looks like X:\, and it's not
            #entirely unlikely that a secondary drive or a SUBST drive
            #letter contains the project root.
            $dirProject eq "/" and return undef;

            return $pkg->new(
                oPerlySense => $oPerlySense,
                dirProjectImplicitDir => $dirProject,
            );
        }
    }

    return(undef);
}





=head2 newFindExplicit(dir => $dir, oPerlySense => $oPs)

Create new Project if there is an explicit .PerlySenseProject
directory in the path above $dir.

Return the new object, or undef if no project could be found.

=cut
sub newFindExplicit(@) {
    my $pkg = shift;
    my ($oPerlySense, $dir) = Devel::PerlySense::Util::aNamedArg(["oPerlySense", "dir"], @_);

    if(my $dirProject = $oPerlySense->dirFindLookingAround(
        ".PerlySenseProject",
        $dir,
        ["."],
    )) {
        return $pkg->new(
            oPerlySense => $oPerlySense,
            dirProjectExplicitDir => $dirProject,
            oConfig => Devel::PerlySense::Config::Project->new(dirRoot => $dirProject),
        );
    }

    return undef;
}





=head1 MEHTODS

=head2 rhRunFile(file => $fileSource, [rhConfigType = DEDUCED_FROM_FILE])

Figure out what type of source file $fileSource is, and how it should
be run.

The settings in the global config->{run_file} is used to determine the
details.

Return hash ref with (keys: "dir_run_from", "command_run",
"type_source_file"), or die on errors (like if no Project could be
found).

dir_run_from is an absolute file name which should be the cwd when
command_run is executed.

type_source_file is something like "Test", "Module".

=cut
sub rhRunFile {
    my ($file) = Devel::PerlySense::Util::aNamedArg(["file"], @_);
    my %p = @_;
    my $rhConfigType = $p{rhConfigType};

    $file = file($file)->absolute;
    $rhConfigType ||= $self->rhConfigTypeForFile(file => $file);

    my $dirProject = dir($self->dirProject)->absolute;
    my %hTypeDirRunFrom = (
        source_root_directory => sub { $dirProject },
        file_directory => sub { $file->dir },
    );
    my $typeRunFrom = $rhConfigType->{run_from};
    my $rsDirRunFrom = $hTypeDirRunFrom{$typeRunFrom} or die("Invalid run_from value ($typeRunFrom)\n" . Dumper($rhConfigType) . "Allowed values: (" . join(", ", (sort keys %hTypeDirRunFrom)) . ")\n");
    my $dirRunFrom = $rsDirRunFrom->();

    my @aDirIncProject = $self->aDirIncProject(dirRelativeTo => $dirRunFrom);
    my $optionInc = join(" ", map { qq|"-I$_"| } @aDirIncProject);

    my $fileSource = $file->relative($dirRunFrom);
    my $commandRun = textRenderTemplate(
        $rhConfigType->{command}, {
            INC => $optionInc,
            SOURCE_FILE => $fileSource . "",
        },
    );

    my $rhConfigRun = {
        dir_run_from => $dirRunFrom . "",
        command_run => $commandRun,
        type_source_file => $rhConfigType->{moniker},
    };
    return($rhConfigRun);
}





=head2 rhConfigTypeForFile(file => $fileSource)

Return the config type hash ref (keys: command, moniker) from the ones
available in the config. Match the $fileSource name against each rex
in the config.

Die if no configType could be identified.

=cut
sub rhConfigTypeForFile {
    my ($file) = Devel::PerlySense::Util::aNamedArg(["file"], @_);

    my $rhConfig = $self->oPerlySense->rhConfig;
    for my $rhConfigType (@{ $rhConfig->{run_file} }) {
        my $rex = $rhConfigType->{rex}
                or die("Missing rex key in config chunk: " . Dumper($rhConfigType));

        eval { $file =~ /$rex/ } and return $rhConfigType;
        $@ and die("Invalid rex value in config chunk.\nError: $@\n" . Dumper($rhConfigType));
    }

    die("No run_perl rex matched the file ($file). Please check your config\n");
}





=head2 aDirIncProject(dirRelativeTo => $dirRelativeTo)

Return array wirth dir objects which are the inc_dir directories in
the config, plus the usual inc directories. They are all relative to
$dirRelativeTo.

=cut
sub aDirIncProject {
    my ($dirRelativeTo) = Devel::PerlySense::Util::aNamedArg(["dirRelativeTo"], @_);

    my $dirProject = dir($self->dirProject)->absolute;
    my $dirProjectRelativeTo = $dirProject->relative( $dirRelativeTo );

    my @aDirIncProject = @{ $self->oPerlySense->rhConfig->{project}->{inc_dir} || [] };
    my @aDirInc = (@aDirIncProject, ".", "lib");

    my @aDirIncRelative =
            map { dir($dirProjectRelativeTo, $_) . "" }
            @aDirInc;

    return(@aDirIncRelative);
}





=head2 flymakeFile(file => $fileSource)

Do a flymake run with $fileSource according to the flymake config and
output the result to STDOUT and STDERR.

Return 1 or die on errors (but that will look like the result of a
failed flymake run).

=cut
sub flymakeFile {
    my ($file) = Devel::PerlySense::Util::aNamedArg(["file"], @_);

    if($self->oPerlySense->rhConfig->{external}{editor}{emacs}{flymake}{syntax}) {
        my $rhConfigRun = $self->rhRunFile(
            file => $file,
            rhConfigType =>  {
                command => q{perl -c ${INC} "${SOURCE_FILE}" 2>&1 | perl -ne " /Subroutine [\\w:]+ redefined at/ or print"},
                moniker => "Flymake",
                rex => "",
                run_from => "file_directory",
            },
        );

        local $CWD = $rhConfigRun->{dir_run_from};
        system( $rhConfigRun->{command_run} );
    }

    if($self->oPerlySense->rhConfig->{external}{editor}{emacs}{flymake}{critic}) {
        ###TODO: don't run if syntax errors found

        my $fileConfigCritic = file(
            dir($self->dirProject)->absolute, ".PerlySenseProject", ".perlcritic",
        );
        
        my @aOption = (-profile => $fileConfigCritic . "");
        -e $fileConfigCritic or @aOption = ();
        
        my $oCritic = Perl::Critic->new(@aOption);

        my @aViolation = $oCritic->critique($file);

        local $Perl::Critic::Violation::FORMAT = "%m near '%r' (%e, %p) at %f line %l.\n";
        for my $violation (@aViolation) {
            print STDERR "Warning: $violation";
        }
    }

    return 1;
}





1;





__END__

=head1 AUTHOR

Johan Lindström, C<< <johanl[ÄT]DarSerMan.com> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-devel-perlysense@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-PerlySense>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2005 Johan Lindström, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
