=head1 NAME

Devel::PerlySense - IntelliSense for Perl


=head1 DESCRIPTION

PerlySense is an IntelliSense style utility for editors.

Conveniently navigate and browse the code and documentation of your
project and Perl installation.

Run tests and scripts and syntax check source with easy navigation to
errors/warnings/failing tests.

Highlight syntax errors, warnings and Perl::Critic complaints in the
source while editing.



=head1 SYNOPSIS


=head2 From Emacs

C-o C-c -- Class Overview -- Show information about the Class at point
or the current Class.

C-o C-d -- Smart Docs -- Show docs (POD/signature/etc) for the symbol
(module/method/sub) at point. A doc hint is displayed in the message
area (for methods and subs), or a new POD buffer is created (for
modules).

C-o C-g -- Smart Go to -- Open file at proper location for module,
method/sub declaration for the symbol (module/method/sub) at point. If
no sub declaration is available (like for generated getters/setters),
any appropriate POD is used instead.

C-o C-r -- Run file -- Run the current file using the Compilation mode
and the settings appropriate for the source type (Test, Module,
etc.). Highlight errors and jump to source with C-c C-c.

C-o m f -- Perl Module open File -- Open the source file of the module
at point.

Flymake may be used to highlight syntax errors and warnings in the
source while editing (continously or at every save).



=head2 From Vim

There is no integraton with Vim available. Well, yet.

Someone may want to write it.



=head2 From other editors

Any editor that is programmable and that can call a shell script could
take advantage of at least some parts of PerlySense to implement
something similar to the Emacs functionality. And most editors are
programmable by the authors, if not by the users.



=head2 From the command line

  perly_sense create_project [--dir=DIR]

Create a PerlySense project in DIR (default is current dir).

If there is already a project.yml file, back it up with a datestamp
first.


  perly_sense process_project

Cache all modules in the project. (not implemented)


  perly_sense process_inc

Cache all the modules in @INC.

This is a useful thing to do after installation (and after each
upgrade), but it will take a wile so put it in the background and let
it churn away at those modules.



=head2 From Perl

See the source of the L<bin/perly_sense> script, or the t directory.



=head1 INSTALLATION

=head2 perly_sense installation

Install required modules from CPAN.


=head2 Emacs installation

Make sure the Devel::PerlySense CPAN module is installed, it contains
the required elisp files which will be loaded automatically with the
following in your .emacs config file:


    ;; *** PerlySense Config ***

    ;; ** PerlySense **
    ;; The PerlySense prefix key (unset only if needed)
    (global-unset-key "\C-o")
    (setq perly-sense-key-prefix "\C-o")


    ;; ** Flymake **
    ;; Load flymake if t
    ;; Flymake must be installed.
    ;; It is included in Emacs 22, or available from
    ;;   http://flymake.sourceforge.net/
    ;; Put flymake.el somewhere in your load-path.
    (setq perly-sense-load-flymake t)
    ;; Note: more flymake config below, after loading PerlySense


    ;; *** PerlySense load (don't touch) ***
    (setq perly-sense-external-dir (shell-command-to-string "perly_sense external_dir"))
    (if (string-match "Devel.PerlySense.external" perly-sense-external-dir)
        (progn
          (message
           "PerlySense elisp files  at (%s) according to perly_sense, loading..."
           perly-sense-external-dir)
          (setq load-path (cons
                           (expand-file-name
                            (format "%s/%s" perly-sense-external-dir "emacs")
                            ) load-path))
          (load "perly-sense")
          (if perly-sense-load-flymake (load "perly-sense-flymake"))
          )
      (message "Could not identify PerlySense install dir.
    Is Devel::PerlySense installed properly?
    Does 'perly_sense external_dir' give you a proper directory? (%s)" perly-sense-external-dir)
      )


    ;; ** Flymake Config **

    ;; If you only want syntax check whenever you save, not continously
    (setq flymake-no-changes-timeout 9999)
    (setq flymake-start-syntax-check-on-newline nil)

    ;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
    ;; These colors work fine with a white X11 background. They may not look
    ;; that great on a console with the default color scheme.
    (set-face-background 'flymake-errline "antique white")
    (set-face-background 'flymake-warnline "lavender")


    ;; *** PerlySense End ***


The load path is handled automatically by asking "perly_sense
external_dir" where the elisp source was installed (that way,
Devel::PerlySense and the elisp is always in sync).



=head2 Emacs Configuration

The most important config you can change is the prefix key.

The default, \C-o, seemed to have a rater low useful-to-keystroke
ratio and so was a strong candidate for stealing for this much more
important purpose :)

If you want to use flymake to do background syntax and Perl::Critic
checks, set perly-sense-load-flymake to t (this is a very nifty thing,
so yes you want to do this) and configure the colors to your liking.

Note: This also needs to be enabled on a per-project basis.



=head1 GETTING STARTED WITH EMACS


=head2 Smart docs

C-o C-d is the "Smart docs" command. It brings up documentation for
what's at point.

Put the cursor on the "method" word of a $self->method call and press
C-o C-d and wait until a documentation hint for the method call is
displayed briefly in the message buffer. PerlySense will look in base
classes if the method can't be found in the current class.

Put the cursor on the "method" word of an $object->method call and
press C-o C-d to see the docs hint. PerlySense will look through all
your "use" modules (and their base classes) for the method call and
try to identify the best match.

Note! The first time each module is parsed this will take a second or
two, and the very first time you run the command with lots of "use"
modules it's bound to take longer than that.

Put the cursor on a module name and press C-o C-d to bring up a new
buffer with the POD for that module (this is similar to the cperl-mode
feature, only a) not as good, but b) it works on Windows).

Press C-o C-d with nothing under the cursor brings up a POD buffer for
the current file.


=head2 Smart go to

C-o C-g is the "Smart go to" command. It's similar to Smart Docs, but
instead of bringing the docs to you, it brings you to the definition
of what's at point.

The definition can be either the sub declaration, or if the
declaration can't be found (like for auto-generated getters/setters,
autoloaded subs etc), the POD documentation for the sub.

Before you go anywhere the mark is set. Go back to earlier marks
globally with C-x C-SPC, or locally with C-u C-SPC.


=head2 Go to Class

C-o m f -- Go to Class (will be changed to C-o g c) at point.


=head2 Class Overview

Pressing C-o C-c will bring up the Class Overview of the Class name at
point (not yet implemented), or otherwise the current Class (the
active Package).

Example class CatalystX::FeedMe::Controller::Feed

  * Inheritance *
       [ Class::Accessor                     ]
    +> [ Class::Accessor::Fast               ] <-----+
    |  [ Catalyst::AttrContainer             ] ------+---------------------------+
    |    |                                           |                           v
    +- [ Catalyst::Base                      ] --> [ Catalyst::Component ] --> [ Class::Data::Inheritable ]
       [ Catalyst::Controller                ]
       [<CatalystX::FeedMe::Controller::Feed>]

  * Uses *
  [ Data::Dumper      ] [ XML::Atom::Syndication::Content ] [ XML::Atom::Syndication::Feed ]
  [ Template::Filters ] [ XML::Atom::Syndication::Entry   ] [ XML::Atom::Syndication::Link ]

  * NeighbourHood *
  [ CatalystX::FeedMe::DBIC ] [<CatalystX::FeedMe::Controller::Feed    >] -none-
                              [ CatalystX::FeedMe::Controller::FeedItem ]
                              [ CatalystX::FeedMe::Controller::Homepage ]
                              [ CatalystX::FeedMe::Controller::Root     ]

  * Bookmarks *
  - Todo
  Feed.pm:83: remove duplication

  * Structure *
  ;;;';;;;;;;{;";}{;;;;{}"";{}"";";}{;;;';';";;;{;'";;';;;};';}S{;";"";;}S{;'{}{"};;;}S{;;;;";"
  ;;};


The B<Inheritance> section shows all Base classes of the
Class. Inheriting from something like Catalyst is hopefully the
hairiest you'll see. Classes inherit from their parents upwards in the
diagram unless there is an arrow pointing elsewhere.

The B<Uses> section shows all used modules in the Class.

The B<NeighbourHood> section shows three columns (1: parent dir, 2:
current dir, 3: subdir for the current class) with Classes located
nearby.

The B<Bookmarks> section shows matches for bookmark definitions you
have defined in the Project config (see below).

The B<Structure> section shows a Signature Survey of the file, with an
extreme abbreviation of the source. The intent is to convey an ambient
feel for what the source contains. It's not clear this is a useful
thing (it will probably be ditched).


When in the Class Overview buffer:

g -- Go to the file of the thing at point.

d -- Documentation for the thing at point.

c -- Class Overview for the thing at point. RET does the same.

I -- Move point to the Inheritance heading in the buffer.

U -- Move point to the Uses heading in the buffer.

H -- Move point to the NeighbourHood heading (mnemonic: 'Hood).

B -- Move point to the Bookmarks heading.

S -- Move point to the Structure heading.

M -- Move point to the Methods heading.

N -- Move point to the 'new' method in the buffer (if any).

q -- Quit the Class Overview buffer.



=head2 Run File

C-o C-r -- Run the file of the current buffer using the Compilation
mode.

Files are run according to the source type, which is determined by the
file name (see the config file).  The default for .t files is to run
"prove -v", for .pm files "perl -c", etc. This can be configured per
Project (see below).

The file is run from the Project root directory or from the file
directory depending on the file type, and the @INC is set
appropriately. You can also specify additional @INC directories in the
Project config.

(Note that you can configure whatever type of run profile you like,
not just Perl source files.

As a taste of what's possible, imagine that you have a test framework
with .yml acceptance test data files and a corresponding yml-runner.pl
script. With the x option (not implemented) you can edit the .yml file and
type C-o C-r to run the acceptance test the same way as a regular
test.)

If any warnings, errors or test failures are encountered, they are
highlighted in the *compilation* buffer. Use C-c C-c to move from one
error to the next. Or press RET on a highlighted line.

If you wish to start many runs at the same time, rename the
compilation buffer with "M-x rename-buffer".


=head2 Re-run File

Invoke C-o C-r from within the *compilaton* buffer to re-run (M-x
recompile) the file. Useful when you have skipped around the source
fixing errors and the .t file isn't visible.

C-o r r -- If not even the *compilation* buffer is visible, issue
Re-Run File from anywhere to bring it up and re-run.



=head2 Go to Error line

If you run tests in a regular shell (inside Emacs or in a terminal
window), this may be handy.

C-o g e -- If point is located on an error line from a syntax error,
or a stack trace from the debugger or similar, go to that file+line.

If no file name can be found, prompt for a piece of text that contains
the file+line spec. The kill ring or clipboard text is used as default
if available (so it's easy to just copy the error line from the
terminal, run this command and hit return to accept the default text).



=head2 Flymake Introduction

"Flymake performs on-the-fly syntax checks of the files being edited
using the external syntax check tool (usually the compiler).
Highlights erroneous lines and displays associated error messages."

Flymake is included in Emacs 22 (or available from
http://flymake.sourceforge.net/, put flymake.el somewhere in your
load-path. [[[explain how to fix brokenness?]]] ).

PerlySense uses flymake to check syntax, Perl Critic, etc.

Three inconveniences with vanilla Flymake are fixed: no proper @INC,
only .pl files, and "perl -c" warns about redefined subs for
recursively used modules (which is perfectly fine Perl).

Syntax errors and warnings both use the error face.

Perl Critic violations use the warning face.



=head2 Enabling Flymake

First off, flymake itself needs to be enabled. Refer to the Emacs
Installation description.

This will enable Flymake for all cperl-mode buffers, causing Emacs to
call perly_sense for each check.

PerlySense won't do anything at this point though. You still need to
configure what should happen during a flymake.

Create a PerlySense Project directory (see below) and look in the
project.yml file for instructions on how to configure Flymake
activities.

Set "syntax" / "critic" to 1 to enable them.

The primary reason "syntax" is turned off by default is that it's a
potential security hole; running "perl -c" on a file will not only
check the syntax; BEGIN and CHECK blocks are also executed. Doing that
on random code may be considered... baaad.

This way you can have Flymake enabled globally and still not run "perl
-c" on everything that happens to be in a buffer.



=head2 Using Flymake

In the Project config file there are some hints on how to customize
Flymake, when it should run, etc. You can also customize it with "M-x
customize-group flymake".

(Personally I find the nagging while I type very distracting, but I
welcome the immediate feedback whenever I save the file. YMMV.)

Look in the mode line for hints on whether there are any errors or
warnings.

C-o s n -- Go to the next Source error/warning.

Display the error in the minibuffer. If the warning is from a
Perl::Critic module, copy the module name into the kill-ring, so you
easily can yank it into the .perlcritic config file to disable
it. (not implemented)

C-o s p -- Go to the previous Source error/warning.

C-o s s -- Display the error/warning text of the current line.



=head1 THE PERLYSENSE USER DIRECTORY

PerlySense keeps a per-user directory to store cache files, logs,
etc. The C<.PerlySense> user directory is located under the first
available of these environment variables:

  $APPDATA
  $ALLUSERSPROFILE
  $USERPROFILE
  $HOME
  $TEMP
  $TMP



=head1 PROJECTS

PerlySense has the concept of a Project root directory.

If you follow the standard directory structure for CPAN modules, the
Project directory is typically the one which contains the Makefile.PL,
the lib, bin, and t directory, etc.

Basically, this is where all the source lives, and where your program
can go to find modules that are used. This is from where tests are run
and files are found.



=head2 Identifying a Project root directory

The fastest and most solid way for PerlySense to know which is the
Project directory is to create a .PerlySenseProject directory with a
config file in it. This is highly recommended for all of your own
projects.

The complete project identification strategy is as follows:


=over 4

=item *

First, if there is any directory upwards in the dirctory path with a
.PerlySenseProject dir in it, that is the Project directory.


=item *

Second, PerlySense will try figure out from where the current file (if
any) was being required/used given the contained package names or used
modules.


=item *

Third, if that doesn't work, PerlySense will look for lib and t
directories.

=back

If that doesn't work, PerlySense is lost and you really do need to
create an explicit Project directory by running the following command
in your intended Project root directory (that would typically be the
directory which has a "lib" directory in it):

  perly_sense create_project

Any existing .PerlySenseProject/project.yml config file will be
renamed.



=head2 Project Configuration

The Project has a .PerlySenseProject/project.yml config file. Here you
can change the name of the Project, add extra @INC directories, etc.

There is a yaml-mode for Emacs, but I haven't got it to work properly
(unless an infinite loop counts as "properly" these days). The
shell-script-mode is good enough.

The config file documentation is where it belongs, in the config file,
so just take a look at it.



=head2 perly_sense Project commands


  perly_sense create_project [--dir=DIR]

Create a PerlySense project in DIR (default is current dir).



  perly_sense process_project

Cache all modules in the project. (not implemented)



=head1 BOOKMARKS

Bookmarks are regexes that may match against a single line. Each
bookmark definition has a name/moniker under which the matches are
grouped in the Class Overview display.

The primary point of Bookmarks is to highlight unusual things in the
source. The secondary to make it easy for you go navigate to them.

This can be anything you like, but things that come to mind are:

=over 4

=item * TODO comments

=item * FIXME/XXX/HACK comments

=item * Things you don't want left in the code, like

Breakpoints ($DB::single = 1)

Debugging warn/print statements

=back


=head2 Configuration

Bookmarks are defined in the Project Config file (technical details
are documented there).



=head1 IN CLOSING -- ON PARSING PERL

Since Perl is so dynamic, a perfect static analysis of the source is
impossible. But not unusably so. Well, hopefully. Most of the time.

Because of this PerlySense is not about exact rules, but about
heuristics and a 90% solution that isn't perfect, but good-enough.

PerlySense tries to take advantage of the fact that Perl code is more
than the plain source file. The source lives in a context of POD and a
directory structure and common Perl idioms.

Sometimes when PerlySense can't make a decision, you're expected to
chip in and tell it what you meant.

Sometimes it won't work at all.

Such is the way of dynamic languages.

If it works for you, brilliant, use it to be more productive. If
not...  well, there's always Java >:)



=head1 SEE ALSO

L<sepia> - similar effort

L<PPI> - excellent for parsing Perl

L<CPANXR> - also uses PPI for cross referencing the CPAN

L<http://www.DarSerMan.com/Perl/Oasis/> - Win32 class
browser/IDE. Earlier (a lot) work by me.

L<http://c2.com/doc/SignatureSurvey/> - The idea behind Signature
Surveys. Introduced in this article about Software Archeology
(L<http://www.pragmaticprogrammer.com/articles/mar_02_archeology.pdf>).



=head1 AUTHOR

Johan Lindström, C<< <johanl[ÄT]DarSerMan.com> >>

=head1 BUGS AND CAVEATS

=head2 BUG REPORTS

Please report any bugs or feature requests to
C<bug-devel-perlysense@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-PerlySense>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.


=head2 CAVEATS

Tab/space isn't supported by PPI yet, but it's supposed to be. So
using Tab instead of spaces won't work properly.



=head2 KNOWN BUGS

PPI is kinda slow for large documents. Lots of objects being created etc.

There are certainly edge cases. Bug reports with failing tests
appreciated :)


=head1 ACKNOWLEDGEMENTS

Peter Liljenberg for his elisp fu.


=head1 COPYRIGHT & LICENSE

Copyright 2007 Johan Lindström, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut





use strict;
use warnings;

package Devel::PerlySense;
our $VERSION = '0.0131';



use Spiffy -Base;
use Carp;
use Data::Dumper;
use File::Basename;
use File::Path;
use File::Find::Rule;
use Path::Class qw/dir file/;
use Pod::Text;
use IO::String;
use Cache::Cache;
use Storable qw/freeze thaw/;

use Devel::PerlySense::Util;
use Devel::PerlySense::Util::Log;
use Devel::PerlySense::Project;
use Devel::PerlySense::Project::Unknown;
use Devel::PerlySense::Config::Project;
use Devel::PerlySense::Home;
use Devel::PerlySense::Class;
use Devel::PerlySense::Document;
use Devel::PerlySense::Document::Location;
use Devel::PerlySense::BookmarkConfig;




=head1 *** THE FOLLOWING IS DEVELOPER DOCUMENTATION ***





=head1 PROPERTIES

=head2 oCache

Cache::Cache object, or undef if no cache is active.

Default: undef

=cut
field "oCache" => undef;





=head2 oProject

Devel::PerlySense::Project object.

Default: A Devel::PerlySense::Project::Unknown object.

=cut
field "oProject" => Devel::PerlySense::Project::Unknown->new();




=head2 oHome

Devel::PerlySense::Home object.

Default: A newly created Home object.

=cut
field "oHome" => Devel::PerlySense::Home->new();





=head2 rhConfig

Hash ref with the current config.

If there is a known Project, it reflects the Project's config,
otherwise it's the default config.

Readonly. Note that the _entire_ data structure is readonly. Each time
you change/add/remove a value from it, a kitten is slain. So, dude,
just don't go there!

=cut
sub rhConfig {
    return $self->oProject->rhConfig;
}





=head2 oBookmarkConfig

Devel::PerlySense::BookmarkConfig object.

=cut
field "oBookmarkConfig" => undef;





=head1 API METHODS

=head2 new()

Create new PerlySense object.

=cut
sub new() {
    my $self = bless {}, shift;
    $self->oBookmarkConfig(Devel::PerlySense::BookmarkConfig->new( oPerlySense => $self ));
    return($self);
}





=head2 setFindProject([file => $file], [dir => $dir])

Identify a project given the $file or $dir, and set the oProject
property.

If there is already a project defined, don't change it.

If no project was found, don't change oProject.

Return 1 if there is a valid project, else 0.

Die on errors.

=cut
sub setFindProject {
    if( ! $self->oProject->isa("Devel::PerlySense::Project::Unknown")) {
        return 1;
    }

    my $oProject = Devel::PerlySense::Project->newFromLocation(
        @_,
        oPerlySense => $self,
    ) or return 0;
    $self->oProject($oProject);

    return(1);
}





=head2 oDocumentParseFile($file)

Parse $file into a new PerlySense::Document object.

Return the new object.

Die on errors (like if the file wasn't found).

=cut
sub oDocumentParseFile {
	my ($file) = @_;

    my $oDocument = Devel::PerlySense::Document->new(oPerlySense => $self);
    $oDocument->parse(file => $file);

    return($oDocument);
}





=head2 podFromFile(file => $file)

Return the pod in $file as text, or die on errors.

Die if $file doesn't exist.

=cut
sub podFromFile {
    my ($file) = Devel::PerlySense::Util::aNamedArg(["file"], @_);

    open(my $fhIn, $file) or die("Could not open file ($file): $!\n");

    my $textPod = "";
    my $fhOut = IO::String->new($textPod);
    Pod::Text->new()->parse_from_filehandle($fhIn, $fhOut);

    return($textPod);
}





=head2 oLocationSmartGoTo(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what is
there. Depending on what's there, find the source
declaration/whatever, find it and return an
Devel::PerlySense::Document::Location object.

Currently supported:

  $self->method, look in current file and base classes. If no sub can
  be found, look for POD.

  $object->method, look in current file and used modules. If no sub
  can be found, look for POD.

  Module::Name (bareword)

  Module::Name (as the only contents of a string literal)

If there's nothing at $row/col, or if the source can't be found,
return undef.

Die if $file doesn't exist, or on other errors.

=cut
sub oLocationSmartGoTo {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);
    debug("oLocationSmartGoTo file($file) row($row) col($col)");

    my $oDocument = $self->oDocumentParseFile($file);

    {
        if(my $method = $oDocument->selfMethodCallAt(row => $row, col => $col)) {
            my $oLocation = $oDocument->oLocationSubDefinition(row => $row, name => $method);
            $oLocation and return($oLocation);
        }
    }

    my ($module, $method) = $oDocument->moduleMethodCallAt(row => $row, col => $col);
    if($module && $method) {
        if(my $oDocumentDest = $self->oDocumentFindModule(nameModule => $module, dirOrigin => dirname($file))) {
            my $oLocation = $oDocumentDest->oLocationSubDefinition(row => $row, name => $method);
            $oLocation and return($oLocation);
        }
    }


    my ($oObject, $oMethod, $oLocationSub) = $oDocument->aObjectMethodCallAt(row => $row, col => $col);
    if($oObject && $oMethod && $oLocationSub) {
        debug("Looking for $oObject->$oMethod");
        my @aMethodCall = $oDocument->aMethodCallOf(
            nameObject => "$oObject",
            oLocationWithin => $oLocationSub,
        );
        my @aNameModuleUse = $oDocument->aNameModuleUse();  #Add all known modules, not just the ones explicitly stated
        my @aDocumentDest = $self->aDocumentFindModuleWithInterface(
            raNameModule => \@aNameModuleUse,
            raMethodRequired => [ "$oMethod" ] ,
            raMethodNice => \@aMethodCall,
            dirOrigin => dirname($file),
        );
        if(@aDocumentDest) {
            debug("Possible matching modules:\n" . join("\n", map { "  * $_" } map { @{$_->oMeta->raPackage} } @aDocumentDest));
            my $oLocation = $aDocumentDest[0]->oLocationSubDefinition(
                row => $row,
                name => "$oMethod",
            );
            $oLocation and return($oLocation);
        }
    }


    if(my $module = $oDocument->moduleAt(row => $row, col => $col)) {
        my $file = $self->fileFindModule(nameModule => $module, dirOrigin => dirname($file))
                or return(undef);

        my $oLocation = Devel::PerlySense::Document::Location->new(file => $file, row => 1, col => 1);
        return($oLocation);
    }

    return(undef);
}





=head2 oLocationSmartDoc(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what is
there. Depending on what's there, find the documentation for it and
return a Document::Location object with the following rhProperty keys set:

  text - the docs text
  found - "method" | "module"
  docType - "hint" | "document"
  name - the name of the thing found


Currently supported:

  Same as for oLocationSmartGoTo

If there's nothing at $row/col, use the current document.

Die if $file doesn't exist, or on other errors.

=cut
#Rework this so it can deal with HTML output as well
sub oLocationSmartDoc {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    my $oDocument = $self->oDocumentParseFile($file);

    my $oLocation = undef;
    if(my $method = $oDocument->selfMethodCallAt(row => $row, col => $col)) {
        $oLocation = $oDocument->oLocationPod(name => $method, lookFor => "method");
        return( $self->oLocationRenderPodToText($oLocation) );
    }

    my ($module, $method) = $oDocument->moduleMethodCallAt(row => $row, col => $col);
    if($module && $method) {
        if(my $oDocumentDest = $self->oDocumentFindModule(nameModule => $module, dirOrigin => dirname($file))) {
            my $oLocation = $oDocumentDest->oLocationPod(name => $method, lookFor => "method");
            return( $self->oLocationRenderPodToText($oLocation) );
        }
    }


    my ($oObject, $oMethod, $oLocationSub) = $oDocument->aObjectMethodCallAt(row => $row, col => $col);
    if($oObject && $oMethod && $oLocationSub) {
        my @aMethodCall = $oDocument->aMethodCallOf(nameObject => "$oObject", oLocationWithin => $oLocationSub);
        my @aNameModuleUse = $oDocument->aNameModuleUse();
        my @aDocumentDest = $self->aDocumentFindModuleWithInterface(raNameModule => \@aNameModuleUse, raMethodRequired => [ "$oMethod" ] , raMethodNice => \@aMethodCall, dirOrigin => dirname($file));
        if(@aDocumentDest) {
            my $oLocation = $aDocumentDest[0]->oLocationPod(name => "$oMethod", lookFor => "method");
            return( $self->oLocationRenderPodToText($oLocation) );
        }
    }


    if(my $module = $oDocument->moduleAt(row => $row, col => $col)) {
        my $file = $self->fileFindModule(nameModule => $module, dirOrigin => dirname($file))
                or return(undef);

        my $oLocation = Devel::PerlySense::Document::Location->new(file => $file, row => 1, col => 1);
        $oLocation->rhProperty->{found} = "module";
        $oLocation->rhProperty->{docType} = "document";
        $oLocation->rhProperty->{name} = "$module";
        $oLocation->rhProperty->{text} = $self->podFromFile(file => $file) or return(undef);
        return($oLocation);
    }


    #Fail to docs about this current file
    if($oDocument->isEmptyAt(row => $row, col => $col)) {
        $oLocation = Devel::PerlySense::Document::Location->new(file => $file, row => 1, col => 1);
        $oLocation->rhProperty->{found} = "module";
        $oLocation->rhProperty->{docType} = "document";
        $oLocation->rhProperty->{name} = $oDocument->packageAt(row => $row);
        $oLocation->rhProperty->{text} = $self->podFromFile(file => $file) or return(undef);
        return($oLocation);
    }

    return(undef);
}





=head2 rhRunFile(file => $fileSource)

Figure out what type of source file $fileSource is, and how it should
be run.

The settings in the Project's config->{run_file} is used to determine
the details.

Return hash ref with (keys: "dir_run_from", "command_run",
"type_source_file"), or die on errors (like if no Project could be
found).

dir_run_from is an absolute file name which should be the cwd when
command_run is executed.

type_source_file is something like "Test", "Module".

=cut
sub rhRunFile {
    my ($file) = Devel::PerlySense::Util::aNamedArg(["file"], @_);

    $self->setFindProject(file => $file)
            or die("Could not identify any PerlySense Project\n");

    return $self->oProject->rhRunFile(file => $file);
}





=head2 flymakeFile(file => $fileSource)

Do a flymake run with $fileSource according to the flymake config and
output the result to STDOUT and STDERR.

=cut
sub flymakeFile {
    my ($file) = Devel::PerlySense::Util::aNamedArg(["file"], @_);

    $self->setFindProject(file => $file)
            or die("Could not identify any PerlySense Project\n");

    return $self->oProject->flymakeFile(file => $file);
}





=head2 createProject(dir => $dir)

Create a new PerlySense Project in $dir.

Return 1 on success, or die on errors.

=cut
sub createProject {
    my ($dir) = Devel::PerlySense::Util::aNamedArg(["dir"], @_);

    my $oConfig = Devel::PerlySense::Config::Project->new();
    $oConfig->createFileConfigDefault(dirRoot => $dir);

    ###TODO: assign the config to $self->oConfigProject

    return(1);
}





=head2 classNameAt(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what class name that is.

Return the class name or "" if it's package main.

Die if $file doesn't exist, or on other errors.

=cut
sub classNameAt {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    my $oDocument = $self->oDocumentParseFile($file);

    my $package = $oDocument->packageAt(row => $row);

    $package eq "main" and return "";
    return($package);
}





=head2 classAt(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what
PerlySelse::Class that is.

Return the Class object or undef if it's package main.

Die if $file doesn't exist, or on other errors.

=cut
sub classAt {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    return(Devel::PerlySense::Class->newFromFileAt(
        oPerlySense => $self,
        file => $file,
        row => $row,
        col => $col,
    ));
}





=head2 classByName(name => $name, dirOrigin => $dirOrigin)

Find the file that contains the Class $name, starting at $dirOrigin.

Return the Class object or undef if it couldn't be found.

Die on errors.

=cut
sub classByName {
    my ($name, $dirOrigin) = Devel::PerlySense::Util::aNamedArg(["name", "dirOrigin"], @_);

    my $oDocument = $self->oDocumentFindModule(
        nameModule => $name,
        dirOrigin => $dirOrigin,
    ) or return undef;

    return( Devel::PerlySense::Class->new(
        oPerlySense => $self,
        name => $name,
        raDocument => [ $oDocument ],
    ) );
}





=head2 fileFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin)

Find the file containing the $nameModule given the $dirOrigin.

Return the absolute file name, or undef if none could be found. Die on
errors.

=cut
sub fileFindModule {
    my ($nameModule, $dirOrigin) = Devel::PerlySense::Util::aNamedArg(["nameModule", "dirOrigin"], @_);
#my $tt = Devel::TimeThis->new("fileFindModule");
    my $fileModuleBase = $self->fileFromModule($nameModule);
    $dirOrigin = dir($dirOrigin)->absolute;

    return(
        $self->fileFindLookingAround($fileModuleBase, $dirOrigin) ||
        $self->fileFindLookingInInc($fileModuleBase) ||
        undef
    );
}





=head2 oDocumentFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin)

Find the file containing the $nameModule given the $dirOrigin.

Return a parsed PerlySense::Document, or undef if none could be
found. Die on errors.

=cut
sub oDocumentFindModule {
    my ($nameModule, $dirOrigin) = Devel::PerlySense::Util::aNamedArg(["nameModule", "dirOrigin"], @_);

    my $fileModule = $self->fileFindModule(
        nameModule => $nameModule,
        dirOrigin => $dirOrigin,
    ) or return(undef);

    my $oDocument = $self->oDocumentParseFile($fileModule) or return(undef);

    return($oDocument);
}





=head1 IMPLEMENTATION METHODS

=head2 fileFindLookingAround($fileModuleBase, $dirOrigin)

Find the file containing the $fileModuleBase given the $dirOrigin.

Return the file name relative to $dirOrigin, or undef if none could be
found. Die on errors.

=cut
sub fileFindLookingAround {
	my ($fileModuleBase, $dirOrigin) = @_;

    my $dir = dir($dirOrigin);
    while(1) {
        for my $dirCur (map { dir($dir, $_) } qw/. bin lib/) {
            if(my $fileFound = $self->fileFoundInDir($dirCur, $fileModuleBase)) {
                return(file($fileFound)->absolute . "");
            }
        }

        $dir = $dir->parent;
        $dir =~ m{^( / | \\ | \w: \\ )$}x and last;  #At the root? Unix/Win32. What filesystems are missing?
    }

    return(undef);
}





=head2 dirFindLookingAround($fileModuleBase, $dirOrigin, [$raDirSub = [".", "lib", "bin"]])

Find the dir containing the $fileModuleBase (relative file path) given
the $dirOrigin. For all directories, also look in subdirectories in
$raDirSub.

Return the absolute dir name, or undef if none could be found. Die on
errors.

=cut
###TODO: remove duplication
sub dirFindLookingAround {
	my ($fileModuleBase, $dirOrigin, $raDirSub) = @_;
    $raDirSub ||= [".", "lib", "bin"];

    my $dir = dir($dirOrigin);
    while(1) {
        for my $dirCur (map { dir($dir, $_) } @$raDirSub) {
            if($self->fileFoundInDir($dirCur, $fileModuleBase)) {
                return($dirCur->absolute . "");
            }
        }

        $dir = $dir->parent;
        $dir =~ m{^( / | \\ | \w: \\ )$}x and last;  #At the root? Unix/Win32. What filesystems are missing?
    }

    return(undef);
}





=head2 fileFindLookingInInc($fileModuleBase)

Find the file containing the $nameModule in @INC.

Return the absolute file name, or undef if none could be found. Die on
errors.

=cut
sub fileFindLookingInInc {
	my ($fileModuleBase) = @_;

    for my $dirCur (@INC) {
        if(my $fileFound = $self->fileFoundInDir($dirCur, $fileModuleBase)) {
            return($fileFound);
        }
    }

    return(undef);
}





=head2 fileFromModule($nameModule)

Return the $nameModule converted to a file name (i.e. with dirs and
.pm extension).

=cut
sub fileFromModule {
	my ($nameModule) = @_;
    return( file( split(/::/, $nameModule) ) . ".pm" );
}





=head2 fileFoundInDir($dir, $fileModuleBase)

Check if $fileModuleBase is located in $dir.

Return the absolute file name, or "" if not found at $dir.

=cut
sub fileFoundInDir {
	my ($dir, $fileModuleBase) = @_;

    my $file = file($dir, $fileModuleBase);
    -e $file and return( $file->absolute . "" );

    return("");
}





=head2 textFromPod($pod)

Return $pod rendered as text, or die on errors.

=cut
sub textFromPod {
	my ($pod) = @_;

    my $text = "";
    my $fhIn = IO::String->new($pod);
    my $fhOut = IO::String->new($text);
    Pod::Text->new()->parse_from_filehandle($fhIn, $fhOut);

    $text =~ s/\s+$//s;

    return($text);
}





=head2 oLocationRenderPodToText($oLocation)

Render the $oLocation->rhProperty->{pod} and put it in
rhProperty->{text}.

Return the same (modified) $oLocation object, or undef if no
rhProperty->{pod} property ended up as text (after this operation,
there is content in rhProperty->{text}).

Return undef if $oLocation is undef.

Die on errors.

=cut
sub oLocationRenderPodToText {
	my ($oLocation) = @_;
    $oLocation or return(undef);

    my $pod = $oLocation->rhProperty->{pod} or return(undef);
    $oLocation->rhProperty->{text} = $self->textFromPod($pod) or return(undef);

    return($oLocation);
}





=head2 aDocumentFindModuleWithInterface(raNameModule => $raNameModule, raMethodRequired => $raMethodRequired, raMethodNice => $raMethodNice, dirOrigin => $dirOrigin)

Return a list with Devel::PerlySense::Document objects that support
all of the methods in $raMethodRequired and possibly the methods in
$raMethodNice. Look in modules in $raNameModule.

The list is sorted with the best match first.

If the document APIs have one or more base classes, look in the @ISA
(depth-first, just like Perl (see perldoc perltoot)).

Warn on some failures to find the location. Die on errors.

=cut
sub aDocumentFindModuleWithInterface {
    my ($raNameModule, $raMethodRequired, $raMethodNice, $dirOrigin) = Devel::PerlySense::Util::aNamedArg(["raNameModule", "raMethodRequired", "raMethodNice", "dirOrigin"], @_);
#my $tt = Devel::TimeThis->new("aDocumentFindModuleWithInterface");

    my @aDocument;
    for my $nameModule (@$raNameModule) {
#print "module: $nameModule\n";
        my $oDocument = $self->oDocumentFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin) or next;
        $oDocument->determineLikelyApi(nameModule => $nameModule) or next;
        my $score = $oDocument->scoreInterfaceMatch(nameModule => $nameModule, raMethodRequired => $raMethodRequired, raMethodNice => $raMethodNice) or next;

        push(@aDocument, { oDocument => $oDocument, score => $score });
    }

    my @aDocumentWithInterface =
            map { $_->{oDocument} }
            sort { $a->{score} <=> $b->{score} }
            @aDocument;

    return(@aDocumentWithInterface);
}





=head2 aApiOfClass(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what package is
there.

Return a two item array with (Package name,
Devel::PerlySense::Document::Api object with the likely API of that
class), or () if none was found.

Die if $file doesn't exist, or on other errors.

=cut
sub aApiOfClass {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    my $oDocument = $self->oDocumentParseFile($file);
    my $packageName = $oDocument->packageAt(row => $row) or return(undef);

    $oDocument->determineLikelyApi(nameModule => $packageName) or return(undef);

    return($packageName, $oDocument->rhPackageApiLikely->{$packageName});
}





=head2 aDocumentGrepInDir(dir => $dir, rsGrepFile => $rsGrepFile, rsGrepDocument => $rsGrepDocument)

Return a list with Devel::PerlySense::Document objects found under the
$dir, and that return true for the grep sub $rsGrepFile and $rsGrepDocument.

If any found file couldn't be parsed, skip it silently from the list.

=cut
sub aDocumentGrepInDir {
    my ($dir, $rsGrepFile, $rsGrepDocument) = Devel::PerlySense::Util::aNamedArg(["dir", "rsGrepFile", "rsGrepDocument"], @_);

    my @aDocument =
            map {
                my $oDocument = Devel::PerlySense::Document->new(oPerlySense => $self);
                eval { $oDocument->parse(file => $_) };
                $@ ?
                    () :
                    $rsGrepDocument->($oDocument) ?
                        $oDocument :
                        ();
            }
            grep { $rsGrepFile->($_) }
            File::Find::Rule->file->name("*.pm")->in($dir);

    return(@aDocument);
}





=head1 CACHE METHODS


=head2 cacheSet(file => $file, key => $key, value => $valuex)

If the oCache isn't undef, store the $value in the cache under the
total key of ($file, $file's timestamp, $key, and the PerlySense
VERSION).

$value should be a scalar or reference which can be freezed.

$file must be an existing file.

Return 1 if the $value was stored, else 0. Die on errors.

=cut
#Move these to Devel::PerlySense::Util::Cache ?
sub cacheSet {
    my ($file, $key, $value) = Devel::PerlySense::Util::aNamedArg(["file", "key", "value"], @_);

    my $keyTotal = $self->cacheKeyTotal($file, $key) or return(0);

    my $data = freeze($value) or return(0);
    $self->oCache->set($keyTotal, $data);

    return(1);
}





=head2 cacheGet(file => $file, key => $key)

If the oCache isn't undef, get the value in the cache under the total
key of ($file, $file's timestamp, $key) and return it.

$file must be an existing file.

Return the value, or undef if the value could not be fetched. Die on errors.

=cut
sub cacheGet {
    my ($file, $key) = Devel::PerlySense::Util::aNamedArg(["file", "key"], @_);

    my $keyTotal = $self->cacheKeyTotal($file, $key) or
#            warn("Could not get key for ($file) ($key)\n"),
                    return(undef);

    my $data = $self->oCache->get($keyTotal) or
#            warn("?\n"),
                    return(undef);
#warn("!\n");

    my $rValue = thaw($data) or warn("Could not thaw\n"), return(undef);
    return( $rValue );
}





=head2 cacheKeyTotal($file, $key)

If oCache is undef, return undef.

Otherwise, return the total key of ($file, $file's timestamp, $key,
and the PerlySense VERSION).

$file must be an existing file.

Die on errors.

=cut
sub cacheKeyTotal {
    my ($file, $key) = @_;
    $self->oCache or return(undef);

    my $timestamp = (stat($file))[9] or die("Could not read timestamp for file ($file)\n");
    my $keyTotal = join("\t", $file, $timestamp, $key, $self->VERSION);

    return($keyTotal);
}





1;





__END__
