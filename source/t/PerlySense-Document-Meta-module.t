#!/usr/bin/perl -w
use strict;

use Test::More tests => 28;
use Test::Exception;

use Data::Dumper;
use File::Basename;
use File::Spec::Functions;

use lib "../lib";

use_ok("Devel::PerlySense");
use_ok("Devel::PerlySense::Document");
use_ok("Devel::PerlySense::Document::Meta");


BEGIN { -d "t" and chdir("t"); }


ok(my $oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");


my $dirData = "data/project-lib";
my $fileOrigin = "$dirData/Game/Object/Worm.pm";

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");

my $oMeta = $oDocument->oMeta;

is(scalar(keys %{$oMeta->rhRowColModule}), 18, " found correct number of modules");

is($oMeta->rhRowColModule->{23}->{5}->{oNode} . "", "Data::Dumper", " got module node");
is($oMeta->rhRowColModule->{23}->{5}->{module} . "", "Data::Dumper", " got module");
is($oMeta->rhRowColModule->{24}->{5}->{oNode} . "", "Game::Location", " got module node");
is($oMeta->rhRowColModule->{41}->{5}->{oNode} . "", "Exception::Class", " got module node");
is($oMeta->rhRowColModule->{152}->{24}->{oNode} . "", "Game::Event::Timed", " got module node");
is($oMeta->rhRowColModule->{318}->{13}->{oNode} . "", "ExceptionCouldNotMoveForward", " got module node");

is($oMeta->rhRowColModule->{156}->{17}->{oNode} . "", q{"Carp"}, " got module node, looks somewhat like module, and exists");
is($oMeta->rhRowColModule->{156}->{17}->{module} . "", "Carp", " got module node, looks somewhat like module, and exists");
is($oMeta->rhRowColModule->{157}->{14}->{oNode} . "", q{"File::Spec"}, " got module node, looks like module, good enough");
is($oMeta->rhRowColModule->{157}->{14}->{module} . "", "File::Spec", " got module node, looks like module, good enough");
is($oMeta->rhRowColModule->{171}->{14}->{module} . "", "None::Exsistent::Module", " got module, looks like module, good enough");



#print Dumper($oMeta->rhRowColModule->{42});

is($oMeta->rhRowColModule->{341}->{5}, undef, " no module at sub declaration");
is($oMeta->rhRowColModule->{341}->{28}, undef, " no module at variable name");
is($oMeta->rhRowColModule->{341}->{27}, undef, " no module at variable sigil");
is($oMeta->rhRowColModule->{332}->{1}, undef, " no module at nothing");
is($oMeta->rhRowColModule->{363}->{16}, undef, " no module at string literal");
is($oMeta->rhRowColModule->{365}->{5}, undef, " no module at keyword return");
is($oMeta->rhRowColModule->{161}->{47}, undef, " no module at method call");
is($oMeta->rhRowColModule->{145}->{29}, undef, " no module at numeric literal");


is($oMeta->rhRowColModule->{42}->{5}, undef, " no module at string literal 'Exception'");
is($oMeta->rhRowColModule->{159}->{16}, undef, ' no module at string literal "O"');
is($oMeta->rhRowColModule->{151}->{18}, undef, ' no module at string literal "white"');

__END__
