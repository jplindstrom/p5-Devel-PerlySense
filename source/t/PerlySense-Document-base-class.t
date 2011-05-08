#!/usr/bin/perl -w
use strict;

use Test::More tests => 13;
use Test::Exception;

use Data::Dumper;
use File::Basename;
use File::Spec::Functions;

use lib "../lib";

use_ok("Devel::PerlySense::Document");


BEGIN { -d "t" and chdir("t"); }


ok(my $oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");

my $dirData = "data/simple-lib";
my $fileOrigin = "$dirData/lib/Win32/Word/Writer.pm";

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");


is($oDocument->aNameBase() + 0, 0, "No base classes ok");





ok($oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");

$dirData = "data/project-lib";
$fileOrigin = "$dirData/Game/Object/Worm/Bot.pm";

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");

is_deeply([ $oDocument->aNameBase() ], ["Game::Object::Worm"], "One base class (use base) ok");




ok($oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");

$dirData = "data/project-lib";
$fileOrigin = "$dirData/Game/Object/Worm/ShaiHulud.pm";

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");
#print Dumper($oDocument->raToken);

ok(eq_set([ $oDocument->aNameBase() ], ["Game::Object::Worm", "Game::Lawn"]), 'Two base classes (@ISA = ...) ok');




ok($oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");

$dirData = "data/project-lib";
$fileOrigin = "$dirData/Game/Object/Worm/Shaitan.pm";

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");

ok(eq_set([ $oDocument->aNameBase() ], ["Game::Lawn", "Game::Object::Worm"]), 'Two base classes (push @ISA) ok');





__END__
