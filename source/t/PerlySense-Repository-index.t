#!/usr/bin/perl -w
use strict;

use Test::More tests => 8;
use Test::Exception;

use Data::Dumper;

use lib "../lib";

use_ok("Devel::PerlySense::Repository");
use_ok("Devel::PerlySense::Document::Api");
use_ok("Devel::PerlySense::Document");
use_ok("Devel::PerlySense");


my $dirData = "t/data/project-lib";
my $fileOrigin = "$dirData/Game/Event/Timed.pm";
my $nameModule = "Game::Event::Timed";

my $oLocation;
my $method;

my $oPs = Devel::PerlySense->new();
ok(my $oDocument = Devel::PerlySense::Document->new(oPerlySense => $oPs), "new ok");
ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");

ok($oDocument->determineLikelyApi(nameModule => $nameModule), "determineLikelyApi ok");


my $oRepository = Devel::PerlySense::Repository->new( oPerlySense => $oPs );
ok($oRepository, "Created Repository ok");

$oRepository->index(oDocument => $oDocument);



__END__
