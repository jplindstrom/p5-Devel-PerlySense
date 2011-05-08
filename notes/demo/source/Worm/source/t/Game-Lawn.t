#!perl -w
use strict;

use lib "../lib";

use Test::More tests => 52;

use Data::Dumper;



use_ok("Game::Lawn");



my ($width, $height) = (76, 22);

ok(my $oLawn = Game::Lawn->new($width, $height), "new");

is($oLawn->width, $width, " width");
is($oLawn->height, 17, " height");


is($oLawn->isLocationOnLawn(Game::Location->new(0, 0)), 1, "location is on the lawn");
is($oLawn->isLocationOnLawn(Game::Location->new($width - 1, $height - 1)), 1, "location is on the lawn");

is($oLawn->isLocationOnLawn(Game::Location->new(-1, 0)), 0, "location is not on the lawn");
is($oLawn->isLocationOnLawn(Game::Location->new($width, 0)), 0, "location is not on the lawn");
is($oLawn->isLocationOnLawn(Game::Location->new(0, $height)), 0, "location is not on the lawn");





ok(my $oLocRandom = $oLawn->oLocationRandom());
is($oLawn->isLocationOnLawn($oLocRandom), 1, "random location is on the lawn");



my ($left, $top) = (11, 12);

ok(my $oWorm = $oLawn->oPlaceWorm($left, $top), "placed worm ok");



ok(!$oLawn->oPlaceWorm(-1, $top), "found invalid location");
ok(!$oLawn->oPlaceWorm($left, -1), "found invalid location");
ok( my $oWorm2 = $oLawn->oPlaceWorm($left, 0), "found valid location");
ok( $oLawn->oPlaceWorm($left, $height - 1), "found valid location");
ok(!$oLawn->oPlaceWorm($left, $height), "found invalid location");
ok( $oLawn->oPlaceWorm($width - 3, $top), "found valid location");
ok(!$oLawn->oPlaceWorm($width - 2, $top), "found invalid location");
ok(!$oLawn->oPlaceWorm($width, $top), "found invalid location");

is($oLawn->isObjectAt($oWorm, -1, -1), 0, "no worm at location");
is($oLawn->isObjectAt($oWorm, 0, 0), 0, "no worm at location");
is($oLawn->isObjectAt($oWorm, $left, $top), 1, "worm at location");
is($oLawn->isObjectAt($oWorm, $left, 0), 0, "no worm at location");

#print Dumper($oLawn);

ok($oWorm->moveForward(), "moved forward");
$left--;
is($oWorm->oLocation->left, $left, " to correct left");
is($oWorm->oLocation->top, $top, " to correct top");

is( $oLawn->isObjectAt($oWorm, $left, $top), 1, "worm at location");
is( $oLawn->isObjectAt($oWorm, $left + 1, $top), 1, "worm at location");
is( $oLawn->isObjectAt($oWorm, $left + 2, $top), 1, "worm at location");
is(!$oLawn->isObjectAt($oWorm, $left + 3, $top), 1, "worm at location");


is($oWorm->turn("left"), "down", "turn left");

ok($oWorm->moveForward(), "moved forward");
$top++;
is($oWorm->oLocation->left, $left, " to correct left");
is($oWorm->oLocation->top, $top, " to correct top");

is( $oLawn->isObjectAt($oWorm, $left, $top), 1, "worm at location");
is( $oLawn->isObjectAt($oWorm, $left + 1, $top - 1), 1, "worm at location");
is(!$oLawn->isObjectAt($oWorm, $left + 2, $top - 1), 1, "worm at location");





ok( $oLawn->isLocationValidForMove($oWorm, Game::Location->new(0, 0)), "isLocationValidForMove 0,0");
ok(!$oLawn->isLocationValidForMove($oWorm, Game::Location->new(0, -1)), "not isLocationValidForMove outside");
ok(!$oLawn->isLocationValidForMove($oWorm, Game::Location->new($left, $top)), "not isLocationValidForMove worm head");
ok(!$oLawn->isLocationValidForMove($oWorm, Game::Location->new($left + 1, $top - 1)), "not isLocationValidForMove worm tail");
ok( $oLawn->isLocationValidForMove($oWorm, Game::Location->new($left + 2, $top - 1)), "isLocationValidForMove after worm tail");



my ($prizeLeft, $prizeTop) = (42, 3);
ok(my $oPrize = $oLawn->oPlacePrize(Game::Location->new($prizeLeft, $prizeTop), 150), "placed prize ok");
is($oLawn->isObjectAt($oPrize, $prizeLeft, $prizeTop), 1, "prize there");

ok($oLawn->prizeWasClaimedBy($oPrize, $oWorm), "prizeWasClaimedBy");

is($oLawn->isObjectAt($oPrize, $prizeLeft, $prizeTop), 0, "prize no longer there");





is($oLawn->isObjectAt($oWorm, $left, $top), 1, "worm at location");
is($oWorm->oLawn, $oLawn, "worm on lawn");
ok($oLawn->removeObject($oWorm), "removed worm");
is($oLawn->isObjectAt($oWorm, $left, $top), 0, "worm no longer at location");
is($oWorm->oLawn, undef, "worm no longer on lawn");




1;
__END__
