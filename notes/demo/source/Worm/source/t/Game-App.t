#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";
use Time::HiRes qw( time sleep );

use Data::Dumper;



use_ok("Game::App");
use_ok("Game::Object::Worm");
use_ok("Game::Object::Worm::Bot");
use_ok("Game::Object::Prize");
use_ok("Game::Object::Wall");


my ($width, $height) = (60, 36);
my ($offsetLeft, $offsetTop) = (4, 4);


ok(my $oApp = Game::App->new($offsetLeft, $offsetTop, $width, $height), "new app");


#Create worm
my ($wormLeft, $wormTop, $wormDirection, $wormLength) = (35, 14, "left", 15);
ok(my $oWorm = Game::Object::Worm->new($wormLeft, $wormTop, $wormDirection, $wormLength), "new worm");

ok(! exists $oApp->rhTimedObject->{$oWorm}, "is not timedObject");
is($oApp->placeWormOnLawn($oWorm), 1, " placed worm on Lawn");
ok(exists $oApp->rhTimedObject->{$oWorm}, "is timedObject");


ok(my $oWormBot = Game::Object::Worm::Bot->new($wormLeft, $wormTop + 15, $wormDirection, $wormLength), "new worm bot");
$oWormBot->probabilityTurnRandomly(0.07);
ok(! exists $oApp->rhTimedObject->{$oWormBot}, "is not timedObject");
is($oApp->placeWormBotOnLawn($oWormBot), 1, " placed worm bot on Lawn");
ok(exists $oApp->rhTimedObject->{$oWormBot}, "is timedObject");


my ($prizeLeft, $prizeTop, $value) = (11, 29, 100);
ok(my $oPrize = Game::Object::Prize->new(Game::Location->new($prizeLeft, $prizeTop), $value), "new Prize");
is($oApp->placePrizeOnLawn($oPrize), 1, " placed worm on Lawn");



my $oWall = Game::Object::Wall->new(Game::Location->new(20, 8), "horizontal", 30);
is($oApp->placeWallOnLawn($oWall), 1, " placed wall on Lawn");

$oWall = Game::Object::Wall->new(Game::Location->new(6, 24), "horizontal", 30);
is($oApp->placeWallOnLawn($oWall), 1, " placed wall on Lawn");



my $wormLengthIdeal = $oWorm->lengthIdeal;
ok($oApp->prizeWasClaimedBy($oPrize, $oWorm), "prizeWasClaimedBy");
is($oWorm->lengthIdeal, $wormLengthIdeal + 1, "worm ideal length");




eval {
	$oApp->run();
	};
like($@, qr/move forward/, "Died ok after crashing into the wall");


is(scalar(keys %{$oApp->rhTimedObject}), 2, "correct number of timed objects");
ok($oApp->wormHasCrashed($oWorm), "bot crashed");
is(scalar(keys %{$oApp->rhTimedObject}), 2, "correct number of timed objects");

ok($oApp->wormHasCrashed($oWormBot), "bot crashed");

is(scalar(keys %{$oApp->rhTimedObject}), 2, "correct number of timed objects");


ok($oApp->removeWormFromLawn($oWorm), "remove worm");
is(scalar(keys %{$oApp->rhTimedObject}), 1, "correct number of timed objects");





1;
__END__