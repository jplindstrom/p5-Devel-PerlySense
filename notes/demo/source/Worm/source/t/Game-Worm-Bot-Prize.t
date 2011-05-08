#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";

use Data::Dumper;



use_ok("Game::Lawn");
use_ok("Game::Object::Worm::Bot");
use_ok("Game::Object::Wall");
use_ok("Game::Object::Prize");
use_ok("Game::UI");


my ($width, $height) = (76, 21);

ok(my $oLawn = Game::Lawn->new($width, $height), "new");

is($oLawn->width, $width, " width");
is($oLawn->height, $height, " height");

ok(my $oUI = Game::UI->new($oLawn), "new Display");
is($oUI->offsetLeft(2), 2, "offsetLeft");
is($oUI->offsetTop(1), 1, "offsetTop");
ok($oLawn->oUI($oUI), "ui set to lawn");
is($oUI->initScreen(), 1, "init");



my ($left, $top) = (55, 18);

ok(my $oWorm = Game::Object::Worm::Bot->new($left, $top, "left", 10), "new worm");
ok($oLawn->placeObjectAt($oWorm), "placed worm ok");


my ($wallLeft, $wallTop) = ($oWorm->oLocation->left - 2, $oWorm->oLocation->top - 10);
ok(my $oWall = Game::Object::Wall->new(Game::Location->new($wallLeft, $wallTop), "horizontal", 15), "new wall");
ok($oLawn->placeObjectAt($oWall), "placed wall ok");



my ($prizeLeft, $prizeTop) = ($left - 1, 3);
ok(my $oPrize = Game::Object::Prize->new(Game::Location->new($prizeLeft, $prizeTop), 100), "new prize");
ok($oLawn->placeObjectAt($oPrize), "placed prize ok");




ok($oWorm->probabilityTurnTowardsPrize(1), "probabilityTurnTowardsPrize");

is($oLawn->oDirectionToPrize($oWorm->oLocation), undef, "No prize in any direction");

ok($oWorm->moveForward(), "moved forward");
is($oLawn->oDirectionToPrize($oWorm->oLocation)->direction, "up", "prize in up direction");
is($oWorm->oDirection->direction, "up", "facing left");

$oWorm->moveForward();
$oWorm->turn("right");
$oWorm->moveForward();
$oWorm->turn("left");
$oWorm->moveForward() for(1..8);
ok($oWorm->turn("left"), "turned left");

ok($oWorm->moveForward(), "move forward");
is($oWorm->oDirection->direction, "left", "facing left");



1;
__END__