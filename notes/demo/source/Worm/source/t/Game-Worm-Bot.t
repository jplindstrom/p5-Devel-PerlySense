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
is($oWorm->isRealPlayer, 0, "not isRealPlayer");
ok($oLawn->placeObjectAt($oWorm), "placed worm ok");



is($oWorm->oppositeDirection("left"), "right", "opposite");
is($oWorm->oppositeDirection("right"), "left", "opposite");



my ($prizeLeft, $prizeTop) = (59, 19);
ok(my $oPrize = Game::Object::Prize->new(Game::Location->new($prizeLeft, $prizeTop), 100), "new prize");
ok($oLawn->placeObjectAt($oPrize), "placed prize ok");





is($oWorm->turn("left"), "down", "turned left");
ok($oWorm->moveForward(), "moved forward");

is($oWorm->turn("left"), "right", "turned left");
ok($oWorm->moveForward(), "moved forward");

is($oWorm->turn("left"), "up", "turned left");
ok($oWorm->moveForward(), "could move forward after auto-turn");
is($oWorm->oDirection->direction, "right", "direction");

ok($oWorm->moveForward(), "moved forward");


is($oWorm->score, 0, "score 0");
ok($oLawn->isObjectAt($oPrize, $prizeLeft, $prizeTop), "Prize is there");

ok($oWorm->moveForward(), "moved forward onto Prize");

is($oWorm->score, 100, "score 100");
ok(!$oLawn->isObjectAt($oPrize, $prizeLeft, $prizeTop), "Prize is not there");



my ($wallLeft, $wallTop) = ($oWorm->oLocation->left + 2, $oWorm->oLocation->top);
ok(my $oWall = Game::Object::Wall->new(Game::Location->new($wallLeft, $wallTop), "horizontal", 15), "new wall");
ok($oLawn->placeObjectAt($oWall), "placed wall ok");

ok($oWall = Game::Object::Wall->new(Game::Location->new($wallLeft - 1, $wallTop - 1), "horizontal", 3), "new wall");
ok($oLawn->placeObjectAt($oWall), "placed wall ok");

ok($oWall = Game::Object::Wall->new(Game::Location->new($wallLeft - 2, $wallTop + 1), "horizontal", 1), "new wall");
ok($oLawn->placeObjectAt($oWall), "placed wall ok");


ok($oWorm->moveForward(), "moved forward");
ok($oWorm->moveForward(), "couldn't moved forward into wall, turned and moved");

$oWorm->moveForward() for (1..15);

ok(! $oWorm->moveForward(), "couldn't moved forward into cul-de-sac");






ok($oWorm = Game::Object::Worm::Bot->new(20, 7, "left", 10), "new worm");
ok($oLawn->placeObjectAt($oWorm), "placed worm ok");

is($oWorm->probabilityTurnRandomly(0), 0, "zero prob for turning");
my $rhTurnResult = {};
$rhTurnResult->{ $oWorm->possiblyTurnRandomly() }++ for(1..1000);
is($rhTurnResult->{""}, 1000, "correct non-turns");

is($oWorm->probabilityTurnRandomly(1), 1, "zero prob for turning");
$rhTurnResult = { "" => 0 };

my $dir;
for(1..1000) {
	$dir = $oWorm->possiblyTurnRandomly() and $oWorm->turn( $oWorm->oppositeDirection($dir) );	#Turn and turn back
	$rhTurnResult->{ $dir }++ ;
	}
is($rhTurnResult->{""}, 0, "correct non-turns");





$oWorm->probabilityTurnRandomly(0.83);
$oWorm->probabilityTurnTowardsPrize(0.82);

ok(my $oWorm2 = Game::Object::Worm::Bot->new(23, 21, "left", 2), "created new bot");
isnt($oWorm->probabilityTurnRandomly, $oWorm2->probabilityTurnRandomly, "not same probabilityTurnRandomly");
isnt($oWorm->probabilityTurnTowardsPrize, $oWorm2->probabilityTurnTowardsPrize, "not same probabilityTurnTowardsPrize");

ok($oWorm2->cloneValuesFrom($oWorm), "cloned");

is($oWorm->probabilityTurnRandomly, $oWorm2->probabilityTurnRandomly, "same probabilityTurnRandomly");
is($oWorm->probabilityTurnTowardsPrize, $oWorm2->probabilityTurnTowardsPrize, "same probabilityTurnTowardsPrize");





1;
__END__