#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";

use Data::Dumper;



use_ok("Game::Lawn");
use_ok("Game::Object::Worm");
use_ok("Game::Object::Wall");
use_ok("Game::Object::Prize");
use_ok("Game::UI");


my ($width, $height) = (76, 22);

ok(my $oLawn = Game::Lawn->new($width, $height), "new");

is($oLawn->width, $width, " width");
is($oLawn->height, $height, " height");

ok(my $oUI = Game::UI->new($oLawn), "new Display");
is($oUI->offsetLeft(2), 2, "offsetLeft");
is($oUI->offsetTop(1), 1, "offsetTop");
ok($oLawn->oUI($oUI), "ui set to lawn");
is($oUI->initScreen(), 1, "init");



my ($left, $top) = (55, 18);

ok(my $oWorm = Game::Object::Worm->new($left, $top, "left", 10), "new worm");
ok($oLawn->placeObjectAt($oWorm), "placed worm ok");


my ($prizeLeft, $prizeTop) = (59, 19);
ok(my $oPrize = Game::Object::Prize->new(Game::Location->new($prizeLeft, $prizeTop), 100), "new prize");
ok($oLawn->placeObjectAt($oPrize), "placed prize ok");





is($oWorm->turn("left"), "down", "turned left");
ok($oWorm->moveForward(), "moved forward");

is($oWorm->turn("left"), "right", "turned left");
ok($oWorm->moveForward(), "moved forward");

is($oWorm->turn("left"), "up", "turned left");
ok(!$oWorm->moveForward(), "could not move forward");

is($oWorm->turn("right"), "right", "turned right");
ok($oWorm->moveForward(), "moved forward");
ok($oWorm->moveForward(), "moved forward");


is($oWorm->score, 0, "score 0");
ok($oLawn->isObjectAt($oPrize, $prizeLeft, $prizeTop), "Prize is there");

ok($oWorm->moveForward(), "moved forward onto Prize");

is($oWorm->score, 100, "score 100");
ok(!$oLawn->isObjectAt($oPrize, $prizeLeft, $prizeTop), "Prize is not there");



my ($wallLeft, $wallTop) = ($oWorm->oLocation->left + 2, $oWorm->oLocation->top);
ok(my $oWall = Game::Object::Wall->new(Game::Location->new($wallLeft, $wallTop), "horizontal", 4), "new wall");
ok($oLawn->placeObjectAt($oWall), "placed wall ok");


ok($oWorm->oValidLocationAfterMove(), "would be ok to move");
ok( $oWorm->moveForward(), "moved forward");

ok(!$oWorm->oValidLocationAfterMove(), "would not be ok to move");
ok(!$oWorm->moveForward(), "couldn't moved forward into wall");

is($oWorm->oEventMove->timeInterval(0), 0, "time interval set to 0");

eval { $oWorm->checkTick(0); };	#time 0 is waay back 
like($@, qr/Could not move forward/, "died ok");
ok(UNIVERSAL::isa($@, "ExceptionCouldNotMoveForward"), "is correct exception");
is($@->oObject, $oWorm, "The object is the worm");




ok($oWorm->crash(), "the worm crashed");






1;
__END__