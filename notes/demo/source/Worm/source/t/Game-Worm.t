#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";



use_ok("Game::Object::Worm");


ok(my $oWorm = Game::Object::Worm->new(11, 12, "left", 3), "new ok");

is($oWorm->isRealPlayer, 1, "isRealPlayer");
is($oWorm->lengthIdeal, 3, "lengthIdeal");
is($oWorm->lengthActual, 3, "lengthActual");

is($oWorm->oLocation->left, 11, "left");
is($oWorm->oLocation->top, 12, "top");

is($oWorm->oDirection->direction, "left", "direction");

ok($oWorm->moveForward(), "move one step");
is($oWorm->oLocation->left, 10, "left");
is($oWorm->oLocation->top, 12, "top");

ok($oWorm->turn("right"), "turn right");

ok($oWorm->moveForward(), "move one step");
is($oWorm->oLocation->left, 10, "left");
is($oWorm->oLocation->top, 11, "top");

is($oWorm->isBlocking, 1, "isn't blocking");


is($oWorm->score, 0, "score");

ok($oWorm->awardScorePoints(10), "awardPrize");
is($oWorm->score, 10, "score");

ok($oWorm->awardScorePoints(7), "awardPrize");
is($oWorm->score, 17, "score");



ok($oWorm->moveForward(), "move one step");

ok($oWorm->grow(), "worm grow");
is($oWorm->lengthIdeal, 4, "ideal length");
is($oWorm->lengthActual, 3, "actual length");
ok($oWorm->moveForward(), "move one step");
is($oWorm->lengthIdeal, 4, "ideal length");
is($oWorm->lengthActual, 4, "actual length");

is($oWorm->lengthIdeal(3), 3, "ideal length");
ok($oWorm->moveForward(), "move one step");
is($oWorm->lengthIdeal, 3, "ideal length");
is($oWorm->lengthActual, 4, "actual length");





1;
__END__