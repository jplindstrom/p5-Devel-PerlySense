#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";



use_ok("Game::Direction");
use_ok("Game::Location");


ok(my $oDirection = Game::Direction->new("left"), "new ok");

is($oDirection->direction, "left", "direction");


my $oLoc = Game::Location->new(23, 43);
my $oLocNew = $oDirection->oMove($oLoc);
is($oLocNew->left, 22, "move left: left");
is($oLocNew->top, 43, "move left: top");

$oLoc = $oLocNew;
is($oDirection->direction("up"), "up", "direction");
$oLocNew = $oDirection->oMove($oLoc);
is($oLocNew->left, 22, "move left: left");
is($oLocNew->top, 42, "move left: top");


is($oDirection->direction("down"), "down", "direction");
is($oDirection->turn("right"), "left", "turn right");
is($oDirection->turn("right"), "up", "turn right");
is($oDirection->turn("right"), "right", "turn right");
is($oDirection->turn("right"), "down", "turn right");
is($oDirection->turn("right"), "left", "turn right");
is($oDirection->turn("left"), "down", "turn left");
is($oDirection->turn("left"), "right", "turn left");
is($oDirection->turn("left"), "up", "turn left");
is($oDirection->turn("left"), "left", "turn left");
is($oDirection->turn("left"), "down", "turn left");


#difference

ok(my $oDir1 = Game::Direction->new("up"), "new");
ok(my $oDir2 = Game::Direction->new("up"), "new");

is($oDir1->turnDifference($oDir2), "", "same dir, no turn");

is($oDir2->turn("right"), "right", "turned right");
is($oDir1->turnDifference($oDir2), "right", "diff left");

is($oDir2->turn("right"), "down", "turned to down");
is($oDir1->turnDifference($oDir2), "", "impossible diff, 180o");

is($oDir2->turn("right"), "left", "turned to left");
is($oDir1->turnDifference($oDir2), "left", "impossible diff, 180o");


$oDir1->direction("left");
$oDir2->direction("up");
is($oDir1->turnDifference($oDir2), "right", "left, up");
is($oDir2->turnDifference($oDir1), "left", "left, up");

$oDir1->direction("left");
$oDir2->direction("down");
is($oDir1->turnDifference($oDir2), "left", "left, down");
is($oDir2->turnDifference($oDir1), "right", "left, down");

$oDir1->direction("up");
$oDir2->direction("down");
is($oDir1->turnDifference($oDir2), "", "up, down");
is($oDir2->turnDifference($oDir1), "", "up, down");

$oDir1->direction("up");
$oDir2->direction("right");
is($oDir1->turnDifference($oDir2), "right", "up, right");
is($oDir2->turnDifference($oDir1), "left", "up, right");





1;
__END__