#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";



use_ok("Game::Object::Wall");
use_ok("Game::Object::Worm");
use_ok("Game::Location");


ok(my $oWall = Game::Object::Wall->new(Game::Location->new(4, 16), "horizontal", 20), "new ok");

is($oWall->oLocation->left, 4, "left");
is($oWall->oLocation->top, 16, "top");

is($oWall->isBlocking, 1, "is blocking");





1;
__END__