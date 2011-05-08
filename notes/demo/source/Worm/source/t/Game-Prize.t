#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";



use_ok("Game::Object::Prize");
use_ok("Game::Object::Worm");
use_ok("Game::Location");


my $value = 100;
ok(my $oPrize = Game::Object::Prize->new(Game::Location->new(11, 12), $value), "new ok");

is($oPrize->oLocation->left, 11, "left");
is($oPrize->oLocation->top, 12, "top");
is($oPrize->value, 100, "value");

is($oPrize->isBlocking, 0, "isn't blocking");


ok(my $oWorm = Game::Object::Worm->new(11, 12, "left", 3), "new ok");

is($oWorm->score, 0, "score 0");
ok($oPrize->wasCrashedIntoBy($oWorm), "worm crashed into prize");
is($oWorm->score, $value, "score $value");




1;
__END__