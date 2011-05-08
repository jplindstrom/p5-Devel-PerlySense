#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";



use_ok("Game::Location");


ok(my $oLoc = Game::Location->new(), "new, default params");
is($oLoc->left, 0, " left ok");
is($oLoc->top, 0, " top ok");


ok($oLoc = Game::Location->new(10, 10), "new, specific params");
is($oLoc->left, 10, " left ok");
is($oLoc->top, 10, " top ok");


ok(my $oLoc2 = $oLoc->oClone(), "cloned");
is($oLoc->left, $oLoc2->left, " same left");
is($oLoc->top, $oLoc2->top, " same top");
isnt($oLoc, $oLoc2, " not the same object");





1;
__END__