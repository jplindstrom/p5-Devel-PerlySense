#!perl -w
use strict;

use lib "lib", "../lib";

use Test::More tests => 12;


use Time::HiRes qw( time sleep );


use_ok("Game::Event::Timed");


ok(my $oEvent = Game::Event::Timed->new(), "new");
is($oEvent->timeInterval, undef, "timeInterval");

is($oEvent->checkTick(time()), 0, "no tick");
is($oEvent->checkTick(time()), 0, "no tick");

is($oEvent->timeInterval(0), 0, "interval 0");

is($oEvent->checkTick( time() ), 1, "tick");
is($oEvent->checkTick( time() ), 1, "tick");

is($oEvent->timeInterval(0.5), 0.5, "interval 0.5");
is($oEvent->checkTick( time() ), 1, "tick");
is($oEvent->checkTick( time() ), 0, "tick");
sleep(1);
is($oEvent->checkTick( time() ), 1, "tick");



1;



__END__
