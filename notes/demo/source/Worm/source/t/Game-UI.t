#!perl -w
use strict;

use lib "../lib";

use Test::More "no_plan";
use Time::HiRes qw( time sleep );

use Data::Dumper;



use_ok("Game::Lawn");
use_ok("Game::UI");


my ($width, $height) = (76, 22);

ok(my $oLawn = Game::Lawn->new($width, $height), "new Lawn");


ok(my $oUI = Game::UI->new($oLawn), "new Display");
is($oUI->offsetLeft(2), 2, "offsetLeft");
is($oUI->offsetTop(1), 1, "offsetTop");
ok($oUI->oLocationScore( Game::Location->new(65, 0) ), "location score");
ok($oLawn->oUI($oUI), "ui set to lawn");
is($oUI->initScreen(), 1, "init");


my ($left, $top) = (11, 14);
ok(my $oWorm = $oLawn->oPlaceWorm($left, $top), "placed worm ok");

sleeep(0.25);
ok($oWorm->moveForward(), "moved forward");

sleeep(0.25);
ok($oWorm->moveForward(), "moved forward");

is($oWorm->turn("right"), "up", "turned right->up");

sleeep(0.25);
ok($oWorm->moveForward(), "moved forward");

sleeep(0.25);
ok($oWorm->moveForward(), "moved forward");

is($oWorm->turn("left"), "left", "turned left");
ok($oWorm->moveForward(), "moved forward");
is($oWorm->turn("left"), "down", "turned left");

do { sleeep(0.05); $oWorm->moveForward(); } for(1..8);

sleeep(0.25);
ok($oWorm->moveForward(), "moved forward");

sleeep(0.25);
ok(!$oWorm->moveForward(), "failed to move forward out of screen");


is($oWorm->turn("left"), "right", "turned left, facing right");





#Timed movement

my $timeInterval = 0.01;
$oWorm->oEventMove->timeInterval($timeInterval);

my $noMove = 0;
my $timeStart = time();
eval {
	for (1..1000000) {

		$oWorm->checkTick(time()) and $noMove++;		
		
		sleeep(0.005);
		}
	};
my $timeDuration = (time() - $timeStart) * 10;
is($noMove, 67, "moved correct number of steps");
my $timeExpected = ($timeInterval * $noMove) * 10;
my $timeDiff = int($timeDuration - $timeExpected);
is($timeDiff, 0, "took the correct time");


#print Dumper($oLawn);


ok(my $oPrize = $oLawn->oPlacePrize(Game::Location->new(41, 19), 150), "placed prize ok");
ok(! $oLawn->oPlacePrize(Game::Location->new($width - 1, $height - 1), 100), "failed place prize on worm");




is($oUI->soundsEnabled, 0, "soundsEnabled off");
ok($oUI->playSound("missing_file_JDSLKFJD"), "Stayed silent for missing file");
is($oUI->soundsEnabled(1), 1, "soundsEnabled off");
ok(!$oUI->playSound("missing_file_JDSLKFJD"), "Failed for missing file");



ok($oUI->wormHasCrashed($oWorm), "wormHasCrashed");		#Meaningless test...



is($oLawn->isObjectAt($oWorm, $oWorm->oLocation->left, $oWorm->oLocation->top), 1, "worm at location");
ok($oLawn->removeObject($oWorm), "removed worm");
is($oLawn->isObjectAt($oWorm, $oWorm->oLocation->left, $oWorm->oLocation->top), 0, "worm at location");




sub sleeep {
	select(undef, undef, undef, $_[0] || 0.25);
	}





1;
__END__