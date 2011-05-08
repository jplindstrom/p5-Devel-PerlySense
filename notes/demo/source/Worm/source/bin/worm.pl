#!perl -w
use strict;

use lib "../lib";



use Game::App;
use Game::Object::Worm;
use Game::Object::Worm::Bot;
use Game::Object::Prize;
use Game::Object::Wall;





print 
qq{

QUICK INTRO:

- You are the top worm

- F - Turn LEFT
- J - Turn RIGHT
- Q - QUIT

Please enlarge the console window, then press RETURN to start
};
<STDIN>;





my ($width, $height) = (74, 40);
my ($offsetLeft, $offsetTop) = (3, 1);


my $oApp = Game::App->new($offsetLeft, $offsetTop, $width, $height);
$oApp->oUI->soundsEnabled(1);


#Create worm
my ($wormLeft, $wormTop, $wormDirection, $wormLength) = (40, 10, "left", 15);
my $oWorm = Game::Object::Worm->new($wormLeft, $wormTop, $wormDirection, $wormLength);
$oApp->placeWormOnLawn($oWorm);


my $oWormBot = Game::Object::Worm::Bot->new($wormLeft, $wormTop + 8, $wormDirection, $wormLength);
$oWormBot->oEventMove->timeInterval(0.12);
$oWormBot->probabilityTurnRandomly(0.04);
$oWormBot->probabilityTurnTowardsPrize(1.00);
$oApp->placeWormBotOnLawn($oWormBot);


$oWormBot = Game::Object::Worm::Bot->new($wormLeft, $wormTop + 16, $wormDirection, $wormLength);
$oWormBot->probabilityTurnRandomly(0.07);
$oWormBot->probabilityTurnTowardsPrize(0.60);
$oApp->placeWormBotOnLawn($oWormBot);


$oWormBot = Game::Object::Worm::Bot->new($wormLeft, $wormTop + 24, $wormDirection, $wormLength);
$oWormBot->probabilityTurnTowardsPrize(0.80);
$oApp->placeWormBotOnLawn($oWormBot);

###for my $i (1..5) {
###	$oWormBot = Game::Object::Worm::Bot->new($wormLeft - $i, $wormTop  5 + ($i * 2), $wormDirection, $wormLength);
###	$oWormBot->probabilityTurnRandomly(0.07);
###	$oWormBot->probabilityTurnTowardsPrize(0.80);
####	$oWormBot->oEventMove->timeInterval(0.005);
###	$oApp->placeWormBotOnLawn($oWormBot);
###	}


my ($prizeLeft, $prizeTop, $value) = (11, 13, 100);
my $oPrize = Game::Object::Prize->new(Game::Location->new($prizeLeft, $prizeTop), $value);
$oApp->placePrizeOnLawn($oPrize);

$oPrize = Game::Object::Prize->new(Game::Location->new($prizeLeft + 40, $prizeTop + 2), $value);
$oApp->placePrizeOnLawn($oPrize);



my ($wallLeft, $wallTop) = (30, 5);
my $oWall = Game::Object::Wall->new(Game::Location->new($wallLeft, $wallTop), "horizontal", 30);
$oApp->placeWallOnLawn($oWall);

$oWall = Game::Object::Wall->new(Game::Location->new(12, 16), "horizontal", 30);
$oApp->placeWallOnLawn($oWall);

$oWall = Game::Object::Wall->new(Game::Location->new($wallLeft, $wallTop + 25), "horizontal", 30);
$oApp->placeWallOnLawn($oWall);




eval { $oApp->run(); };


sleep(2);



__END__