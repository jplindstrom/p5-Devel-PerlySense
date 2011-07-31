#!/usr/bin/perl -w
use strict;

use Test::More tests => 4;
use Test::Exception;

use File::Basename;
use File::Spec::Functions;

use lib "../lib";

use_ok("Devel::PerlySense");

BEGIN { -d "t" and chdir("t"); }


ok(my $oPs = Devel::PerlySense->new(), "new ok");


{
    local @INC = (@INC, "data/inc-lib");

    {
        my $dirData = "data/simple-lib/lib";
        my $dirOrigin = $dirData;
        my $nameModule = "Game::Event::Timed";
        my $fileModuleTarget = catfile("Game", "Event", "Timed.pm");

        like(
            $oPs->fileFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin),
            qr/ \Q$fileModuleTarget\E $/x,
            "Found file downwards before \@INC ok",
        );
    }


    {
        my $dirData = "data/inc-lib";
        my $dirOrigin = "/";
        my $nameModule = "Game::Event::Timed";
        my $fileModuleTarget = catfile($dirData, "Game", "Event", "Timed.pm");

        like(
            $oPs->fileFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin),
            qr/ \Q$fileModuleTarget\E $/x,
            "Found file in inc ok",
        );
    }
}







__END__
