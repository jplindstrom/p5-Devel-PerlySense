#!/usr/bin/perl -w
use strict;

use Test::More;
use Test::Exception;

use File::Basename;
use File::Spec::Functions;

use lib "../lib";

use_ok("Devel::PerlySense");
use_ok("Devel::PerlySense::Document::Location");


BEGIN { -d "t" and chdir("t"); }


ok(my $oPs = Devel::PerlySense->new(), "new ok");



my $dirData = "data/simple-lib";
my $fileOrigin = "$dirData/lib/Win32/Word/Writer.pm";
my $oLocation;




ok($oLocation = $oPs->oLocationSmartGoTo(
    file => $fileOrigin,
    row  => 423,
    col  => 21,
), "Found source ok, on method");
is($oLocation->file, $fileOrigin, " file name");
is($oLocation->row, 446, " row ok");
is($oLocation->col, 1, " col ok");


ok($oLocation = $oPs->oLocationSmartGoTo(
    file => $fileOrigin,
    row  => 429,
    col  => 14,
), "Found source ok, on method");
is($oLocation->file, $fileOrigin, " file name");
is($oLocation->row, 396, " row ok");
is($oLocation->col, 1, " col ok");





done_testing();

__END__
