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


note("Happy path: Point on regular variable name, finds declaration");
ok($oLocation = $oPs->oLocationSmartGoTo(
    file => $fileOrigin,
    row  => 538,
    col  => 8,
), "Found source ok, on scalar");
is($oLocation->file, $fileOrigin, " file name");
is($oLocation->row, 535, " row ok");
is($oLocation->col, 5, " col ok");

note("Point on sigil of variable, finds declaration");
ok($oLocation = $oPs->oLocationSmartGoTo(
    file => $fileOrigin,
    row  => 538,
    col  => 5,
), "Found source ok, on sigil of scalar");
is($oLocation->file, $fileOrigin, " file name");
is($oLocation->row, 535, " row ok");
is($oLocation->col, 5, " col ok");

note("Point on declaration of variable");
ok($oLocation = $oPs->oLocationSmartGoTo(
    file => $fileOrigin,
    row  => 534,
    col  => 9,
), "Found source ok, on declaration");
is($oLocation->file, $fileOrigin, " file name");
is($oLocation->row, 534, " row ok");
is($oLocation->col, 5, " col ok");



note("Point on object with method call");
ok($oLocation = $oPs->oLocationSmartGoTo(
    file => $fileOrigin,
    row  => 514,
    col  => 6,
), "Found source ok, on object with method call");
is($oLocation->file, $fileOrigin, " file name");
is($oLocation->row, 512, " row ok");
is($oLocation->col, 5, " col ok");



note('Point on $abc[0] (not the index part) finds @abc');
{
    local $TODO = "Bug in EditorTools? Doesn't seem to identify that \$abc[123]";
    ok($oLocation = $oPs->oLocationSmartGoTo(
        file => $fileOrigin,
        row  => 536,
        col  => 19,
    ), "Found source ok, on hash with key lookup");
    is($oLocation->file, $fileOrigin, " file name");
    is($oLocation->row, 534, " row ok");
    is($oLocation->col, 5, " col ok");
}



done_testing();



__END__
