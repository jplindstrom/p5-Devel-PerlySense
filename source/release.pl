#!/usr/bin/env perl -w
use strict;

use lib "lib";
use Devel::PerlySense;


main();

sub main {
    my $version = sprintf("%.4f", Devel::PerlySense->VERSION);

    -e "../../branches/release-$version" and
            die("The version ($version) seems to be released already\n");

    -f "Build" and system("perl Build realclean");
    sys("perl Build.PL");
    sys("perl Build distcheck");
    sys("perl Build manifest");
    sys("perl Build disttest");
    sys("perl Build dist");

    sys("perl Build install");

    my $release_tar_file = "Devel-PerlySense-$version.tar.gz";
    rename($release_tar_file, "../release/$release_tar_file");

    sys("perl Build realclean");

    say("Adding file");
    sys("svn add ../release/$release_tar_file");
    say("Committing file");
    sys(qq{svn commit -m "Tarball for release ($version)" ../release/$release_tar_file});

    chdir("../..");  # above trunk

    my ($svn_root) = grep { /URL:/ } `svn info`;
    chomp($svn_root);
    $svn_root =~ s/URL: //;

    my $release = "release-$version";
    my $release_branch = "branches/release-$version";

    sys(qq{svn cp -m "Branched for release ($version)" $svn_root/trunk $svn_root/$release_branch});
    sys(qq{svn co $svn_root/$release_branch $release_branch});

    say("Uploading to CPAN...");
    sys("cpan-upload-http trunk/release/$release_tar_file");
}



sub pause { <STDIN>; }

sub say { print @_, "\n"; }

sub sys {
    my ($command) = @_;

    say("SYS($command)");
    system($command) and die("Could not run ($command)\n");
}


__END__
