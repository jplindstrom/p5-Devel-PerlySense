#!perl.exe -w
use strict;

use File::Basename;
use Path::Class;

my $root_dir = file(dirname($0))->absolute;

my $fileDb = file($root_dir, "../sqlite/feedme.db");
system(qq{dbish.bat dbi:SQLite:dbname=$fileDb});

__END__
