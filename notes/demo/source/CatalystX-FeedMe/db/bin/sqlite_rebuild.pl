#!perl.exe -w
use strict;


use File::Basename;
use Path::Class;

my $root_dir = file(dirname($0))->absolute;


my $fileDb = file($root_dir, "../sqlite/feedme.db");
my $fileSqlite = file($root_dir, "..\\sqlite\\create.sql");

unlink($fileDb); -f and die("Could not delete ($fileDb)\n");
system(qq{dbish dbi:SQLite:dbname=$fileDb <$fileSqlite});



__END__
