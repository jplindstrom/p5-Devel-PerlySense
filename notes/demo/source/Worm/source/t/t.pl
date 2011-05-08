#!perl -w
use Test::Harness;
runtests(glob( shift || "*.t" ));
1;
__END__