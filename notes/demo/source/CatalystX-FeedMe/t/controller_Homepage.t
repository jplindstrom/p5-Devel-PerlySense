use strict;
use warnings;
use Test::More tests => 3;

BEGIN { use_ok 'Catalyst::Test', 'CatalystX::FeedMe' }
BEGIN { use_ok 'CatalystX::FeedMe::Controller::Homepage' }

ok( request('/homepage')->is_success, 'Request should succeed' );


