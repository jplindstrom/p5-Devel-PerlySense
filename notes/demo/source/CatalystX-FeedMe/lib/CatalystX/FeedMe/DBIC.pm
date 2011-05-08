
package CatalystX::FeedMe::DBIC;

=head1 NAME 

DBIC - DBIC Schema Class

=cut

use base qw/DBIx::Class::Schema/;

__PACKAGE__->load_classes(
    qw/
       Feed
       FeedItem
       /
   );



1;



__END__
