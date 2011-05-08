package CatalystX::FeedMe::DBIC::Feed;

=head1 NAME

Feed - A model object representing a Feed.

=head1 DESCRIPTION

=cut



use base qw/DBIx::Class/;  

__PACKAGE__->load_components(qw/ PK::Auto Core HTMLWidget /);
__PACKAGE__->table('feed');
__PACKAGE__->add_columns(
    qw/
       feed_id
       name
       create_time
       /
   );
__PACKAGE__->set_primary_key(qw/feed_id/);

# has_many():
#   args:
# 1) Name of relationship, DBIC will create accessor with this name
# 2) Name of the model class referenced by this relationship
# 3) Column name in *foreign* table
__PACKAGE__->has_many(feed_items => 'CatalystX::FeedMe::DBIC::FeedItem', 'feed_id');




1;
