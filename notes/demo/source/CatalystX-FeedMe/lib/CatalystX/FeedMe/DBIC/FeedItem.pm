package CatalystX::FeedMe::DBIC::FeedItem;

=head1 NAME

FeedItem - A model object representing an Item in a Feed.

=head1 DESCRIPTION

=cut



use base qw/DBIx::Class/;

use URI::URL ();  #Don't import "url", it clashes with the model "url"



__PACKAGE__->load_components(qw/ PK::Auto Core HTMLWidget /);
__PACKAGE__->table('feed_item');
__PACKAGE__->add_columns(
    qw/
       feed_item_id
       feed_id
       title
       body
       url
       create_time
       /
   );
__PACKAGE__->set_primary_key(qw/feed_item_id/);

# has_many():
#   args:
# 1) Name of relationship, DBIC will create accessor with this name
# 2) Name of the model class referenced by this relationship
# 3) Column name in *foreign* table
__PACKAGE__->belongs_to(feed => 'CatalystX::FeedMe::DBIC::Feed', 'feed_id');



sub favicon_url_for_url {
    my ($self) = @_;
    $self->url or return "";

    my $url = URI::URL->new( $self->url );

    return $url->scheme . "://" . $url->netloc . "/favicon.ico";
}




1;
