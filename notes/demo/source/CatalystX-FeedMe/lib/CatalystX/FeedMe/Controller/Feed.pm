package CatalystX::FeedMe::Controller::Feed;

use strict;
use warnings;
use base 'Catalyst::Controller';



use Data::Dumper;

use XML::Atom::Syndication::Feed;
use XML::Atom::Syndication::Entry;
use XML::Atom::Syndication::Content;
use XML::Atom::Syndication::Link;
use Template::Filters;



=head1 NAME

CatalystX::FeedMe::Controller::Feed - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut



sub index : Private {
    my ($self, $c) = @_;
    $c->forward("list");
}




sub list : Local {
    my ($self, $c, $name) = @_;
    my $feed = $self->feed_from_name($c, $name);

    $c->stash(feed => $feed);
    $c->stash(feed_items => $self->recent_feed_items($c, $feed));

    $c->stash->{feed_item_text_widget} ||= $c->controller("FeedItem")->create_text_widget(
        $c,
        [ "/feeditem/create_text" ],
        $feed,
    )->result();

    $c->stash->{feed_item_web_page_widget} ||= $c->controller("FeedItem")->create_web_page_widget(
        $c,
        [ "/feeditem/create_web_page" ],
        $feed,
    )->result();

    $c->stash(template => "feed/list.tt");
}





sub rss : Local {
    my ($self, $c, $name) = @_;
    my $feed = $self->feed_from_name($c, $name);

    my $atom_feed = XML::Atom::Syndication::Feed->new();
    $atom_feed->title('FeedMe: ' . $feed->name);
    $atom_feed->id('FeedMe:USER:' . $feed->name);  #todo: USER
    $self->add_atom_link($atom_feed, $c->uri_for("/feed/list/", $feed->name));

    my $item_max_count = 20;   #todo: move to feed config property
    my $feed_items = $self->recent_feed_items($c, $feed, $item_max_count);
    while (my $feed_item = $feed_items->next) {

        my $entry = XML::Atom::Syndication::Entry->new();
        $entry->id('FeedMe:USER:' . $feed->name . ":" . $feed_item->feed_item_id); #todo: USER

        $feed_item->title and $entry->title($feed_item->title);
        $feed_item->body and $entry->content(XML::Atom::Syndication::Content->new(
            Type => 'text/html',
            Body => Template::Filters::html_paragraph( $feed_item->body ),
        ));
        $self->add_atom_link($entry, $feed_item->url);

        $atom_feed->add_entry($entry);
    }
    $c->stash(xml_atom_object => $atom_feed);

    $c->forward('View::Atom');
}



sub feed_from_name {
    my ($self, $c, $name) = @_;
    $name ||= "default";
    my $feed = $c->model("Db::Feed")->search(name => $name)->first or die("Unknown Feed ($name)\n");
    return $feed;
}



#todo: move to the Feed model class
sub recent_feed_items {
    my ($self, $c, $feed, $limit) = @_;

    my $result = $c->model('Db::FeedItem')->search(
        { feed_id => $feed->feed_id },
        { order_by => "create_time desc" },
    );
    $limit and $result = $result->slice(0, $limit - 1);
    
    return $result;
}



sub add_atom_link {
    my ($self, $atom_object, $url) = @_;
    $url or return 0;

    $atom_object->link( my $link = XML::Atom::Syndication::Link->new() );
    $link->href($url);
    $link->rel("alternate");
    $link->type("text/html");

    return 1;
}



=head1 AUTHOR

A clever guy

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
