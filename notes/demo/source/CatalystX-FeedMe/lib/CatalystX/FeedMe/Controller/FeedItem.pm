package CatalystX::FeedMe::Controller::FeedItem;

use strict;
use warnings;
use base 'Catalyst::Controller';

use Data::Dumper;
use LWP::Simple qw/ get /;
use WWW::Mechanize;



=head1 NAME

CatalystX::FeedMe::Controller::FeedItem - Controller for a Feed Item

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut



sub index : Private {
    my ($self, $c) = @_;
    $c->forward("list");
}



sub view : Local {
    my ($self, $c, $feed_item_id) = @_;

    #Todo: validate ownership
    my $feed_item = $c->stash->{feed_item} = $c->model("Db::FeedItem")->find($feed_item_id) or die("Unknown Feed Item ($feed_item_id)\n");
    my $feed = $c->stash->{feed} = $c->model("Db::Feed")->find($feed_item->feed_id) or die("Unknown Feed (" . $feed_item->feed_id . ")\n");

    $c->stash(template => "feeditem/view.tt");
}



sub create_text : Local {
    my ($self, $c) = @_;

    my $feed = $c->controller("Feed")->feed_from_name($c, $c->req->param("feed_name"));
    $c->req->param("feed_id", $feed->feed_id);

    #Special case: User typed Firefox shortcut without any text at
    #all. Don't add that text.
    $c->req->param("body") eq "%s" and return $c->response->redirect($c->uri_for("/feed/list", $feed->name));

    my $widget = $self->create_text_widget(
        $c,
        [ "/feeditem/create_text", $feed->name ],
        $feed,
    );
    my $widget_result = $c->stash->{feed_item_text_widget} = $widget->process($c->req);
    $widget_result->param("title") || $widget_result->param("url") || $widget_result->param("body") or $widget_result->add_error({
        name => "title",
        message => "Please enter either of a Title, URL or Text",
    });

    if($widget_result->has_errors) {
        $c->stash(error_msg => 'Validation errors!');
        return $c->forward("/feed/list", $feed->name);
    }

    my $feed_item = $c->model('Db::FeedItem')->new({});
    $feed_item->populate_from_widget($widget_result);

    #Default title to the first part of the body
    #TODO: move to model
    if(!$feed_item->title) {
        my $title = $feed_item->body || $self->title_of_url($feed_item->url);
        $title =~ s/ (.{1,40}) .* /$1.../xms;
        $feed_item->title($title);
        $feed_item->update;
    }

    return $c->response->redirect($c->uri_for("/feed/list", $feed->name));
}



sub create_text_widget {
    my ($self, $c, $uri_for, $feed) = @_;
    my @uri_for = ref $uri_for ? @$uri_for : $uri_for;

    my $w = $c->widget('text_widget')->method('post');

    $w->element('Textfield', 'title'  )->label('Title')->size(255);
    $w->element('Textfield', 'url'  )->label('URL')->size(255);
    $w->element('Textarea', 'body' )->label('Text')->rows(8)->wrap('wrap');
    $w->element('Hidden', 'feed_name'  )->value($feed->name);

    $w->element('Submit',    'submit' )->value('Create Feed Item');


#    $w->constraint(All => qw/ body /)->message('Required. ');
    for my $column (qw/ title body url /) {
        $w->filter( HTMLEscape => $column );
        $w->filter( TrimEdges  => $column );
    }

    $w->action($c->uri_for(@uri_for));

    return $w;
}



sub create_web_page : Local {
    my ($self, $c) = @_;
$c->log->debug("web page 1");
    my $feed = $c->controller("Feed")->feed_from_name($c, $c->req->param("feed_name"));
$c->log->debug("web page 2");
    $c->req->param("feed_id", $feed->feed_id);

    my $widget = $self->create_web_page_widget(
        $c,
        [ "/feeditem/create_web_page", $feed->name ],
        $feed,
    );
    my $widget_result = $c->stash->{feed_item_web_page_widget} = $widget->process($c->req);
$c->log->debug("web page 3");

    if($widget_result->has_errors) {
        $c->stash(error_msg => 'Validation errors!');
        return $c->forward("/feed/list", $feed->name);
    }

$c->log->debug("web page 4");
    my ($body, $title) = $self->get_compressed_web_page_and_title(
        $c,
        $widget_result->param("url"),
    );
$c->log->debug("web page 5");
    my $feed_item = $c->model('Db::FeedItem')->create({
        url => $widget_result->param("url"),
        title => $title,
        body => $body,
        feed_id => $feed->feed_id,
    });

    return $c->response->redirect($c->uri_for("/feed/list", $feed->name));
}



sub create_web_page_widget {
    my ($self, $c, $uri_for, $feed) = @_;
    my @uri_for = ref $uri_for ? @$uri_for : $uri_for;

    my $w = $c->widget('web_page_widget')->method('post');

    $w->element('Textfield', 'url'  )->label('URL')->size(255);
    $w->element('Hidden', 'feed_name'  )->value($feed->name);

    $w->element('Submit',    'submit' )->value('Create Feed Item');


    $w->constraint(All => qw/ url /)->message('Required. ');
    for my $column (qw/url/) {
        $w->filter( HTMLEscape => $column );
        $w->filter( TrimEdges  => $column );
    }

    $w->action($c->uri_for(@uri_for));

    return $w;
}



sub delete : Local {
    my ($self, $c, $feed_item_id) = @_;

    my $feed_item = $c->model("Db::FeedItem")->find($feed_item_id) or die("Unknown Feed Item ($feed_item_id)\n");
    my $feed = $feed_item->feed();
    $feed_item->delete();

    return $c->response->redirect($c->uri_for("/feed/list", $feed->name));
}



sub title_of_url {
    my ($self, $url) = @_;

    get($url) =~ m| < \s* title \s* > (.+?) < \s* / \s* title \s*> |msx or return $url; #todo: use html parser

    return $1;
}



sub get_compressed_web_page_and_title  {
    my ($self, $c, $url) = @_;

    my $provider_conf = $c->config->{app}->{web_compression}->{mlvb} or die("Unknown provider MLVB\n");

    my $mech = WWW::Mechanize->new();
    eval {
        $c->log->debug("blvb: login");
        $mech->get($provider_conf->{login_form_url});
        $c->log->debug("blvb: login submit");
        $mech->submit_form(
            form_number => $provider_conf->{login_form_number},
            fields => $provider_conf->{login_form},
        );

        $c->log->debug("blvb: surf");
        $mech->get($provider_conf->{convert_form_url});
        $mech->field($provider_conf->{convert_form}->{url} => $url);
        $c->log->debug("blvb: surf submit");
        $mech->submit();
        $c->log->debug("blvb: done");
    };
    $@ and $c->log->error("Mechanize failure: $@"), die;

    my $content = $mech->content;
    $content =~ s|<img src="(/imgproxy.*?)"||gsm;

    return( $content, $mech->title || $url);
}



=head1 AUTHOR

A clever guy

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
