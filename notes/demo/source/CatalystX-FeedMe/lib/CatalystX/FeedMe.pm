
use strict;
use warnings;

package CatalystX::FeedMe;
our $VERSION = '0.01';


use Catalyst::Runtime '5.70';

use Catalyst
        qw/
           -Debug
           ConfigLoader
           Static::Simple
           StackTrace

           Cache
           Cache::Store::Memory

           Authentication
           Authentication::Store::Minimal
           Authentication::Credential::HTTP

           Authentication::Credential::Atom
           AtomServer

           HTML::Widget
           /;

use XML::Atom::Feed;



__PACKAGE__->setup;




sub uri_for {
    my $c = shift;
    my $uri = $c->NEXT::uri_for(@_);

    if ( $uri->port == 55555 || $c->config->{'remove_port'} ) {
        $uri->port( $uri->default_port );       
    }
    
    $uri;
}


=head1 NAME

CatalystX::FeedMe - Catalyst based application

=head1 SYNOPSIS

    script/catalystx_feedme_server.pl

=head1 DESCRIPTION

[enter your description here]

=head1 SEE ALSO

L<CatalystX::FeedMe::Controller::Root>, L<Catalyst>

=head1 AUTHOR

Catalyst developer

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
