package CatalystX::FeedMe::Controller::Homepage;

use strict;
use warnings;
use base 'Catalyst::Controller';

=head1 NAME

CatalystX::FeedMe::Controller::Homepage - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index 

=cut

sub index : Private {
    my ( $self, $c ) = @_;
    $c->stash->{feeds} = [$c->model('Db::Feed')->all];    
    $c->stash->{template} = "frontpage.tt";
}




=head1 AUTHOR

A clever guy

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
