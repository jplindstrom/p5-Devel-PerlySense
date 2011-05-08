package CatalystX::FeedMe::Model::Db;

use strict;
use base 'Catalyst::Model::DBIC::Schema';

__PACKAGE__->config(
    schema_class => 'CatalystX::FeedMe::DBIC',
    connect_info => [
        CatalystX::FeedMe->config->{app}{db}{dsn},
        CatalystX::FeedMe->config->{app}{db}{user},
        CatalystX::FeedMe->config->{app}{db}{password},
        { AutoCommit => 1 },
    ],
);

=head1 NAME

CatalystX::FeedMe::Model::Db - Catalyst DBIC Schema Model
=head1 SYNOPSIS

See L<CatalystX::FeedMe>

=head1 DESCRIPTION

L<Catalyst::Model::DBIC::Schema> Model using schema L<CatalystX::FeedMe::DBIC>

=head1 AUTHOR

A clever guy

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
