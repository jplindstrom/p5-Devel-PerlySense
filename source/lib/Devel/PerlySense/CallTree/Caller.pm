=head1 NAME

Devel::PerlySense::CallTree::Caller - A method call

=head1 DESCRIPTION


=cut

package Devel::PerlySense::CallTree::Caller;

use strict;
use warnings;
use utf8;



use Moo;


=head1 PROPERTIES

=head2 line

The source line describing this caller.

=cut
has line => ( is => "ro", required => 1 );

has normal_line => ( is => "lazy" );
sub _build_normal_line {
    my $self = shift;
    my $line = $self->line;
    $line =~ s/(\s*)#/$1 /;
    return $line;
}

has indentation => ( is => "lazy" );
sub _build_indentation {
    my $self = shift;
    $self->normal_line =~ / ^ (\s*) /x or return 0;
    return length( $1 );
}

has package => ( is => "lazy" );
sub _build_package {
    my $self = shift;
    $self->normal_line =~ /([\w:]+)->([\w]+)/ or return undef;
    return $1;
}

has method => ( is => "lazy" );
sub _build_method {
    my $self = shift;
    $self->normal_line =~ /([\w:]+)->([\w]+)/ or return undef;
    return $2;
}

has caller => ( is => "lazy" );
sub _build_caller {
    my $self = shift;
    my $package = $self->package or return undef;
    my $method = $self->method or return undef;
    return "$package->$method";
}

has id => ( is => "lazy" );
sub _build_id {
    my $self = shift;
    my $id = $self->caller or return undef;
    $id =~ s/::/_/g;
    $id =~ s/->/__/g;
    return lc( $id );
}

has called_by => ( is => "lazy" );
sub _build_called_by { [ ] }



1;




__END__

=encoding utf8

=head1 AUTHOR

Johan Lindstrom, C<< <johanl@cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-devel-perlysense@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-PerlySense>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2005 Johan Lindstrom, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
