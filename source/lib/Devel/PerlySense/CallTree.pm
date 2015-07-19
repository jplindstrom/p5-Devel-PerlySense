=head1 NAME

Devel::PerlySense::CallTree - A tree of method calls

=head1 DESCRIPTION


=cut

package Devel::PerlySense::CallTree;

use strict;
use warnings;
use utf8;

use Moo;
use Path::Tiny;

use Devel::PerlySense::CallTree::Caller;



=head1 PROPERTIES

=head2 source

The call-tree source with callers indented above their targets.

=cut
has source => ( is => "ro", required => 1 );

=head2 callers

Arrayref with Caller objects from the ->source

=cut
has callers => ( is => "lazy" );
sub _build_callers {
    my $self = shift;
    return [
        grep { defined $_->id }
        map { Devel::PerlySense::CallTree::Caller->new({ line => $_ }) }
        reverse split("\n", $self->source)
    ];
}


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
