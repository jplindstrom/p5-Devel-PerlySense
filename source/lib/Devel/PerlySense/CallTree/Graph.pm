=head1 NAME

Devel::PerlySense::CallTree::Graph - A GraphViz graph of the CallTree

=head1 DESCRIPTION


=cut

package Devel::PerlySense::CallTree::Graph;

use strict;
use warnings;
use utf8;



use Moo;



has call_tree => ( is => "ro", required => 1 );

has output_format => ( is => "lazy" );
sub _build_normal_line { "png" }

has output_dir => ( is => "lazy" );
sub _build_indentation { "." }


sub write_graph {
    my $self = shift;

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
