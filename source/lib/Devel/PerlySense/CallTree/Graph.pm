=head1 NAME

Devel::PerlySense::CallTree::Graph - A GraphViz graph of the CallTree

=head1 DESCRIPTION


=cut

package Devel::PerlySense::CallTree::Graph;

use strict;
use warnings;
use utf8;



use Moo;
use Path::Tiny;

use Devel::PerlySense::CallTree;



has call_tree => ( is => "ro", required => 1 );

has output_format => ( is => "lazy" );
sub _build_output_format { "png" }

has output_dir => ( is => "lazy" );
sub _build_output_dir { "." }

has base_file => ( is => "lazy" );
sub _build_base_file {
    return "temp";
    time() . "",
}

has dot_file => ( is => "lazy" );
sub _build_dot_file {
    my $self = shift;
    my $file = path($self->output_dir, $self->base_file . ".dot")->absolute;
    return $file;
}

has output_file => ( is => "lazy" );
sub _build_output_file {
    my $self = shift;
    path($self->output_dir, $self->base_file . "." . $self->output_format)->absolute;
}



sub create_graph {
    my $self = shift;
    my $dot_file = $self->dot_file;
    $self->write_dot_file( $dot_file );
    $self->run_dot( $dot_file, $self->output_file );
}

sub write_dot_file {
    my $self = shift;
    my ($filename) = @_;

    my $node_declarations = "abc; def;";
    my $edge_declarations = "abc -> def";
    my $source = qq|
digraph d {
    overlap  = false
    ranksep  = 0.5; nodesep = 0.1;
    rankdir  = TB;
    fontname = "Verdana";
    labelloc = "b";

    graph[ style = invis ];

    node [
        width    = 0.1,
        height   = 0.4,
        fontname = "Verdana",
        fontsize = 8,
        shape    = "none",
    ];
    edge [
        arrowsize = 0.5,
        fontname  = "Helvetica",
        fontsize  = 9,
    ];


    $node_declarations


    $edge_declarations

}

|;
    path($filename)->spew($source);
}

sub run_dot {
    my $self = shift;
    my ($dot_file, $output_file) = @_;
    my $format = $self->output_format;
    system("dot -T$format -o$output_file $dot_file");
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
