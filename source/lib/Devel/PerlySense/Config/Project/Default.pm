=head1 NAME

Devel::PerlySense::Config::Project::Default - A Project's default configuration


=head1 DESCRIPTION

This is the default config for a project, used whenever a proper
.PerlySenseProject with a project.yml couldn't be identified.

=cut





use strict;
use warnings;

package Devel::PerlySense::Config::Project::Default;
use base "Devel::PerlySense::Config::Project";
our $VERSION = '0.01';





use Data::Dumper;
use Carp;
use YAML::Tiny ();

use Devel::PerlySense;





=head1 METHODS

=head2 new()

Create new default Config object, with rhConfig set to the default.

Return the new object, or die, e.g. if the yaml contains syntax
errors.

=cut
sub new(@) {
    my $pkg = shift;

    my $self = $pkg->SUPER::new(@_);
    
    my $sourceConfig = $self->textConfigDefault;
    my ($rhConfig) = eval { YAML::Tiny::Load($sourceConfig) };
    $rhConfig or die($YAML::Tiny::errstr);

    $self->rhConfig($rhConfig);

    return $self;
}





1;





__END__

=head1 AUTHOR

Johan Lindström, C<< <johanl[ÄT]DarSerMan.com> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-devel-perlysense@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-PerlySense>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2005 Johan Lindström, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
