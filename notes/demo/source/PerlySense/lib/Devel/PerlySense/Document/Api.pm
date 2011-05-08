=head1 NAME

Devel::PerlySense::Document::Api - The methods (and their locations)
of a package

=head1 DESCRIPTION

An API is the methods/subs a module/package supports (or in some cases
_may_ support).

=cut





use strict;
use warnings;

package Devel::PerlySense::Document::Api;
our $VERSION = '0.01';





use Spiffy -Base;
use Carp;
use Data::Dumper;

use Devel::PerlySense::Document::Location;





=head1 PROPERTIES

=head2 rhSub

Hash ref with (keys: method/sub name; values: Document::Location objects).

Default: {}

The Location objects have a C<sub> property which is the name of the
sub.

=cut
field "rhSub" => {};



=head1 API METHODS

=head2 new()

Create new Api object.

=cut
sub new(@) {
    my $pkg = shift;
    my (%p) = @_;

    my $self = bless {}, $pkg;

    return($self);
}





=head2 parsePackageSetSub(raNodeSub => $raNodeSub, source => $source, oDocument => $oDocument)

Parse the entire package data, both $source and found method
nodes. Add both found subs and $raNodeSub to the rhSub property.

Return 1 or die on errors.

=cut
sub parsePackageSetSub {
    my ($raNodeSub, $source, $oDocument) = Devel::PerlySense::Util::aNamedArg(["raNodeSub", "source", "oDocument"], @_);

    #Temporal cohesion: let the sub declarations overwrite the called subs
      #TODO: The called subs shouldn't overwrite sub declarations of a base class
    $self->parseSourceSetSub(source => $source, oDocument => $oDocument);

    for my $oNodeSub (@$raNodeSub) {
        $self->oLocationSetSub(nameSub => $oNodeSub->name, oDocument => $oDocument, oNode => $oNodeSub);
    }

    return(1);
}





=head2 parseSourceSetSub(source => $source, oDocument => $oDocument)

Parse the $source, looking for $self->method calls, and
$self->{hash_key}, and add them to the rhSub property.

Return 1 or die on errors.

=cut
sub parseSourceSetSub {
    my ($source, $oDocument) = Devel::PerlySense::Util::aNamedArg(["source", "oDocument"], @_);

    #Look for $self->method calls
    my @aSelfMethod = $source =~ / \$self \s* -> \s* (\w+) /gsx;

    #Look for $self->{property_name}
    my @aSelfHash =
            #Remove quotes
            map { s/ ^ (["'])  ( [^\1]* )   \1 $ /$2/x; $_ }  ## no critic
            $source =~
            /
             \$self \s* -> \s* {
             (
               (?:  " [^"\ ]+ "  )
               |
               (?:  ' [^'\ ]+ '  )
               |
               (?:    \w+      )
             )
             /gsx;

    my %hSeen;
    for my $method ( grep { ! $hSeen{$_} ++ }  @aSelfMethod, @aSelfHash ) {
        $self->oLocationSetSub(nameSub => $method, oDocument => $oDocument);
    }

    return(1);
}





=head2 oLocationSetSub(nameSub => $nameSub, oDocument => $oDocument, [oNode => $oNode])

Set the $self->rhSub->{$nameSub} to a new Document::Location with
$oDocument and possibly a row/col for $oNode. Set the rhProperty for:

  sub

If no $oNode is passed, the location is supposed to be unknown, with
row/col: 0/0.

Return the new oLocation. Die on errors.

=cut
sub oLocationSetSub {
    my ($nameSub, $oDocument) = Devel::PerlySense::Util::aNamedArg(["nameSub", "oDocument"], @_);
    my %p = @_;  my ($oNode) = ($p{oNode});

    my $oLocation;

    if($oNode) {
        $oLocation = $oDocument->oLocationOfNode($oNode);
    } else {
        $oLocation = Devel::PerlySense::Document::Location->new(
            file => $oDocument->file,
        );
    }

    $oLocation->rhProperty->{sub} = $nameSub;
    $self->rhSub->{$nameSub} = $oLocation;

    return($oLocation);
}





=head2 mergeWithBase($oApiBase)

Adjust this object by adding appropriate parts of $oApiBase, i.e. the
methods in $oApiBase->rhSub that aren't overridden in this class.

If a method has no row/col in neither base or self, it's supposed to
be defined in the base class. Any method definition with row/col in
self overrides one in base.

Return 1 on success. Die on errors.

=cut
sub mergeWithBase {
    my ($oApiBase) = @_;

    my $rhSub = $self->rhSub;
    while(my ($method, $oLocationBase) = each %{$oApiBase->rhSub}) {

        if(my $oLocation = $rhSub->{$method}) {

            #If both are just seen as $self->X, go with the base one
            if($oLocation->row == 0 && $oLocationBase->row == 0) {   #TODO: refactor: ! hasPosition
                $rhSub->{$method} = $oLocationBase;
            }

            #If the base one is a real declaration and self is only seen as $self->X, go with the base one
            if($oLocationBase->row != 0 && $oLocation->row == 0) {
                $rhSub->{$method} = $oLocationBase;
            }

        } else {
            #Not present in self, copy from base
            $rhSub->{$method} = $oLocationBase;
        }
    }

    return(1);
}





=head2 isSubSupported($nameSub)

Return true if $nameSub is supported by this API, else false.

=cut
sub isSubSupported {
    my ($nameSub) = @_;
    return( exists $self->rhSub->{$nameSub} );
}





=head2 percentSupportedOf($raNameSub)

Return percent (0..100) of how many of the sub names in raNameSub that
are present in the api.

=cut
sub percentSupportedOf {
    my ($raNameSub) = @_;

    my $countSupported = grep { $self->isSubSupported($_) } @$raNameSub;
    my $percent = $countSupported / (scalar(@$raNameSub) || 1);

    return($percent * 100);
}





=head2 percentConsistsOf($raNameSub)

Return percent (0..100) of how much of the api consists of the sub
names in raNameSub.

I.e. a large API will have a low percentage. Extra sub names in
$raNameSub will not affect the percentage.

=cut
sub percentConsistsOf {
    my ($raNameSub) = @_;

    my %hNameSub = map { $_ => 1 } @$raNameSub;
    my $countConsists = grep { $hNameSub{$_} } keys %{$self->rhSub};
    my $percent = $countConsists / (scalar(keys %{$self->rhSub}) || 1);

    return($percent * 100);
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
