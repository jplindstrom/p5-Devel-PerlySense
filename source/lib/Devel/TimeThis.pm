=head1 NAME

Devel::TimeThis - Time the duration of a variable until it goes out of
scope


=head1 DESCRIPTION



=head1 SYNOPSIS



=cut





use strict;
use warnings;

package Devel::TimeThis;
our $VERSION = '0.01';





use Carp;
use Data::Dumper;
use Time::HiRes qw/time/;




=head1 PROPERTIES

=head2 timeStart


=cut






my $rhNameInfo = {};




=head1 API METHODS

=head2 new($name)

Create new TimeThis object.

Invocations with the same $name will be reported together.

=cut
sub new() {
    my $self = bless {}, shift;
    my ($name) = @_;

    $self->{timeStart} = time();
    $self->{name} = $name;

    return($self);
}





=head2 DESTROY

Collect the timing data

=cut
sub DESTROY {
	my ($self) = @_;

    my $timeDuration = time() - $self->{timeStart};

    $rhNameInfo->{$self->{name}}->{timeDurationAcc} += $timeDuration;
    $rhNameInfo->{$self->{name}}->{count}++;
    $rhNameInfo->{$self->{name}}->{name} = $self->{name};
}





=head2 END

Print timing data

=cut
sub END {
    keys %$rhNameInfo and print qq{

* Timing info *

};
    for my $rhInfo (
        sort { $b->{timeDurationAcc} <=> $a->{timeDurationAcc} }
                values %$rhNameInfo)
            {
        printf("% 40s: % 4d : %3.5f\n", $rhInfo->{name}, $rhInfo->{count}, $rhInfo->{timeDurationAcc});
    }
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


=head1 COPYRIGHT & LICENSE

Copyright 2005 Johan Lindström, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
