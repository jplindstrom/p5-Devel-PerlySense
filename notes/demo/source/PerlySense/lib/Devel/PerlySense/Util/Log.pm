=head1 NAME

Devel::PerlySense::Util::Log - Log routines

=cut



use strict;
use warnings;

package Devel::PerlySense::Util::Log;
use base "Exporter";

our @EXPORT = (
    qw/
       debug
       /);

our $VERSION = '0.01';





use Carp;
use Data::Dumper;
use File::Basename;
use Path::Class;

use Devel::PerlySense::Home;





=head1 ROUTINES

=head2 debug($message)

Log debug $message to a log file in the HOME log directory.

Return 1.

=cut
my $fileDebug;
sub debug {
	my ($message) = @_;

    $0 =~ /\.t$/ and return 1;  #Don't log when running tests

    $fileDebug ||= do {
        my $oHome = Devel::PerlySense::Home->new();
        $fileDebug = file($oHome->dirHomeLog, "debug.log");
        
    } or return 0;
    
    open(my $fh, ">>", $fileDebug) or return 0;
    $fh->print(localtime() . ": $message\n");
    
    return(1);
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
