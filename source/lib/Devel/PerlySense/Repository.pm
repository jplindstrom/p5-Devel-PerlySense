
use strict;
use warnings;
package Devel::PerlySense::Repository;
use Moose;
use Method::Signatures;

use Devel::PerlySense::Util::Log;

has oPerlySense => (is => "ro", isa => "Devel::PerlySense");

sub BUILD {
    my $self = shift;
    my $file  = $self->oPerlySense->oHome->dirHomeRepository . "/proto.db";

    my $create = q{
        sub {
            my $dbh = shift;
            $dbh->do('
            CREATE TABLE method (
                id            INTEGER PRIMARY KEY,
                name          TEXT NOT NULL,
                package       TEXT NOT NULL,
                documentation TEXT NOT NULL,
                file          TEXT NOT NULL
            )');
        },
    };
    ## no critic
    eval qq{
        use ORLite( {
            package => "Devel::PerlySense::Repository::DB",
            file    => "$file",
            create  => $create
        } );
        1;
    } or die;
}

method index( :$oDocument ) {
    my @aNamePackage = $oDocument->aNamePackage;
    $oDocument->determineLikelyApi(nameModule => $aNamePackage[0]); ###JPL: loop

    Devel::PerlySense::Repository::DB->begin();
    Devel::PerlySense::Repository::DB::Method->delete(
        "where file = ?",
        $oDocument->file,
    );
    for my $package (keys %{$oDocument->rhPackageApiLikely}) {
        my $oApi = $oDocument->rhPackageApiLikely->{$package};
        for my $name (keys %{$oApi->rhSub}) {
            my $oLocation = $oApi->rhSub->{$name};
            Devel::PerlySense::Repository::DB::Method->create(
                name          => $name,
                package       => $package,
                documentation => "", # $oDocument->textDocumentation($name) || "",
                file          => $oDocument->file,
            );
        }
    }

    Devel::PerlySense::Repository::DB->commit();
}

method raMethodByClass($nameClass) {
    return [
        sort {
               $a->{package}     cmp $b->{package}
            || $a->{method_name} cmp $b->{method_name}
        }
        map {
            +{
                method_name   => $_->name,
                package       => $_->package,
                documentation => $_->documentation,
            };
        }
        Devel::PerlySense::Repository::DB::Method->select(
            "where package = ?",
            $nameClass,
        ),
    ];
}

1;
