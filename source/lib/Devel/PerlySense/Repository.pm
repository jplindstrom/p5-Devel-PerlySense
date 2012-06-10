
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
                id                  INTEGER PRIMARY KEY,
                name                TEXT NOT NULL,
                api_package         TEXT NOT NULL,
                declaration_package TEXT NOT NULL,
                documentation       TEXT NOT NULL,
                file                TEXT NOT NULL
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
# debug("JPL: got thingy " . Dumper($oLocation)); use Data::Dumper;
            Devel::PerlySense::Repository::DB::Method->create(
                name                => $name,
                api_package         => $package,
                declaration_package => $oLocation->{package} || "",
                documentation       => "", # $oDocument->textDocumentation($name) || "",
                file                => $oDocument->file,
            );
        }
    }

    Devel::PerlySense::Repository::DB->commit();
}

method raMethodByClass($nameClass) {
    return [
        sort {
               $a->{api_package} cmp $b->{api_package}
            || $a->{method_name} cmp $b->{method_name}
        }
        map {
            +{
                method_name         => $_->name,
                api_package         => $_->api_package,
                declaration_package => $_->declaration_package,
                documentation       => $_->documentation,
            };
        }
        Devel::PerlySense::Repository::DB::Method->select(
            "where api_package = ?",
            $nameClass,
        ),
    ];
}

1;
