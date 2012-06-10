
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

method raResponseFromResultset($raRow) {
    ###JPL: if the class isn't known, order them so any likely classes
    ###are displayed earlier. e..g package name mentioned in the
    ###vincinity.
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
        @$raRow
    ];
}

method raMethodByClass($nameClass) {
    return $self->raResponseFromResultset([
        Devel::PerlySense::Repository::DB::Method->select(
            "where api_package = ?",
            $nameClass,
        ),
    ]);
}

method raMethodByNames($raNameMethod, $nameMethodStartsWith) {

    use Data::Dumper; warn "JPL: " . Dumper([$raNameMethod, $nameMethodStartsWith]);
    my $nameMethodCount = scalar @$raNameMethod;
    my $nameMethodPlaceholders = join(", ", map { "?" } 1..$nameMethodCount);

    my @raRow = Devel::PerlySense::Repository::DB::Method->select(
        # Can't use placeholder for $nameMethodCount for some reason,
        # it' safe to interpolate anyway
        qq| where api_package in (
                select api_package from (
                    select api_package, count(*) from method where
                        name in ($nameMethodPlaceholders)
                    group by api_package having count(*) >= $nameMethodCount
                ) as packages
            )
            |,
        @$raNameMethod,
    );

    ###JPL: Filter out classes that doesn't have $nameMethodStartsWith

    return $self->raResponseFromResultset(\@raRow);
}

1;

__END__

Foo abc
Foo def
Foo ghi
Bar abc
Bar def
Baz def

select api_package, name from method where api_package in (
    select api_package from (
        select api_package, count(*) from method where
            name in (
                "fileHtml",
                "oNotes"
            )
        group by api_package having count(*) >= 2
    ) as packages
);


