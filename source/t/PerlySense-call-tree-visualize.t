#!/usr/bin/perl -w
use strict;

use Test::More tests => 4;
use Test::Exception;
use Data::Dumper;

use lib "../lib";

use_ok("Devel::PerlySense::CallTree");



my $source = "
#     Devel::PerlySense->oLocationSmartGoTo
#     Devel::PerlySense->oLocationSmartDoc
#     Devel::PerlySense->classByName
#         * Devel::PerlySense->oLocationSmartGoTo
#         * Devel::PerlySense->oLocationSmartDoc
#     Devel::PerlySense->aDocumentFindModuleWithInterface
#             Devel::PerlySense::Class->new
#         Devel::PerlySense::Class->findBaseClasses
#     Devel::PerlySense::Class->newFromName
#         * Devel::PerlySense->oLocationSmartGoTo
#             Devel::PerlySense::Class->oLocationMethodGoTo
#         Devel::PerlySense->oLocationMethodDefinitionFromDocument
#         * Devel::PerlySense::Document->oLocationSubDefinition
#     Devel::PerlySense::Document->oLocationSubDefinition
#             * Devel::PerlySense->oLocationSmartDoc
#             Devel::PerlySense::Class->oLocationMethodDoc
#             Devel::PerlySense::Document::Api::Method->new
#         Devel::PerlySense->oLocationMethodDocFromDocument
#         * Devel::PerlySense::Document->oLocationSubDefinition
#         * Devel::PerlySense::Document->oLocationPod
#     Devel::PerlySense::Document->oLocationPod
#             * Devel::PerlySense->aDocumentFindModuleWithInterface
#             Devel::PerlySense->aApiOfClass
#             * Devel::PerlySense::Document->determineLikelyApi0
#             Devel::PerlySense::Editor->textClassApi
#         Devel::PerlySense::Document->determineLikelyApi
#     Devel::PerlySense::Document->determineLikelyApi0
# Devel::PerlySense->oDocumentFindModule
";

ok(my $call_tree = Devel::PerlySense::CallTree->new(source => $source), "new ok");

warn "JPL: " . Dumper([ map { $_->id } @{$call_tree->callers} ]);









__END__

# Devel::PerlySense->oDocumentFindModule
#     Devel::PerlySense::Document->determineLikelyApi0
#         Devel::PerlySense::Document->determineLikelyApi
#             Devel::PerlySense::Editor->textClassApi
#             * Devel::PerlySense::Document->determineLikelyApi0
#             Devel::PerlySense->aApiOfClass
#             * Devel::PerlySense->aDocumentFindModuleWithInterface
#     Devel::PerlySense::Document->oLocationPod
#         * Devel::PerlySense::Document->oLocationPod
#         * Devel::PerlySense::Document->oLocationSubDefinition
#         Devel::PerlySense->oLocationMethodDocFromDocument
#             Devel::PerlySense::Document::Api::Method->new
#             Devel::PerlySense::Class->oLocationMethodDoc
#             * Devel::PerlySense->oLocationSmartDoc
#     Devel::PerlySense::Document->oLocationSubDefinition
#         * Devel::PerlySense::Document->oLocationSubDefinition
#         Devel::PerlySense->oLocationMethodDefinitionFromDocument
#             Devel::PerlySense::Class->oLocationMethodGoTo
#         * Devel::PerlySense->oLocationSmartGoTo
#     Devel::PerlySense::Class->newFromName
#         Devel::PerlySense::Class->findBaseClasses
#             Devel::PerlySense::Class->new
#     Devel::PerlySense->aDocumentFindModuleWithInterface
#         * Devel::PerlySense->oLocationSmartDoc
#         * Devel::PerlySense->oLocationSmartGoTo
#     Devel::PerlySense->classByName
#     Devel::PerlySense->oLocationSmartDoc
#     Devel::PerlySense->oLocationSmartGoTo
