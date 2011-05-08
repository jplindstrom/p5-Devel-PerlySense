#!perl
use strict;

use YAML::Tiny qw/Dump/;
use Data::Dumper;



my $project_config = {

    project => {
        #The human readable name of the Project
        moniker => "A PerlySense Project",

        #Extra @INC directories, relative to the project root
        inc_dir => [ "sdfj" ],
    },

    #These are evaluated in order to find a way to run a file. First
    #match is used.
    run_file => [
        {
            moniker => "Test",
            rex => q/\.t$/,
            command => 'prove -v ${INC} "${SOURCE_FILE}"',
            run_from => "project_root",
        },

        {
            moniker => "Module",
            rex => q/\.pm$/,
            command => 'perl -c ${INC} "${SOURCE_FILE}"',
            run_from => "project_root",
        },
        {
            moniker => "Script",
            rex => q/\.pl$/,
            command => 'perl ${INC} "${SOURCE_FILE}"',
            run_from => "cwd",
        },
        
        #This is a catch-all for all other types of files
        {
            moniker => "Script (no .pl)",
            rex => q/\./,
            command => 'perl ${INC} "${SOURCE_FILE}"',
            run_from => "cwd",
        },

    ],

};

# $project_config = {
#     a => [
#         {
#             a => "dlkjf",
#             b => 2,
#         },
#         2,
#         3,
#         4,            
#     ],
# };


print Dumper($project_config);
print Dump($project_config);




__END__
