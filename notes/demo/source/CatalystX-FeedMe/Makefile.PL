use inc::Module::Install;

name 'CatalystX-FeedMe';
all_from 'lib\CatalystX\FeedMe.pm';

requires 'Catalyst' => '5.7006';
requires 'Catalyst::Plugin::ConfigLoader';
requires 'Catalyst::Plugin::Static::Simple';
requires 'Catalyst::Action::RenderView';
requires 'YAML'; # This should reflect the config file format you've chosen
                 # See Catalyst::Plugin::ConfigLoader for supported formats

requires 'Path::Class';
requires 'Catalyst::Model::DBIC::Schema';
requires 'DBIx::Class::HTMLWidget';
requires 'Catalyst::Plugin::HTML::Widget';

requires 'XML::Atom::Syndication';
requires 'Catalyst::Plugin::AtomServer';

requires 'LWP::Simple';
requires 'WWW::Mechanize';



catalyst;

install_script glob('script/*.pl');
auto_install;
WriteAll;
