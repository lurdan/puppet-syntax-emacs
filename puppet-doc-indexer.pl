#!/usr/bin/env perl

use strict;
use warnings;

use WWW::Mechanize;
use Web::Scraper;

my %xpath = (
  "configuration" => "nav/ol/li/ol/li/a",
  "metaparameter" => "nav/ol/li/ol/li/a",
  "type"          => "div[2]/nav/ol/li/a",
  "indirection"   => "nav/ol/li/a",
  "report"        => "nav/ol/li/a",
  "function"      => "nav/ol/li/a",
);

print "(\n";
while (my ($reftype, $xpath) = each(%xpath)) {
  fetch_index ($reftype, "/html/body/section[2]/div/div/div/$xpath");
}
print ")\n";

sub fetch_index {
  my $ref = $_[0];
  my $xpath = $_[1];

  my $mech = new WWW::Mechanize( autocheck => 1 );
  $mech->get("http://docs.puppetlabs.com/references/latest/${ref}.html");

  my $scraper = scraper {
    process $xpath, 'list[]' => 'TEXT';
  };
  my $res = $scraper->scrape($mech->content);
  &write_index($ref, $res->{list});
}

sub write_index {
  my ($ref, $keys) = @_;

  for my $key (@$keys) {
	my $anchor = $key;
	$anchor =~ s/_//g;
    print qq{("$key" "${ref}.html#${anchor}")\n};
  }
}

__END__

