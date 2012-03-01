#!/usr/bin/env perl

use strict;
use warnings;

use WWW::Mechanize;
use Web::Scraper;

print "(\n";
for my $reftype ("configuration", "metaparameter", "type") {
  fetch_index ($reftype, '/html/body/section[2]/div/div/div[2]/div/ol/li/ol/li/a');
}
for my $reftype ("function", "indirection", "network", "report") {
  fetch_index ($reftype, '/html/body/section[2]/div/div/div[2]/div/ol/li/a');
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

