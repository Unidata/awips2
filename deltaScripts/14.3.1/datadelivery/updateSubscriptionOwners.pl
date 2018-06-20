#!/usr/bin/perl

open (IN, $ARGV[0]);
print $ARGV[0];
@lines = <IN>;
close IN;
$site = "";
foreach $line (@lines) {
   if ($line =~ /originatingSite=&quot;([\w]+)&quot;/) {
   $site = $1;
   }
}

foreach $line (@lines) {
if ($line =~ s/owner="([\w]+)"/owner="$site"/g) {
print $line;
}
}
$OUTFILE = "TEMP";
open (OUT, ">>TEMP");
foreach $line (@lines) {
print OUT $line;
}
rename $OUTFILE, $ARGV[0]