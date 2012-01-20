#!/usr/bin/perl
#File: extractTag.pl

# This is a utility script that will extract the tag name from a
# Subversion URL / Location.

if (($#ARGV + 1) != 1)
{
   print "Usage: extractSite.pl 'Localization Directory'\n";
   exit;
}
$localizationDirectory = $ARGV[0];
@svn_dir_split = split(/\./, $localizationDirectory);
$numElements = scalar (@svn_dir_split);
$site = @svn_dir_split[$numElements - 1];

print "$site\n";
