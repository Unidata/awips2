#!/usr/bin/perl
#File: extractTag.pl

# This is a utility script that will extract the tag name from a
# Subversion URL / Location.

if (($#ARGV + 1) != 1)
{
   print "Usage: extractTag.pl 'SVN URL'";
   exit;
}
$svn_url = $ARGV[0];
@svn_url_split = split(/\//, $svn_url);
$numElements = scalar (@svn_url_split);
$tag = @svn_url_split[$numElements - 1];

print "$tag\n";
