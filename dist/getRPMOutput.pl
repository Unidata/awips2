#!/usr/bin/perl

$date="20240108";

$path="/awips2/repo/awips2/dist/el7-dev-$date";

@repos=("noarch","x86_64");

foreach $repo(@repos)
{
  chomp $repo;

  @rpms=`ls $path/$repo`;
  foreach $rpm(@rpms)
  {
    chomp $rpm;
    $output=`rpm -qpl $path/$repo/$rpm`;
    @splitOutput=split(/\n/,$output);

    foreach $l(@splitOutput)
    {
      chomp $l;
      print "$rpm:=====$l\n";
    }
  }
}

