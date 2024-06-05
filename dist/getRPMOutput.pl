#!/usr/bin/perl

$date="20240521";

$path="/awips2/repo/awips2/dist/el8-dev-$date";
#$path="~/dev/awips2_rh8/awips2";

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

