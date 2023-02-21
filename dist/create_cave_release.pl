#!/usr/bin/perl

$pwd=`pwd`;
chomp $pwd;

$dir="el7-dev-20230216";
$baseDir="$dir-cave";

`rm -rf $baseDir`;
`mkdir $baseDir`;

open IN, "<comps-cave.xml" or die "cannot open\n";
@lines=<IN>;

foreach $line(@lines)
{
  chomp $line;
  if($line !~/mandatory/) { next; }
  $line=~s/<\/packagereq>//g;
  @splitLine=split(/>/,$line);

  $rpm=$splitLine[1];
  print "$rpm\n";

  $find=`find $pwd/$dir -name $rpm-[0-9]*rpm`;
  chomp $find;
  
  if($find=~/noarch/)
  {  `rsync -aP $dir/noarch/$rpm-[0-9]*rpm $baseDir/noarch/`;  }
  if($find=~/x86_64/)
  { `rsync -aP $dir/x86_64/$rpm-[0-9]*rpm $baseDir/x86_64/`; }
}

`sudo su - -c \"createrepo -g $pwd/comps-cave.xml $pwd/$baseDir\"`;
`sudo chown -R awips:fxalpha $baseDir`;
