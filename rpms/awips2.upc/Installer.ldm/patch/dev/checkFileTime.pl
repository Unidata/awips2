#!/bin/perl

#$path="/awips2/data_store/grid";

@paths=("/awips2/data_store/grid","/awips2/data_store/modelsounding");
foreach $path(@paths)
{

#find files that haven't been touched in the past 5 minutes 
  $syscmd="find $path -name \"*-concat-*\" -mmin +2"; 

  print "\t$syscmd\n";

  @output=`$syscmd`;

  foreach $line(@output)
  {
    chomp $line;
    if($line!~/\/staging\//) { next; }
    @dirs=split(/\//, $line);
    $outPath="";
    for($i=0; $i<$#dirs-1; $i++)
    { $outPath.=$dirs[$i]."/";  }
    $file=$dirs[-1];

    $syscmd = "mv $line $outPath";
    print "\t$syscmd\n";
    `$syscmd`;

   # $syscmd=" /awips2/python/bin/python /awips2/ldm/dev/notifyAWIPS2-unidata.py $outPath/$file";
    $syscmd="sudo su - awips -c \"/awips2/python/bin/python /awips2/fxa/bin/src/qpidNotify/qpidNotify.py $outPath/$file\"";

    print "\t$syscmd\n";
    `$syscmd`;
  }
}
