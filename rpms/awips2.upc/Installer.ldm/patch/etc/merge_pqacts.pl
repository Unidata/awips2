#This perl script runs by taking in one argument (main or all) and creates a new pqact file by combining the individual pqacts

if($#ARGV != 0)
{
  $args=$#ARGV+1; 
  die "You provided " . $args . " argument(s) which is not valid. This script requires one command line argument.\nAn example of how to run this script is:\n\tperl merge_pqacts.pl all\n";
}
if($ARGV[0] ne "all" and $ARGV[0] ne "main")
{
  die "You provided an input argument of $ARGV[0] which is not valid. This script requires either \"all\" or \"main\" as an input argument. \nAn example of how to run this script is:\n\tperl merge_pqacts.pl all\n";
}

if($ARGV[0] eq "all")
{ 
  @files=("textplus","goesr","radar","mrms","grids");
  $output="pqact.conf.priority";
} elsif($ARGV[0] eq "main") {
  @files=("textplus","mrms","grids");
  $output="pqact.main";
}

@comments="";
@pqentries="";

foreach $file(@files)
{
  open IN, "<pqact.$file" or die "Cannot open pqact.$file\n";
  @lines=<IN>;
  $header=1;
  $comment=1;
  print "Reading in pqact.$file\n";

  for($i=0; $i<=$#lines; $i++)
  {
    #print "$i out of $#lines\n";
    chomp $lines[$i];
    if($header==1 and $lines[$i]=~/^# [0-9]/)
    {
      #This is in the comments section
      $fullComment=$lines[$i];
      while ($lines[$i+1]!~/^# [0-9]/)
      {
        chomp $lines[$i+1];
        if($lines[$i+1] eq "") { last; } 
        $fullComment="$fullComment\n$lines[$i+1]";
        $i++;
      }
      push(@comments,$fullComment);
      next;

    } elsif ($header==1 and ($lines[$i]=~/^\n/ or $lines[$i] eq "")) {
      #This means we are done with the comments section
      #print "We are done with the comments section\n";
      $comment=0;
    } elsif ($comment==0 and $header==1) {
      #This is where we are finishing the specific header - read in the next 5 lines
      #print "Finished with all headers\n";
      for($j=0; $j<5; $j++)
      {
        chomp $lines[$i+$j];
        #print "$lines[$i+$j]\n";
        push(@pqentries,$lines[$i+$j]);
      }
      $i+=4;
      $header=0;
      next;
    } elsif ($header==0) {
      #This is the start of the pqact entries
      #Need specific case to end for goesr
      if($file eq "goesr" and $lines[$i]=~/GOES East Derived Products/)
      {
        $i=$#lines;
        next;
      }
      #print "$lines[$i]\n";
      push(@pqentries,$lines[$i]);
    }

  }
  close IN;
}

open OUT, ">$output" or die "Cannot write $output\n";
@sortedComments= sort @comments;


print OUT "#
# Unidata AWIPS LDM Default Pattern Actions
#\n
";
foreach $sc(@sortedComments)
{
  print OUT $sc."\n";
}

foreach $entry(@pqentries)
{
  print OUT "$entry\n";
}
