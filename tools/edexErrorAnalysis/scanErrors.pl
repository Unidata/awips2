#!/usr/bin/perl
# !----------------------------------------------------------------------------------------------
# Description:
#   scans the process logs for a specified server for errors. Outputs a date stamped file
#   containing an hourly breakdown of the errors. The output file is tab delineaded for
#   easy import into a spreadsheet (Excel) and includes column headers. Optionally, you
#   can specify the date of a log to scan. In that case, the log date will be used to time
#   stamp the output file.
#
# Usage:
#   scanerrors.pl directory [date] [home-dir]
#
# Prior to running the script, you need to get access to the log files. The following is
# one way to fo this:
#   1) Create a directory called /logs at the file system root.
#   2) In /logs, create a directory for each system you want to monitor. (e.g. int1, int2, etc)
#   3) As root, create a map from each of these directories to the Mule log directory on the
#      corresponding server. (e.g. mount awips-int1:/awips/edex/logs /logs/int1)
#   3) Create a folder called "shared" in your home directory. This is the output directory.
#   4) Run the script. Import the results into Excel and analyze.
# !----------------------------------------------------------------------------------------------
use strict;
use DirHandle;

use constant FALSE => 0;
use constant TRUE  => 1;

# get the command line arguments
(&printUsage(),exit(0)) unless (scalar @ARGV > 0);
my $server = shift;
my $sDate = shift;
my $home = shift;
unless ($sDate =~ /\d{2}\/\d{2}\/\d{4}/) {
   $home = $sDate;
   $sDate = "";
}

# define the search strings
my $file = "(\\d{4})(\\d{2})(\\d{2})\\.log\$";
my $processed = "^.+Processed file.+\$";
my $error = "^ERROR";
my $errData1 = "ERROR \\d{4}-(\\d{2}-\\d{2}) (\\d{2}).+\\[(.+)-\\d+\\] (.+?)\\: (Batch entry) \\d+.+?\\Z";
my $errData2 = "ERROR \\d{4}-(\\d{2}-\\d{2}) (\\d{2}).+\\[(.+)-\\d+\\] (.+?)\\: (org\\..+?Exception\\: .+?\\:).+?\\Z";
my $errData3 = "ERROR \\d{4}-(\\d{2}-\\d{2}) (\\d{2}).+\\[(.+)-\\d+\\] (.+?)\\: (.*?PostgresSQL error.+?)for.+?\\Z";
my $errData = "ERROR \\d{4}-(\\d{2}-\\d{2}) (\\d{2}).+\\[(.+)-\\d+\\] (.+?)\\: (.+?)\\Z";

my $memoryError = "^# java\.lang\.OutOfMemoryError";
# setup the IO directories
my $inDir = "/logs/$server";
$inDir = "$ENV{HOME}/$home$inDir" unless ($home eq "");
my $outDir = "$ENV{HOME}/Desktop/LinuxShare";

# hash containing results
#  $$results{server}{date}{hour}{process}{method}{message} = count
my $results = {};

my $outFile = sprintf("errors.%s.%s.txt",$server,($sDate?$sDate:&getDate()));
MAIN:
{
   print "Scanning $inDir, writing to $outFile\n";
   my $d = new DirHandle($inDir);
   my @logs = sort $d->read();
   $results = &scanLogs(@logs);
   &writeResults($results);
   exit(0);
}

sub writeResults {
   my ($data) = @_;
   open (OUT_FH,">${outDir}/${outFile}");
   print OUT_FH sprintf ("%s\t%s\t%s\t%s\t%s\t%s\t%s \n","Server","Date","Hour","Process","Method","Error Text","Count");
   foreach my $server (sort keys %$data) {
      foreach my $date (sort keys %{$$data{$server}}) {
         foreach my $hour (sort keys %{$$data{$server}{$date}}) {
            foreach my $process (sort keys %{$$data{$server}{$date}{$hour}} ) {
               foreach my $method (sort keys %{$$data{$server}{$date}{$hour}{$process}} ) {
                  foreach my $error (sort keys %{$$data{$server}{$date}{$hour}{$process}{$method}} ) {
                     my $count = $$data{$server}{$date}{$hour}{$process}{$method}{$error};
                     print OUT_FH sprintf ("%s\t%s\t%s\t%s\t%s\t%s\t%s \n",
                                           $server,$date,$hour,$process,$method,$error,$count);
                  }
               }
            }
         }
      }
   }
   close(OUT_FH);
}

sub scanLogs {
   my (@logs) = @_;
   my $retVal = {};
   my $line = ""; my $A = ""; my $B = ""; my $C = ""; my $D = "";
   foreach my $log (@logs) {
      if ($log =~ m/$file/) {
         my $date = "$2/$3/$1";
         if (!$sDate || ($sDate eq $date)) {
            print "Processing $date: ";
            my $lines = 0;
            my $count = 0;
            my $files = 0;
            open (IN_FILE,"$inDir/$log");
            while (my $line = <IN_FILE>) {
               chomp $line;
               if ($line =~ m/$error/) {
                  $count++;
                  if (($line =~ m/$errData1/) || ($line =~ m/$errData2/) || ($line =~ m/$errData3/) 
                     || ($line =~ m/$errData/))
                  {
		     $A = $1; $B = $2; $C = $3; $D = $4;
                     my $errorString = $5;
		     chomp $errorString;
                     # remove file path/names from string
                     $errorString =~ s/\/[a-z]+\/\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2}.+\s/XXX /;
                     $errorString =~ s/\/[a-z]+\/\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2}.+/XXX /;
                     # add a space if the string starts with '-' which cause a formula error in excel
                     $errorString =~ s/^-  //;
                     $errorString =~ s/^- //;
                     $errorString =~ s/^ -  //;
                     $errorString =~ s/^ - //;
                     $errorString =~ s/^-//;
		     if ($errorString =~ "failed persistence due to critical error logged above" )
                     {
                         $errorString = "Record X failed persistence due to critical error logged above";
                     }
		     # save the output from the reg expression before doing the replacement which will wipe them out
                     $$retVal{$server}{$A}{$B}{$C}{$D}{$errorString}++;
                  } else {
                     print "Error didn't match anything: $line \n";
		  }
                  
               } elsif ($line =~ m/$memoryError/){
                  print "Found memory Error";
                  $count++;
		  # use the last known server, date, and hour
                  $$retVal{$server}{$1}{$2}{"None"}{"Java Runtime Error"}{"java.lang.OutOfMemoryError"}++;
               } 
               if ($line =~ m/$processed/) {
                  $files++;
               }
               $lines++;
            }
            print sprintf("Lines = %d (Files = %d, Errors = %d)\n",$lines,$files,$count);
         }
      }
   }
   return $retVal;
}
sub getDate {
   my ($day,$month,$year) = (localtime)[3,4,5];
   $year += 1900;
   $month++;
   return "$month-$day-$year";
}
sub printUsage {
   print "Usage:\n";
   print "   $0  {dir} [mm/dd/yyyy] [home-dir]\n";
}


