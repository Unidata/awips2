#!/usr/local/perl/bin/perl
use LWP::Simple;
use Time::Local;
################################################################################
#                                                                              #
# Program name:  checkCWFGrids.pl                                              #
# Version:  1.0                                                                #
# Language (Perl, C-shell, etc.): perl                                         #
#                                                                              #
# Authors:  Virgil Middendorf (BYZ), Dave Pike (RNO)                           #
#                                                                              #
# Date of last revision:  05/16/08                                             #
#                                                                              #
# Script description: This script downloads information from the Consolidated  #
#    Web Farm netcdf file status page for the site provided by a command line  #
#    argument. In the netcdf file on ALL servers is more than 30 minutes old,  #
#    then the script will return a message to be later used for a Guardian     #
#    red-banner alarm.                                                         #
#                                                                              #
#    To run this script, then use these arguments:                             #
#       ./checkCWFGrids.pl WFO                                                 #
#    (where WFO is the three character uppercase wfo id)                       #
#                                                                              #
#    This script is designed to work in service backup situations and only     #
#    works on ls2/3.                                                             #
#                                                                              #
# Directory program runs from:  /data/ldad/grid                                #
#                                                                              #
# Needed configuration on ls2/3: You will need a /data/ldad/grid directory.    #
#                                Also, the checkCWFGrids.pl script needs to be #
#                                placed in the /data/ldad/grid directory.      #
#                                                                              #
# Revision History:                                                            #
# 05/15/08:  Created script. vtm                                               #
# 05/16/08:  Implemented 3 server check. vtm                                   #
################################################################################

## read in a command line argument that contains the uppercase WFO id
$site = $ARGV[0];

## file path and names where the webpages are stored on ls2/ls3.
$inputFile1 = "/data/ldad/grid/".$site."cwf1.txt";
$inputFile2 = "/data/ldad/grid/".$site."cwf2.txt";
$inputFile3 = "/data/ldad/grid/".$site."cwf3.txt";

## webpages to get
$url1 = "http://www.weather.gov/wtf/gfestatus.php";
$url3 = "http://www.crh.noaa.gov/wtf/gfestatus.php";

## get the current unix time
$currentTime = time()-32*60;

## get netcdf file unix time stamp from server #1
$response_code = getstore($url1, $inputFile1);
if (is_success($response_code)) {
   # Read the saved web page and determine time stamp.
   open(DATA, $inputFile1);
   @data= <DATA>;
   close(DATA);
   foreach $i (@data) {
      if ($i=~/$site/) {
         ($junk,$date) = split /font color=\"black\"\>/,$i;
         ($date,$junk) = split /\</,$date;
         ($date2,$time) = split / /,$date;
         ($mon,$day,$year) = split /-/,$date2;
         ($hour,$min) = split /:/,$time;
         last;
      }
   }
   $sec = 0;
   $gridTime = timegm($sec,$min,$hour,$day,$mon-1,$year-1900);
   $diff1 = ($currentTime-$gridTime)/60;
   unlink "$inputFile1";
} else {
   $diff1 = 999;
}
## get netcdf file unix time stamp from server #3
$response_code = getstore($url3, $inputFile3);
if (is_success($response_code)) {
   # Read the saved web page and determine time stamp.
   open(DATA, $inputFile3);
   @data= <DATA>;
   close(DATA);
   foreach $i (@data) {
      if ($i=~/$site/) {
         ($junk,$date) = split /font color=\"black\"\>/,$i;
         ($date,$junk) = split /\</,$date;
         ($date2,$time) = split / /,$date;
         ($mon,$day,$year) = split /-/,$date2;
         ($hour,$min) = split /:/,$time;
         last;
      }
   }
   $sec = 0;
   $gridTime = timegm($sec,$min,$hour,$day,$mon-1,$year-1900);
   $diff3 = ($currentTime-$gridTime)/60;
   unlink "$inputFile3";
} else {
   $diff3 = 999;
}

# Check to see if old. If so, then send message
if ($diff1 > 30 && $diff2 > 30 && $diff3 > 30) {
        print "$site grids were not sent to the Consolidated Web Farm. Last Sent at $date";
}
