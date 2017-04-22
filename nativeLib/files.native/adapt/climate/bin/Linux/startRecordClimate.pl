#!/usr/local/perl/bin/perl -w

use Env;

#-------------------------------------------------------------------------------
# This script is called by startingest.  
# this script which is needed to start up a recordClimate process.
# Before starting a recordClimate process, any leftover instances are terminated
# by calling killProc. 
# 
# MODIFICATION HISTORY:
#   NAME           DATE       CHANGES
#   ----           ----       -------
#   Bob Morris     12/09/002  - Change path adapt_apps/bin to climate/bin/HP-UX
#                               for OB2.  Must change again once DS is Linux.
#                             - Get env vars from ifps file for OB2.  Failure to
#                               find/set will be fatal after icwf_site.ccc file
#                               is removed in later build.
#   Bob Morris     02/13/002  - Strip double quotes from $wfoname after grep.
#   Bob Morris     03/06/003  - Add " umask 000; " to startup command so that
#                               pipe files are writeable by non-fxa after manual
#                               restarts.  OB2 DR_12281
#   Bob Morris     03/28/03   - Fix env var ID for IFPS_SITE_OFFICE_NAME
#-------------------------------------------------------------------------------

#system (" ${FXA_HOME}/bin/killProc recordClimate");

print "In startRecordClimate.pl, setting up partial IFPS environment:\n\n";
print "ICWF_SITE = $ICWF_SITE\n\n";

$ifpsenvfile = "/awips/adapt/ifps/localbin/ifps-";
$ifpsenvfile .= $ICWF_SITE;
$ifpsenvfile .= ".env";
if ( -s $ifpsenvfile )
{
  $wfoname = `grep "IFPS_SITE_OFFICE_NAME=" $ifpsenvfile | cut -f2 -d=`;
  $wfoname =~ s/\"//g;
  $wfoname =~ s/'//g;
  $wfoname =~ s/\n//;
  if ($wfoname eq "") {
    print "In startRecordClimate.pl:  WARNING!!\n";
    print "IFPS_SITE_OFFICE_NAME not set in $ifpsenvfile !!\n";
    print "RER products will have erroneous WFO name !!\n\n";
  }
  else {
    $ENV{'IFPS_SITE_OFFICE_NAME'} = $wfoname;
    system ("echo IFPS_SITE_OFFICE_NAME = \$IFPS_SITE_OFFICE_NAME");
  }

  $wfotz = `grep "IFPS_SITE_TIMEZONE=" $ifpsenvfile | cut -f2 -d=`;
  $wfotz =~ s/\n//;
  if ($wfotz eq "") {
    print "In startRecordClimate.pl:  WARNING!!\n";
    print "IFPS_SITE_TIMEZONE not set in $ifpsenvfile !!\n";
    print "RER products will have erroneous issue time !!\n\n";
  }
  else {
    $ENV{'IFPS_SITE_TIMEZONE'} = $wfotz;
    system ("echo IFPS_SITE_TIMEZONE = \$IFPS_SITE_TIMEZONE");
  }
}
else {
print "In startRecordClimate.pl, setting up partial IFPS environment,\n";
print "file empty or not found:  $ifpsenvfile \n";
}
# Figure out which platform we are on and run that binary.
#system("umask 000; /usr/bin/ddd /awips/adapt/climate/bin/`uname -s`/recordClimate");
system("umask 000; /awips/adapt/climate/bin/`uname -s`/recordClimate");
#system ("/awips/fxa/data/recordClimate > /dev/null 2>&1 &");

