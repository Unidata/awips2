#!/bin/csh
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

#
# A script wrapper around a UEngine call that is meant to get all available
# metar data in the A-II database over a specified range of times.  The data
# is output to stdout as ASCII.  Each line is one time/station combination.
# The individual data items are comma delimited. This version can adapt to 
# use apython stub that calls the data access framework.
#
# Usage:
#
#  a2gtmtr.csh {p} {c} yyyy-mm-dd hh:mm yyyy-mm-dd hh:mm
#
#  The literal p and c flags are optional.  The p flag means preserve
#  the final version of the python submitted to the UEngine instead of
#  cleaning it up.  The path to the finalized python is /tmp/a2gtmtrNNNNN.py
#  where NNNNN is a unix process id.  The c flag means to retreive the
#  Laps set of variables, instead of the default MSAS set.
#
#   Not using the 'c' format, the MSAS set of variables, outputs the following
#   variables for each line:
#
#     stationName,timeObs,latitude,longitude,elevation,wmoId,autoStationType
#     seaLevelPress,temperature,dewpoint,windDir,windSpeed,altimeter
#
#   Using the 'c' format, the Laps set of variables, outputs the following
#   variables for each line:
#
#     stationName,timeObs,latitude,longitude,elevation,wmoId,autoStationType
#     reportType,presWeather,visibility,skyCover,skyLayerBase,altimeter
#     seaLevelPress,pressChange3Hour,pressChangeChar,temperature,tempFromTenths
#     dewpoint,dpFromTenths,windDir,windSpeed,windGust,maxTemp24Hour,
#     minTemp24Hour,precip1Hour,precip3Hour,precip6Hour,precip24Hour
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/2014      3593          nabowle        Initial modification to properly use DAF version of the script.
#
#

set rmpy = yes
if ( "$1" == "p" ) then
    set rmpy = no
    shift
endif
#
# Identify directory this script is in, will be one of the directories we
# search for other files in.
#
set mydir = `dirname $0`
set d1 = `echo $mydir | cut -c1`
if ( "$mydir" == '.' ) then
     set mydir = $PWD
else if ( "$d1" != "/" ) then
     set mydir = $PWD/$mydir
endif
set mydir = `(cd $mydir ; pwd)`
if ( ! $?FXA_HOME ) set FXA_HOME = xxxx
if ( ! $?FXA_LOCAL_SITE ) set FXA_LOCAL_SITE = xxxx
if ( ! $?FXA_INGEST_SITE ) set FXA_INGEST_SITE = $FXA_LOCAL_SITE
#
set stubbase = a2gtmtrStub.py
if ( "$1" == "c" ) then
    shift
    set stubbase = a2cvmtrStub.py
endif
#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./$stubbase ) then
    set stubpy = ./$stubbase
else if ( -e $mydir/$stubbase ) then
    set stubpy = $mydir/$stubbase
else if ( -e $FXA_HOME/src/dm/metar/$stubbase ) then
    set stubpy = $FXA_HOME/src/dm/metar/$stubbase
else if ( -e $FXA_HOME/bin/$stubbase ) then
    set stubpy = $FXA_HOME/bin/$stubbase
else
    bash -c "echo could not find $stubbase 1>&2"
    exit
endif

#
# Attempt to use current D-2D localization to determine lat/lon bounds.
#
set ltmn = 0
set ltmx = 90
set lnmn = -180
set lnmx = 180
set locDir = $FXA_HOME/data/localizationDataSets/$FXA_INGEST_SITE
while ( -e $locDir/CenterPoint.dat )
   grep conusScale $locDir/scaleInfo.txt >& /dev/null
   if ( $status == 0 ) then
       set ltmn = 15
       set ltmx = 60
       set lnmn = -145
       set lnmx = -55
       break
   endif
   set cenLoc = `cat $locDir/CenterPoint.dat`
   if ( $#cenLoc != 2 ) break
   set cenlat = `echo $cenLoc[1] | cut '-d.' -f1`
   set cenlat = `( @ x = $cenlat + 0 >& /dev/null ; echo $x )`
   if ( "$cenlat" == "" ) break
   set cenlon = `echo $cenLoc[2] | cut '-d.' -f1`
   set cenlon = `( @ x = $cenlon + 0 >& /dev/null ; echo $x )`
   if ( "$cenlon" == "" ) break
   if ( $cenlat > 75 ) then
       set ltmn = 55
       break
   endif
   if ( $cenlat > 50 ) then
       @ ltmn = $cenlat - 20
       break
   endif
   @ ltmn = $cenlat - 20
   @ ltmx = $cenlat + 20
   @ lnmn = $cenlon - 20
   @ lnmx = $cenlon + 20
   break
end
#
#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    /awips2/python/bin/python $stubpy -b "$1 $2" -e "$3 $4" --lat-min $ltmn --lat-max $ltmx --lon-min $lnmn --lon-max $lnmx
else
    #
    # Set up the environment we need to run the UEngine.
    #
    set method = "uengine"
    if ( -e ./UEngine.cshsrc ) then
        set ueenv = ./UEngine.cshsrc
    else if ( -e $mydir/UEngine.cshsrc ) then
        set ueenv = $mydir/UEngine.cshsrc
    else if ( -e $FXA_HOME/src/dm/point/UEngine.cshsrc ) then
        set ueenv = $FXA_HOME/src/dm/point/UEngine.cshsrc
    else if ( -e $FXA_HOME/bin/UEngine.cshsrc ) then
        set ueenv = $FXA_HOME/bin/UEngine.cshsrc
    else
        bash -c "echo could not find UEngine.cshsrc 1>&2"
        exit
    endif
    source $ueenv

    set specpy = /tmp/a2gtmtr${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    cat $stubpy | sed "s/LTMN/$ltmn/g" | sed "s/LTMX/ /g" | \
                  sed "s/LNMN/$lnmn/g" | sed "s/LNMX/$lnmx/g" | \
                  sed "s/BBBBB/$1 $2/g" | sed "s/EEEEE/$3 $4/g" > $specpy
    cd $UE_BIN_PATH
    ( uengine -r python < $specpy ) | grep -v '<' | sed -n '3,$p'
    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
endif
#
