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
# maritime data in the A-II database over a specified range of times.
# The data is output to stdout as ASCII.  Each line is one time/platform
# combination.  The individual data items are comma delimited.
# This version can adapt to use a python stub that calls the
# data access framework.
#
# Usage:
#
#  a2aircraft.csh {p} {t} yyyy-mm-dd hh:mm yyyy-mm-dd hh:mm
#
#  The literal p flag is optional.  The p flag means preserve
#  the final version of the python submitted to the UEngine instead of
#  cleaning it up.  The path to the finalized python is /tmp/a2aircraftNNNNN.py
#  where NNNNN is a unix process id. This flag does nothing when using the DAF
#  version.
#
#  The optional literal t mean change some of the ascii phenomena descriptions
#  for pireps into codes suitable for D-2D format aircraft data files.
#
#   This outputs the following set of variables for each line:
#
#     lat|lon,asciitime,flightLevel,reportType,aircraftType,
#     temperature,windDir,windSpeed,visibility,
#     FlightWeather*,FlightHazard*,FlightConditions*,WeatherGroup*,
#     numCloudLayers,cloudBaseHeight,cloudTopHeight,cloudAmount,
#     numIcingLayers,heightBaseIcing,heightTopIcing,typeIcing,intensityOfIcing,
#     numTurbulenceLayers,heightBaseTurb,heightTopTurb,
#     typeTurbulence,intensityOfTurbulence
#
#  Asterisk (*) means variable that does not directly correspond to a
#  variable in the D-2D format aircraft data files.
# 
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/02/2014      3405          nabowle        Initial modification to better handle daf.
#
#
set rmpy = yes
set typcod = qwertyuiop
if ( "$1" == "t" ) then
    set typcod = "typecode = 'no'"
    shift
endif
if ( "$1" == "p" ) then
    set rmpy = no
    shift
endif
set repType = ""
set rt = `echo $1 | tr 'a-z' 'A-Z' | grep '^.IREP$'`
if ( "$rt" != "" ) then
    set repType = $rt
    shift
endif
if ( "$1" == "p" ) then
    set rmpy = no
    shift
endif
if ( "$1" == "t" ) then
    set typcod = "typecode = 'no'"
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
#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./a2airepStub.py ) then
    set stubpy = ./a2airepStub.py
else if ( -e $mydir/a2airepStub.py ) then
    set stubpy = $mydir/a2airepStub.py
else if ( -e $FXA_HOME/src/dm/bufr/acars/a2airepStub.py ) then
    set stubpy = $FXA_HOME/src/dm/bufr/acars/a2airepStub.py
else if ( -e $FXA_HOME/bin/a2airepStub.py ) then
    set stubpy = $FXA_HOME/bin/a2airepStub.py
else
    bash -c "echo could not find a2airepStub.py 1>&2"
    exit
endif
#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    set method = "daf"
    if ( "$repType" != "PIREP" ) then
        /awips2/python/bin/python $stubpy -b "$1 $2" -e "$3 $4" --match-legacy
#        /awips2/python/bin/python $stubpy -b "$1 $2" -e "$3 $4"
    endif
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

    #
    set specpy = /tmp/a2airep${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    cat $stubpy | sed "s/BBBBB/$1 $2/g" | sed "s/EEEEE/$3 $4/g" > $specpy
    if ( "$repType" != "PIREP" ) then
        set here = `pwd`
        cd $UE_BIN_PATH
        ( uengine -r python < $specpy ) | grep -v '<' | grep -v Response
        cd $here
    endif
    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
endif

#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./a2pirepStub.py ) then
    set stubpy = ./a2pirepStub.py
else if ( -e $mydir/a2pirepStub.py ) then
    set stubpy = $mydir/a2pirepStub.py
else if ( -e $FXA_HOME/src/dm/bufr/acars/a2pirepStub.py ) then
    set stubpy = $FXA_HOME/src/dm/bufr/acars/a2pirepStub.py
else if ( -e $FXA_HOME/bin/a2pirepStub.py ) then
    set stubpy = $FXA_HOME/bin/a2pirepStub.py
else
    bash -c "echo could not find a2pirepStub.py 1>&2"
    exit
endif
#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    set method = "daf"
    if ( "$repType" != "AIREP" ) then
        # By default, match the legacy output.
        set pyargs = "--match-legacy"
#        set pyargs = ""
        if ( "$typcod" != "qwertyuiop" ) then
            set pyargs = "$pyargs -t"
        endif

        /awips2/python/bin/python $stubpy -b "$1 $2" -e "$3 $4" $pyargs
    endif
else 
    if ( "$method" == "daf" ) then
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
    endif

    #
    set specpy = /tmp/a2pirep${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    cat $stubpy | sed "s/BBBBB/$1 $2/g" | sed "s/EEEEE/$3 $4/g" | \
         grep -v "$typcod" > $specpy
    cd $UE_BIN_PATH
    if ( "$repType" != "AIREP" ) then
        cd $UE_BIN_PATH
        ( uengine -r python < $specpy ) | grep -v '<' | grep -v Response
    endif
    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
endif

