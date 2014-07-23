#!/bin/csh
#
# A script wrapper around a UEngine call that is meant to get all available
# acars data in the A-II database over a specified range of times.
# The data is output to stdout as ASCII.  Each line is one time/platform
# combination.  The individual data items are comma delimited.
# This version can adapt to use a python stub that calls the
# data access framework.
#
# Usage:
#
#  a2acars.csh {p} yyyy-mm-dd hh:mm yyyy-mm-dd hh:mm
#
#  The literal p flag is optional.  The p flag means preserve
#  the final version of the python submitted to the UEngine instead of
#  cleaning it up.  The path to the finalized python is /tmp/a2acarsNNNNN.py
#  where NNNNN is a unix process id. 
#
#   This outputs the following set of variables for each line:
#
#     tailNumber,asciitime,latitude,longitude,acarsReceivingStation,
#     pressure,flightPhase,rollQuality,temperature,windDir,windSpeed,
#     relHumidity,mixRatio
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
#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./a2acarsStub.py ) then
    set stubpy = ./a2acarsStub.py
else if ( -e $mydir/a2acarsStub.py ) then
    set stubpy = $mydir/a2acarsStub.py
else if ( -e $FXA_HOME/src/dm/bufr/acars/a2acarsStub.py ) then
    set stubpy = $FXA_HOME/src/dm/bufr/acars/a2acarsStub.py
else if ( -e $FXA_HOME/bin/a2acarsStub.py ) then
    set stubpy = $FXA_HOME/bin/a2acarsStub.py
else
    bash -c "echo could not find a2acarsStub.py 1>&2"
    exit
endif
#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    set method = "daf"
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
endif
#
set specpy = /tmp/a2acars${$}.py
rm -rf $specpy >& /dev/null
touch $specpy
chmod 775 $specpy
cat $stubpy | sed "s/BBBBB/$1 $2/g" | sed "s/EEEEE/$3 $4/g" > $specpy
if ( "$method" == "daf" ) then
     /awips2/python/bin/python $specpy
else
    cd $UE_BIN_PATH
    ( uengine -r python < $specpy ) | grep -v '<' | grep -v Response
endif
if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
#
