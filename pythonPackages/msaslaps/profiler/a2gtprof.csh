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
# profiler data in the A-II database over a specified range of times.  The
# data is output to stdout as ASCII. Each line is one time/station combination.
# The individual data variables are comma delimited, and when what is returned
# for a data item is a profile, each item in the profile is vertical bar
# delimited. This version can adapt to use a python stub that calls the
# data access framework.
#
# Usage:
#
#  a2gtprof.csh {p} yyyy-mm-dd hh:mm yyyy-mm-dd hh:mm
#
#  The literal p flag is optional.  The p flag means preserve
#  the final version of the python submitted to the UEngine instead of
#  cleaning it up.  The path to the finalized python is /tmp/a2gtprofNNNNN.py
#  where NNNNN is a unix process id.
#
#   The following data variables are output for each line:
#
#    profilerId,validTime,latitude,longitude,elevation,pressure,
#    temperature,relHumidity,windDirSfc,windSpeedSfc,rainRate,submode,
#    height,levelMode,uComponent,vComponent,wComponent,
#    peakPower,HorizSpStdDev,VertSpStdDev,uvQualityCode,consensusNum
#
#   Everything from height onward are profiles.
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Oct 6, 2014     3594          nabowle        Initial modification. Handle DAF version.
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
# Locate python stub that we will modify to create the final python logic.
#
set stubbase = a2gtprofStub.py
if ( -e ./$stubbase ) then
    set stubpy = ./$stubbase
else if ( -e $mydir/$stubbase ) then
    set stubpy = $mydir/$stubbase
else if ( -e $FXA_HOME/src/dm/profiler/$stubbase ) then
    set stubpy = $FXA_HOME/src/dm/profiler/$stubbase
else if ( -e $FXA_HOME/bin/$stubbase ) then
    set stubpy = $FXA_HOME/bin/$stubbase
else
    bash -c "echo could not find $stubbase 1>&2"
    exit
endif
#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    /awips2/python/bin/python $stubpy -b "$1 $2" -e "$3 $4"
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

    set specpy = /tmp/a2gtprof${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    cat $stubpy | sed "s/BBBBB/$1 $2/g" | sed "s/EEEEE/$3 $4/g" > $specpy

    cd $UE_BIN_PATH
    #uengine -r python < $specpy
    ( uengine -r python < $specpy ) | grep -v '<' | sed -n '2,$p'

    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
endif

