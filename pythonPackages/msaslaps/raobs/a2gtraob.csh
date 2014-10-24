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
# raob data in the A-II database over a specified range of times.  The data
# is output to stdout as ASCII.   This version can adapt to use a python 
# stub that calls the data access framework.
#
# Usage:
#
#  a2gtraob.csh {p} yyyy-mm-dd hh:mm yyyy-mm-dd hh:mm
#
#  The literal p flag is optional.  The p flag means preserve
#  the final version of the python submitted to the UEngine instead of
#  cleaning it up.  The path to the finalized python is /tmp/a2gtraobNNNNN.py
#  where NNNNN is a unix process id.
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Oct 10, 2014    3595          nabowle        Initial modification. Fix sed call.
#    Oct 10, 2014    3595          nabowle        Handle DAF version.
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
set stubbase = a2gtraobStub.py
set stubpy = ./${stubbase}
if ( $?FXA_HOME ) then
    if ( -e ./${stubbase} ) then
        set stubpy = ./${stubbase}
    else if ( -e $FXA_HOME/src/dm/raob/${stubbase} ) then
        set stubpy = $FXA_HOME/src/dm/raob/${stubbase}
    else if ( -e $FXA_HOME/bin/${stubbase} ) then
        set stubpy = $FXA_HOME/bin/${stubbase}
    endif
endif

set staInf = $PWD/wmoToNameRaob.txt
if ( $?FXA_HOME ) then
    if ( -e $FXA_HOME/data/wmoToNameRaob.txt ) \
        set staInf = $FXA_HOME/data/wmoToNameRaob.txt
endif

#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
# Replace first field with station names substitutions in wmoToNameRaob.txt or
# with what's stored in the database. They are similar but do not match 100%.
    /awips2/python/bin/python $stubpy -b "$1 $2" -e "$3 $4" | sed -f $staInf
#    /awips2/python/bin/python $stubpy -b "$1 $2" -e "$3 $4" --use-station-name
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

    set specpy = /tmp/a2gtraob${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    cat $stubpy | sed "s/BBBBB/$1 $2/g" | sed "s/EEEEE/$3 $4/g" > $specpy

    cd $UE_BIN_PATH
    ( uengine -r python < $specpy ) | grep -v '<' | sed -n '2,$p' | \
         sed -f $staInf

    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
endif

