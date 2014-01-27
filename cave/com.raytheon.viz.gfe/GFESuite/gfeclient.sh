#!/bin/bash

# CAVE startup script
# Note: CAVE will not run as 'root'

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
#
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Dec 04, 2013  #2589     dgilling    Create command-line arg that controls
#                                     xvfb initialization.
# Dec 05, 2013  #2593     rjpeter     set IGNORE_NUM_CAVES
# Dec 05, 2013  #2590     dgilling    Remove duplicated code and call to 
#                                     cave.sh.
#
#


user=`/usr/bin/whoami`
if [ ${user} == 'root' ];then
   echo "WARNING: CAVE cannot be run as user '${user}'!"
   echo "         change to another user and run again."
   exit 1
fi

source /awips2/cave/caveUtil.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: unable to find and/or access /awips2/cave/caveUtil.sh."
   exit 1
fi

PROGRAM_NAME="gfeclient"

# remove "-enablegl" flag from command-line if set so it doesn't confuse any
# commands we call later.
USER_ARGS=()
while [[ $1 ]]
do
    if [ "$1" == "-enablegl" ]
    then
        ENABLEGL="true"
    else
        USER_ARGS+=("$1")
    fi
    shift
done

if [ -n "$ENABLEGL" ]
then
    # if display not set
    if [ -n "$DISPLAY" ]
    then
        echo "Using Display set to $DISPLAY"
        extendLibraryPath
    else
        echo "Display not set, creating offscreen x on port $$"
        extendLibraryPath "-noX"
        Xvfb :$$ -screen 0 1280x1024x24 &
        xvfb=$!
        export DISPLAY="localhost:$$.0"
        #don't use shader when no display set 
        SWITCHES="${SWITCHES} -no_shader"
    fi
fi

export IGNORE_NUM_CAVES=1

source /awips2/cave/cave.sh -nosplash -noredirect -component gfeclient "${USER_ARGS[@]}" &
wait

if [ -n "$xvfb" ]
then
        echo "Killing Xvfb process id: $xvfb"
        kill $xvfb
fi
