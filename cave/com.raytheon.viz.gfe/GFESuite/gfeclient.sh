#!/bin/bash
# CAVE startup script
# Note: CAVE will not run as 'root'

#
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Dec 05, 2013  #2593     rjpeter     set IGNORE_NUM_CAVES
# Dec 05, 2013  #2590     dgilling    Remove duplicated code and call to 
#                                     cave.sh.
# Sep 15, 2016  #18799    bhunderm    Correct "no more handles" issue.
# Jan 26, 2017  #6092     randerso    Updated for gfeClientServer  
# Aug 17, 2017  #6092     randerso    Fix no_shader arg so it works
#                                     Only use noredirect when running
#                                     from command line  
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

OPTIONAL_ARGS=()
#
# if parent process is request JVM 
#    set COMPONENT to gfeClientServer
# else
#    set COMPONENT to gfeclient
#
COMPONENT="gfeclient"
ps -fp $PPID | grep -q "edex.run.mode=request"
if [ $? == 0 ]
then
    COMPONENT="gfeClientServer"
else
    OPTIONAL_ARGS+=(-noredirect)
fi
export PROGRAM_NAME=${COMPONENT}

# if display not set
if [ -n "$DISPLAY" ]
then
        echo "Using Display set to $DISPLAY"
        extendLibraryPath
else
        echo "Display not set, creating offscreen x on port $$"
        extendLibraryPath "-noX"
        Xvfb :$$ -screen 0 1280x1024x30 -nolisten tcp &
        xvfb=$!
        export DISPLAY=":$$.0"
        #don't use shader when no display set 
        OPTIONAL_ARGS+=(-no_shader)
fi

export IGNORE_NUM_CAVES=1
export CAVE_LOG_DAYS_TO_KEEP=7

trap killIt TERM

function killIt() {
    kill ${PID}
}

exec /awips2/cave/cave.sh -nosplash "${OPTIONAL_ARGS[@]}" -component ${COMPONENT} "$@" &
PID=$!
wait ${PID}
exitCode=$?

if [ -n "$xvfb" ]
then
        echo "Killing Xvfb process id: $xvfb"
        kill $xvfb
fi

exit $exitCode
