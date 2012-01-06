#!/bin/bash
# CAVE startup script
# Note: CAVE will not run as 'root'

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

dir=${0%/*}

if [ "$dir" = "$0" ]; then

 dir="."

fi

cd "$dir"

# Since, we no longer need to worry about re-location ...
CAVE_INSTALL="/awips2/cave"
JAVA_INSTALL="/awips2/java"
PYTHON_INSTALL="/awips2/python"

export AWIPS_INSTALL_DIR="${CAVE_INSTALL}"

export LD_LIBRARY_PATH=${JAVA_INSTALL}/lib:${PYTHON_INSTALL}/lib:$LD_LIBRARY_PATH
export PATH=${JAVA_INSTALL}/bin:${PYTHON_INSTALL}/bin:$PATH
export JAVA_HOME="${JAVA_INSTALL}/jre"

# The user can update this field if they choose to do so.
export HYDRO_APPS_DIR="/awips2/edex/data/hdf5/hydroapps"

TESTCHECK="$TMCP_HOME/bin/getTestMode"
if [ -x ${TESTCHECK} ]; then
    echo "Calling getTestMode()"
    ${TESTCHECK}
    status=${?}
    if [ $status -eq 11 ]; then
        MODE="TEST"
        SWITCHES=" -mode TEST "
    elif [ $status -eq 12 ];then
        MODE="PRACTICE"
        SWITCHES=" -mode PRACTICE "
    elif [ $status -eq 15 ];then
        MODE="OPERATIONAL"
        SWITCHES=" -mode OPERATIONAL"
    else
        MODE="OPERATIONAL (no response)"
        SWITCHES=" "
    fi
    echo "getTestMode() returned ${MODE}"
else
    MODE="UNKNOWN"
    echo "getTestMode() not found - going to use defaults"
fi

export TEXTWS=`hostname | sed -e 's/lx/xt/g'`

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
        export SWITCHES="${SWITCHES} -no_shader"
fi

export LD_PRELOAD=libpython.so
/awips2/cave/cave ${SWITCHES} -nosplash -component gfeclient "$@"

if [ -n "$xvfb" ]
then
        echo "Killing Xvfb process id: $xvfb"
        kill $xvfb
fi
