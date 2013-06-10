#!/bin/bash
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
# edex startup script

# Verify that awips2-python and awips2-java are installed.
rpm -q awips2-python > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: awips2-python Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

rpm -q awips2-java > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: awips2-java Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

rpm -q awips2-psql > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: awips2-psql Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# Find the locations of awips2-python and awips2-java.
PYTHON_INSTALL="/awips2/python"
JAVA_INSTALL="/awips2/java"
PSQL_INSTALL="/awips2/psql"

path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

export EDEX_HOME=$(dirname $dir)
awips_home=$(dirname $EDEX_HOME)

# Source The File With The Localization Information
source ${dir}/setup.env

#source /awips2/edex/bin/yajsw/bin/setenv.sh

### AWIPS 1 support ###
if [ -f /etc/rc.config.d/AWIPS ]; then
   . /etc/rc.config.d/AWIPS
fi
export SHLIB_PATH=$PROJECT/sharedlib
### End AWIPS 1 support ###

export HOSTNAME=`hostname`

# set Python & Java into the path
export PATH=$awips_home/bin:${JAVA_INSTALL}/bin:${PYTHON_INSTALL}/bin:$PATH

# set Service Backup scripts into the path
export PATH=$PATH:$awips_home/GFESuite/bin:$awips_home/GFESuite/ServiceBackup/scripts

# set AWIPS 1 stuff into path
export PATH=$PATH:$PROJECT/bin

export JAVA_HOME="${JAVA_INSTALL}"
export LD_LIBRARY_PATH=$EDEX_HOME/lib/native/linux32/awips1:${JAVA_INSTALL}/lib:${PYTHON_INSTALL}/lib:${PSQL_INSTALL}/lib:$PROJECT/sharedLib
export LD_LIBRARY_PATH=/awips2/edex/lib/lib_illusion:$LD_LIBRARY_PATH

export LD_PRELOAD="libpython.so"
export FXA_DATA=$EDEX_HOME/data/fxa
export ALLOW_ARCHIVE_DATA="false"

# setup environment for HPE
export AMQP_SPEC=$awips_home/python/share/amqp/amqp.0-10.xml

#-------------------------------------------------------------------------
#read and interpret the command line arguments
#-------------------------------------------------------------------------
CONSOLE_FLAG=on
CONSOLE_LOGLEVEL=DEBUG
DEBUG_FLAG=off
PROFILE_FLAG=off
CONF_FILE="wrapper.conf"
RUN_MODE=
for arg in $@
do
  case $arg in
    -b|-d|--debug|-db|-bd) DEBUG_FLAG=on;;
    -p|--profiler) PROFILE_FLAG=on;;
    -h|--highmem) ;;  # does nothing, only here to prevent issues if someone still uses -h
    -noConsole) CONSOLE_FLAG=off;;
    *) RUN_MODE=$arg;;
  esac
done

export EDEX_RUN_MODE=$RUN_MODE

if [ $CONSOLE_FLAG == "off" ]; then
	CONSOLE_LOGLEVEL=NONE
fi

export CONSOLE_LOGLEVEL

# source environment files
. $EDEX_HOME/etc/default.sh

if [ -e $EDEX_HOME/etc/${RUN_MODE}.sh ]; then
    . $EDEX_HOME/etc/${RUN_MODE}.sh
fi

if [ $PROFILE_FLAG == "on" ]; then
    . $EDEX_HOME/etc/profiler.sh
fi

# enable core dumps
#ulimit -c unlimited

# determine whether we are running a 32-bit or 64-bit version of EDEX -
# based on the AWIPS II Java that has been installed
EDEX_BITS="32"
EXPECTED_ARCH=`file ${JAVA_INSTALL}/bin/java | awk '{ print $3; }'`
if [ "${EXPECTED_ARCH}" = "64-bit" ]; then
   EDEX_BITS="64"
fi
export EDEX_BITS=${EDEX_BITS}

WRAPPER_ARGS=""
if [ $DEBUG_FLAG == "on" ]; then
   WRAPPER_ARGS="wrapper.java.debug.port=${EDEX_DEBUG_PORT}"
   echo "To Debug ... Connect to Port: ${EDEX_DEBUG_PORT}."
fi

java -jar ${EDEX_HOME}/bin/yajsw/wrapper.jar -c ${EDEX_HOME}/conf/${CONF_FILE} ${WRAPPER_ARGS}
