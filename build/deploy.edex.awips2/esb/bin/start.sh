#!/bin/bash
# edex startup script

# Verify awips2 RPMs are installed
rpm -q awips2-python > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
    echo "ERROR: awips2-python Must Be Installed."
    exit 1
fi
rpm -q awips2-java > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
    echo "ERROR: awips2-java Must Be Installed."
    exit 1
fi
rpm -q awips2-psql > /dev/null 2>&1
RC=$?
if [ ${RC} -ne 0 ]; then
    echo "ERROR: awips2-psql Must Be Installed."
    exit 1
fi
rpm -q awips2-yajsw > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "ERROR: awips2-yajsw Must Be Installed."
    exit 1
fi

path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)
export EDEX_HOME=$(dirname $dir)
awips_home=$(dirname $EDEX_HOME)
export HOSTNAME=`hostname`
export SHORT_HOSTNAME=`hostname -s`

PYTHON_INSTALL="/awips2/python"
JAVA_INSTALL="/awips2/java"
PSQL_INSTALL="/awips2/psql"
YAJSW_HOME="/awips2/yajsw"

# Source The File With The Localization Information
source ${dir}/setup.env

export JAVA_HOME="${JAVA_INSTALL}"
export PATH=${PSQL_INSTALL}/bin:${JAVA_INSTALL}/bin:${PYTHON_INSTALL}/bin:/awips2/tools/bin:$PATH
export LD_LIBRARY_PATH=${JAVA_INSTALL}/lib:${PYTHON_INSTALL}/lib:${PSQL_INSTALL}/lib:$LD_LIBRARY_PATH
export FXA_DATA=/awips2/fxa/data
export ALLOW_ARCHIVE_DATA="false"

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

WRAPPER_ARGS=""
if [ $DEBUG_FLAG == "on" ]; then
   WRAPPER_ARGS="wrapper.java.debug.port=${EDEX_DEBUG_PORT}"
   echo "To Debug ... Connect to Port: ${EDEX_DEBUG_PORT}."
fi

#create tmp dir
mkdir -p ${AWIPS2_TEMP}

RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Failed to create temp directory ${AWIPS2_TEMP}."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

YAJSW_JVM_ARGS="-Xmx32m -XX:ReservedCodeCacheSize=16m -Djava.io.tmpdir=${AWIPS2_TEMP}"

java ${YAJSW_JVM_ARGS} -jar ${YAJSW_HOME}/wrapper.jar -c ${EDEX_HOME}/conf/${CONF_FILE} ${WRAPPER_ARGS}
