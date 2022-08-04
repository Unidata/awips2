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
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Jul 03, 2019  7875     randerso  Changed to get EDEX version from 
#                                  awips2-version.rpm
##
# edex startup script

if [ -z "${SKIP_RPM_CHECK}" ]; then
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
    
    rpm -q awips2-yajsw > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "ERROR: awips2-yajsw Must Be Installed."
        echo "Unable To Continue ... Terminating."
        exit 1
    fi    
fi

path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

export EDEX_HOME=$(dirname $dir)
awips_home=$(dirname $EDEX_HOME)

# Find the locations of awips2-python and awips2-java.
# only set if the location has not already been exported
if [ -z "$PYTHON_INSTALL" ]; then PYTHON_INSTALL="$awips_home/python"; fi
if [ -z "$JAVA_INSTALL" ]; then JAVA_INSTALL="$awips_home/java"; fi
if [ -z "$PSQL_INSTALL" ]; then PSQL_INSTALL="$awips_home/psql"; fi
if [ -z "$YAJSW_HOME" ]; then YAJSW_HOME="$awips_home/yajsw"; fi

# Find the edex version
version=`rpm -q awips2-version --qf %{VERSION}`
RC=$?
if [ ${RC} -ne 0 ]; then
    version="Undefined"
fi
export EDEX_VERSION=$version

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
export SHORT_HOSTNAME=`hostname -s`

# set Python & Java into the path
export PATH=$awips_home/bin:${JAVA_INSTALL}/bin:${PYTHON_INSTALL}/bin:$PATH

# set Service Backup scripts into the path
export PATH=$PATH:$awips_home/GFESuite/bin:$awips_home/GFESuite/ServiceBackup/scripts

# set AWIPS 1 stuff into path
export PATH=$PATH:$PROJECT/bin

export JAVA_HOME="${JAVA_INSTALL}"
export LD_LIBRARY_PATH=$EDEX_HOME/lib/native/linux32/awips1:${JAVA_INSTALL}/lib:${PYTHON_INSTALL}/lib:${PYTHON_INSTALL}/lib/python3.6/site-packages/jep:${PSQL_INSTALL}/lib:$PROJECT/sharedLib:$LD_LIBRARY_PATH

export FXA_DATA=$EDEX_HOME/data/fxa
export ALLOW_ARCHIVE_DATA="false"

# setup environment for HPE
export AMQP_SPEC=$awips_home/python/share/amqp/amqp.0-10.xml


# get total memory on system in bytes
MEM_IN_MEG=( `free -m | grep "Mem:"` )
export MEM_IN_MEG=${MEM_IN_MEG[1]}
HIGH_MEM=off

if [ $MEM_IN_MEG -gt 12288 ]; then
    HIGH_MEM=on
fi

#-------------------------------------------------------------------------
#read and interpret the command line arguments
#-------------------------------------------------------------------------
CONSOLE_FLAG=on
CONSOLE_LOGLEVEL=DEBUG
DEBUG_FLAG=off
PROFILE_FLAG=off
CONF_FILE="wrapper.conf"
RUN_MODE=
LOG_APPENDERS_CONFIG="logback-edex-appenders-developer.xml"
EDEX_WRAPPER_LOGFILE_FORMAT=M

for arg in $@
do
  case $arg in
    -b|-d|-debug|-db|-bd) DEBUG_FLAG=on;;
    -p|-profiler) PROFILE_FLAG=on;;
    -h|-highmem) HIGH_MEM=on;;
    -noHighmem) HIGH_MEM=off;;
    -noConsole) CONSOLE_FLAG=off;;
    *) RUN_MODE=$arg;;
  esac
done

export EDEX_RUN_MODE=$RUN_MODE
EDEX_WRAPPER_LOGFILE=${EDEX_HOME}/logs/edex-${EDEX_RUN_MODE}-YYYYMMDD.log

if [ $CONSOLE_FLAG == "off" ]; then
    CONSOLE_LOGLEVEL=NONE
    LOG_APPENDERS_CONFIG="logback-edex-appenders.xml"
    EDEX_WRAPPER_LOGFILE=${EDEX_HOME}/logs/edex-${EDEX_RUN_MODE}-wrapper-YYYYMMDD.log
    EDEX_WRAPPER_LOGFILE_FORMAT=LTM
fi

export CONSOLE_LOGLEVEL
export LOG_APPENDERS_CONFIG
export EDEX_WRAPPER_LOGFILE
export EDEX_WRAPPER_LOGFILE_FORMAT


# source environment files
. $EDEX_HOME/etc/default.sh

if [ -e $EDEX_HOME/etc/${RUN_MODE}.sh ]; then
    . $EDEX_HOME/etc/${RUN_MODE}.sh
else
    export DATASTORE_PROVIDER=pypies
fi

if [ $PROFILE_FLAG == "on" ]; then
    . $EDEX_HOME/etc/profiler.sh
fi

# enable core dumps
#ulimit -c unlimited

if [ $DEBUG_FLAG == "off" ]; then
    export EDEX_DEBUG_PORT=-1
else
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

YAJSW_JVM_ARGS="-Xmx32m -Djava.io.tmpdir=${AWIPS2_TEMP}"

java ${YAJSW_JVM_ARGS} -jar ${YAJSW_HOME}/wrapper.jar -c ${EDEX_HOME}/conf/${CONF_FILE}
