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
# Dec 05, 2013  #2593     rjpeter     Added check for number of running
#                                     cave sessions.
# Dec 05, 2013  #2590     dgilling    Modified so gfeclient.sh can be wrapped
#                                     around this script.
#
#


user=`/usr/bin/whoami`
if [ ${user} == 'root' ];then
   echo "WARNING: CAVE cannot be run as user '${user}'!"
   echo "         change to another user and run again."
   exit 1
fi

# Since, we no longer need to worry about re-location ...
CAVE_INSTALL="/awips2/cave"
JAVA_INSTALL="/awips2/java"
PYTHON_INSTALL="/awips2/python"
export AWIPS_INSTALL_DIR="${CAVE_INSTALL}"

source ${CAVE_INSTALL}/caveUtil.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: unable to find and/or access ${CAVE_INSTALL}/caveUtil.sh."
   exit 1
fi

# Run monitorThreads?
runMonitorThreads=false

# copy the viz shutdown utility if necessary.
copyVizShutdownUtilIfNecessary

# delete any old disk caches in the background
deleteOldCaveDiskCaches &

export LD_LIBRARY_PATH=${JAVA_INSTALL}/lib:${PYTHON_INSTALL}/lib:$LD_LIBRARY_PATH
export LD_PRELOAD=libpython.so
if [[ -z "$CALLED_EXTEND_LIB_PATH" ]]; then
    extendLibraryPath
fi
export PATH=${JAVA_INSTALL}/bin:${PYTHON_INSTALL}/bin:$PATH
export JAVA_HOME="${JAVA_INSTALL}/jre"

# The user can update this field if they choose to do so.
export HYDRO_APPS_DIR="/awips2/edex/data/share/hydroapps"

export EDEX_HOME=/awips2/edex 
export LOCALIZATION_ROOT=~/caveData/common
export PGSQL_DRIVER_DIR=`ls -1d /awips2/cave/plugins/org.postgres_*` 
if [ $? -ne 0 ]; then
   echo "FATAL: Unable to locate the PostgreSQL JDBC Driver."
   exit 1
fi
export apps_dir=${HYDRO_APPS_DIR} 

TESTCHECK="$TMCP_HOME/bin/getTestMode"
if [ -x ${TESTCHECK} ]; then
    echo "Calling getTestMode()"
    ${TESTCHECK}
    status=${?}
    if [ $status -eq 11 ]; then
        MODE="TEST"
        SWITCHES="${SWITCHES} -mode TEST "
    elif [ $status -eq 12 ];then
        MODE="PRACTICE"
        SWITCHES="${SWITCHES} -mode PRACTICE "
    elif [ $status -eq 15 ];then
        MODE="OPERATIONAL"
        SWITCHES="${SWITCHES} -mode OPERATIONAL"
    else
        MODE="OPERATIONAL (no response)"
        SWITCHES="${SWITCHES} "
    fi
    echo "getTestMode() returned ${MODE}"
else
    MODE="UNKNOWN"
    echo "getTestMode() not found - going to use defaults"
fi

export TEXTWS=`hostname | sed -e 's/lx/xt/g'`

hostName=`hostname -s`

if [[ $hostName =~ xt.* ]]; then
   export IGNORE_NUM_CAVES=1
fi

# check number of running caves
if [[ -z $IGNORE_NUM_CAVES ]]; then
   # free usually reports below on G threshold (11 instead of 12G), giving the 3 cave recommended in field
   mem=( `free -g | grep "Mem:"` )
   mem=${mem[1]}
   let _maxCaves=mem/3

   getPidsOfMyRunningCaves
   if [[ "$_numPids" -ge "$_maxCaves" ]]; then
      zenity --question --title "Max CAVE sessions already running"  --text "$_numPids CAVE sessions already running. Starting more may impact system performance and stability.\n\nProceed?"
      cancel="$?"

      if [[ "$cancel" == "1" ]]; then
         exit
      fi
   fi
fi

#check for gtk-2.0 value
gtkResource=.gtkrc-2.0
includeLine="include \"$HOME/.gtkrc.mine\""
mineFile=.gtkrc.mine
altButtonLine="gtk-alternative-button-order=1"
if [ -f $HOME/$gtkResource ]; then
   if [ -w $HOME/$gtkResource ]; then
      var=`grep "gtkrc.mine" $HOME/$gtkResource`
      if [ '' == "$var" ]; then
         echo $includeLine >> $HOME/$gtkResource
      fi
   fi
else
    touch $HOME/$gtkResource
    echo $includeLine >> $HOME/$gtkResource
fi

if [ -f $HOME/$mineFile ]; then
   if [ -w $HOME/$mineFile ]; then
      var=`grep "alternative-button-order" $HOME/$mineFile`
      if [ '' == "$var" ]; then
         echo $altButtonLine >> $HOME/$mineFile
      fi
   fi
else
   touch $HOME/$mineFile
   echo $altButtonLine >> $HOME/$mineFile
fi

if [[ -z "$PROGRAM_NAME" ]]
then
    PROGRAM_NAME="cave"
fi

LOGDIR="$HOME/caveData/logs/consoleLogs/$hostName/"

# make sure directory exists
if [ ! -d $LOGDIR ]; then
 mkdir -p $LOGDIR
fi

export pid=$$

curTime=`date +%Y%m%d_%H%M%S`
LOGFILE="${LOGDIR}/${PROGRAM_NAME}_${curTime}_pid_${pid}_console.log"
export LOGFILE_CAVE="${LOGDIR}/${PROGRAM_NAME}_${curTime}_pid_${pid}_alertviz.log"
export LOGFILE_PERFORMANCE="${LOGDIR}/${PROGRAM_NAME}_${curTime}_pid_${pid}_perf.log"

# can we write to log directory
if [ -w ${LOGDIR} ]; then
  touch ${LOGFILE}
fi

# remove "-noredirect" flag from command-line if set so it doesn't confuse any
# commands we call later.
redirect="true"
USER_ARGS=()
while [[ $1 ]]
do
    if [[ "$1" == "-noredirect" ]]
    then
        redirect="false"
    else
        USER_ARGS+=("$1")
    fi
    shift
done

# Special instructions for the 64-bit jvm.
ARCH_ARGS=""
if [ -f /awips2/java/jre/lib/amd64/server/libjvm.so ]; then
   ARCH_ARGS="-vm /awips2/java/jre/lib/amd64/server/libjvm.so"
fi

lookupINI "${USER_ARGS[@]}"

if [[ "${runMonitorThreads}" == "true" ]] ; then 
  # nohup to allow tar process to continue after user has logged out
  nohup ${CAVE_INSTALL}/monitorThreads.sh $pid >> /dev/null 2>&1 &
fi

if [[ "${redirect}" == "true" ]] ; then 
  exec ${CAVE_INSTALL}/cave ${ARCH_ARGS} ${SWITCHES} ${CAVE_INI_ARG} "${USER_ARGS[@]}" > ${LOGFILE} 2>&1
else
  exec ${CAVE_INSTALL}/cave ${ARCH_ARGS} ${SWITCHES} ${CAVE_INI_ARG} "${USER_ARGS[@]}" 2>&1 | tee ${LOGFILE}
fi

