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
# Jan 24, 2014  #2739     bsteffen    Log exit status
# Jan 30, 2014  #2593     bclement    warns based on memory usage, fixed for INI files with spaces
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

MAX_MEM_PROPORTION="0.9"

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

# Enable core dumps
ulimit -c unlimited

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

# check number of running caves
if [[ -z $IGNORE_NUM_CAVES ]]; then
   # get total memory on system in bytes
   mem=( `free -b | grep "Mem:"` )
   mem=${mem[1]}
   # get max amount of system memory used before we warn
   memThreshold=$(echo "$mem * $MAX_MEM_PROPORTION" | bc)
   # remove decimal
   printf -v memThreshold "%.0f" "$memThreshold"
   # get launcher.ini argument determined by user arguments
   lookupINI "$@"
   launcherRegex='--launcher.ini\s(.+\.ini)'
   # default to cave.ini
   targetIni="/awips2/cave/cave.ini"
   if [[ $CAVE_INI_ARG =~ $launcherRegex ]]
   then
        targetIni="${BASH_REMATCH[1]}"
   fi
   # read max memory that could be used by this instance
   memOfLaunchingCave=$(readMemFromIni "$targetIni")
   # read total max memory of caves already running
   getTotalMemOfRunningCaves
   # add them together
   _totalAfterStart=$(($memOfLaunchingCave + $_totalRunningMem))
   if [[ "$_totalAfterStart" -ge "$memThreshold" ]]; then
      # convert to megs for display
      memOfLaunchingCave=$(($memOfLaunchingCave / $BYTES_IN_MB))
      _totalRunningMem=$(($_totalRunningMem / $BYTES_IN_MB))
      getPidsOfMyRunningCaves
      memMsg="$_numPids CAVE applications already running with a combined max memory of ${_totalRunningMem}MB. "
      memMsg+="The requested application has a max memory requirement of ${memOfLaunchingCave}MB. "
      memMsg+="Starting may impact system performance and stability.\n\nProceed?"
      zenity --question --title "Low Available Memory for Application"  --text "$memMsg"
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

BASE_LOGDIR=$HOME/caveData/logs/consoleLogs
LOGDIR=$BASE_LOGDIR/$hostName/


# make sure directory exists
if [ ! -d $LOGDIR ]; then
 mkdir -p $LOGDIR
fi

# delete any old disk caches in the background
deleteOldCaveLogs &

curTime=`date +%Y%m%d_%H%M%S`

# At this point fork so that log files can be set up with the process pid and
# this process can log the exit status of cave.
(
  export pid=`/bin/bash -c 'echo $PPID'`

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
    exec ${CAVE_INSTALL}/cave ${ARCH_ARGS} ${SWITCHES} "${CAVE_INI_ARG}" "${USER_ARGS[@]}" > ${LOGFILE} 2>&1
  else
    exec ${CAVE_INSTALL}/cave ${ARCH_ARGS} ${SWITCHES} "${CAVE_INI_ARG}" "${USER_ARGS[@]}" 2>&1 | tee ${LOGFILE}
  fi
) &

pid=$!
LOGFILE="${LOGDIR}/${PROGRAM_NAME}_${curTime}_pid_${pid}_console.log"
logExitStatus $pid $LOGFILE


