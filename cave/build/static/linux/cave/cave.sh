#!/bin/bash
# CAVE startup script
# Note: CAVE will not run as 'root'

user=`/usr/bin/whoami`
if [ ${user} == 'root' ];then
   echo "WARNING: CAVE cannot be run as user '${user}'!"
   echo "         change to another user and run again."
   exit 1
fi

path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

source ${dir}/caveUtil.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: unable to find and/or access ${dir}/caveUtil.sh."
   exit 1
fi

# Run monitorThreads?
runMonitorThreads=false

# copy the viz shutdown utility if necessary.
copyVizShutdownUtilIfNecessary

# delete any old disk caches in the background
deleteOldCaveDiskCaches &

# Since, we no longer need to worry about re-location ...
CAVE_INSTALL="/awips2/cave"
JAVA_INSTALL="/awips2/java"
PYTHON_INSTALL="/awips2/python"

export AWIPS_INSTALL_DIR="${CAVE_INSTALL}"

export LD_LIBRARY_PATH=${JAVA_INSTALL}/lib:${PYTHON_INSTALL}/lib:$LD_LIBRARY_PATH
export LD_PRELOAD=libpython.so
extendLibraryPath
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

hostName=`hostname -s`
BASE_LOGDIR=$HOME/caveData/logs/consoleLogs
LOGDIR=$BASE_LOGDIR/$hostName/


# make sure directory exists
if [ ! -d $LOGDIR ]; then
 mkdir -p $LOGDIR
fi

# delete any old disk caches in the background
deleteOldCaveLogs &

export pid=$$

curTime=`date +%Y%m%d_%H%M%S`
LOGFILE=${LOGDIR}/cave_${curTime}_pid_${pid}_console.log
export LOGFILE_CAVE=${LOGDIR}/cave_${curTime}_pid_${pid}_alertviz.log
export LOGFILE_PERFORMANCE=${LOGDIR}/cave_${curTime}_pid_${pid}_perf.log

redirect="TRUE"
for flag in $@; do
	if [ $flag == "-noredirect" ]; then
		redirect="FALSE"
		break
	fi
done

# can we write to log directory
if [ -w ${LOGDIR} ]; then
  touch ${LOGFILE}
fi

# Special instructions for the 64-bit jvm.
ARCH_ARGS=""
if [ -f /awips2/java/jre/lib/amd64/server/libjvm.so ]; then
   ARCH_ARGS="-vm /awips2/java/jre/lib/amd64/server/libjvm.so"
fi

lookupINI $@

if [[ "${runMonitorThreads}" == "true" ]] ; then 
  # nohup to allow tar process to continue after user has logged out
  nohup ${dir}/monitorThreads.sh $pid >> /dev/null 2>&1 &
fi

if ( [ ${redirect} == "TRUE" ] ); then 
  exec ${dir}/cave ${ARCH_ARGS} ${SWITCHES} ${CAVE_INI_ARG} $@ > ${LOGFILE} 2>&1
else
  exec ${dir}/cave ${ARCH_ARGS} ${SWITCHES} ${CAVE_INI_ARG} $@ 2>&1 | tee ${LOGFILE}
fi

