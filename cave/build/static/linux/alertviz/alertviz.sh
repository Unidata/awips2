#!/bin/bash

# Alert VIZ Startup Script
# Note: Alert VIZ will not run as 'root'

user=`/usr/bin/whoami`
if [ ${user} == 'root' ]; then
   echo "WARNING: Alert VIZ cannot be run as user '${user}'!"
   echo "         change to another user and run again."
   exit 1
fi

# We will no longer be using hard-coded paths that need to be replaced.
# Use rpm to find the paths that we need.
JAVA_INSTALL="/awips2/java"
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: awips2-java Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
PYTHON_INSTALL="/awips2/python"
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: awips2-python Must Be Installed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
ALERTVIZ_INSTALL="/awips2/alertviz"

path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

export AWIPS_INSTALL_DIR=${ALERTVIZ_INSTALL}

export LD_LIBRARY_PATH=${JAVA_INSTALL}/lib:${PYTHON_INSTALL}/lib:$LD_LIBRARY_PATH
export LD_PRELOAD=${PYTHON_INSTALL}/lib/libpython2.7.so
export PATH=${JAVA_INSTALL}/bin:${PYTHON_INSTALL}/bin:$PATH
export JAVA_HOME="${JAVA_INSTALL}/jre"

exitVal=1

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


#check for the logs directory, which may not be present at first start
hostName=`hostname -s`
LOGDIR=$HOME/caveData/logs/consoleLogs/$hostName/

if [ ! -d $LOGDIR ]; then
 mkdir -p $LOGDIR
fi

# Special instructions for the 64-bit jvm.
ARCH_ARGS=""
if [ -f /awips2/java/jre/lib/amd64/server/libjvm.so ]; then
   ARCH_ARGS="-vm /awips2/java/jre/lib/amd64/server/libjvm.so"
fi

#run a loop for alertviz
count=0
while [ $exitVal -ne 0 -a $count -lt 10 ]
do
 count=`expr $count + 1`
 curTime=`date +%Y%m%d_%H%M%S`
 LOGFILE=${LOGDIR}/alertviz_${curTime}_console.log
 export LOGFILE_ALERTVIZ=${LOGDIR}/alertviz_${curTime}_admin.log

 #first check if we can write to the directory
 if [ -w ${LOGDIR} ]; then
  touch ${LOGFILE}
 fi

 #check for display; if no display then exit
 if [ -z "${DISPLAY}" ]; then
  echo "Display is not available."
  exitVal=0
 else
  #finally check if we can write to the file
  if [ -w ${LOGFILE} ]; then
   ${dir}/alertviz ${ARCH_ARGS} $*  > ${LOGFILE} 2>&1
  else
   ${dir}/alertviz ${ARCH_ARGS} $*
  fi
 fi
 exitVal=$?
done

