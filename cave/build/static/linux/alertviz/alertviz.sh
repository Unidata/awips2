#!/bin/bash

# Alert VIZ Startup Script
# Note: Alert VIZ will not run as 'root'

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
# Oct 09, 2014  #3675     bclement    added cleanExit signal trap
#

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

# takes in a process id
# kills spawned subprocesses of pid
# and then kills the process itself and exits
function cleanExit()
{
    pid=$1
    if [[ -n $pid ]]
    then
        pkill -P $pid
        kill $pid
    fi
    exit
}

trap 'cleanExit $pid' SIGHUP SIGINT SIGQUIT SIGTERM

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
   ${dir}/alertviz $*  > ${LOGFILE} 2>&1 &
  else
   ${dir}/alertviz $* &
  fi
  pid=$!
  wait $pid
  exitVal=$?
 fi
done

