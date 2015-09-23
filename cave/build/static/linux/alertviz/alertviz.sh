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
# Jun 17, 2015  #4148     rferrel     Logback needs fewer environment variables.
# Jul 23, 2015  ASM#13849 D. Friedman Use a unique Eclipse configuration directory
# Aug 03, 2015  #4694     dlovely     Logback will now add user.home to LOGDIR
# Sep 17, 2015  #4869     bkowal      Read dynamic AlertViz version information at startup.
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

export LD_LIBRARY_PATH=${JAVA_INSTALL}/lib:$LD_LIBRARY_PATH
export PATH=${JAVA_INSTALL}/bin:$PATH
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
# Logback configuration files will append user.home to LOGDIR.
export LOGDIR=caveData/logs/consoleLogs/$hostName/
FULL_LOGDIR=$HOME/$LOGDIR

if [ ! -d $FULL_LOGDIR ]; then
 mkdir -p $FULL_LOGDIR
fi

SWITCHES=()

function deleteEclipseConfigurationDir()
{
    if [[ -n $eclipseConfigurationDir ]]; then
        sleep 2
        rm -rf "$eclipseConfigurationDir"
    fi
}

function createEclipseConfigurationDir()
{
    local d dir id=$(hostname)-$(whoami)
    for d in "/local/cave-eclipse/" "$HOME/.cave-eclipse/"; do
        if [[ $d == $HOME/* ]]; then
            mkdir -p "$d" || continue
        fi
        if dir=$(mktemp -d --tmpdir="$d" "${id}-XXXX"); then
            eclipseConfigurationDir=$dir
            trap deleteEclipseConfigurationDir EXIT
            SWITCHES+=(-configuration "$eclipseConfigurationDir")
            return 0
        fi
    done
    echo "Unable to create a unique Eclipse configuration directory.  Will proceed with default." >&2
    return 1
}

createEclipseConfigurationDir

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

VERSION_ARGS=()
if [ -f ${dir}/awipsVersion.txt ]; then
   prevIFS=${IFS}
   IFS=$'\n'
   for line in `cat ${dir}/awipsVersion.txt`; do
      VERSION_ARGS+=(${line})
   done
   IFS=${prevIFS}
fi

#run a loop for alertviz
count=0
while [ $exitVal -ne 0 -a $count -lt 10 ]
do
  count=`expr $count + 1`
  #check for display; if no display then exit
  if [ -z "${DISPLAY}" ]; then
   echo "Display is not available."
   exitVal=0
  else
    if [ -w $FULL_LOGDIR ] ; then
        ${dir}/alertviz "${SWITCHES[@]}" "${VERSION_ARGS[@]}" $* > /dev/null 2>&1 &
    else
        ${dir}/alertviz "${SWITCHES[@]}" "${VERSION_ARGS[@]}" $* &
    fi
  pid=$!
  wait $pid
  exitVal=$?
 fi
done

