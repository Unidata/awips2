#!/bin/sh
#####################################################################
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
#####################################################################
#####################################################################
# Script for capturing data from a wrapper java process when the
# wrapper restarts the process
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer    Description
# ------------- -------- ----------- --------------------------
# Aug 07, 2014  3470     rjpeter     Initial creation
#
#####################################################################
# NOTE: Script must be located at /awips2/qpid/bin/yajsw/scripts for it to work

# base path to save capture data to, will create subdirectory for each server
basePath="/data/fxa/cave"

state=$1
string_state=$2
pid=$4

path_to_script=`readlink -f $0`
curTime=`date +%Y%m%d_%H%M%S`
echo "$curTime: Wrapper running $path_to_script due to state transition for pid $pid.  New State $state|$string_state"

# ensure directory is created and has write permissions
checkDir() {
   dir="$1"
   if [ ! -d "$dir" ]; then
      mkdir -p $dir
      if [ ! -d "$dir" ]; then
         message="Unable to create qpid capture data directory\n$dir"
         echo -e "Capture failed: $message"
         exit 1
      fi
   fi

   if [ ! -w "$dir" ]; then
      message="Do not have write permissions to qpid capture data directory\n$dir"
      echo -e "Capture failed: $message"
      exit 1
   fi
}

# gets top output of local server
runTop() {
   local curTime=`date "+%Y%m%d_%H:%M:%S"`
   echo "$curTime: Capturing top"
   echo "$curTime: Capturing top" >> $processFile
   local out_file="${dataPath}/top.log"
   export COLUMNS=160
   top -b -c -n1 >> $out_file 2>&1
   curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: top captured"
}

# runs jstack 10 times, if it fails will run again with -F
runJstack() {
   local curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: Capturing jstacks"
   local pid="$1"
   local count=1
   local cmd="/awips2/java/bin/jstack"
   local prePath="${dataPath}/pid_${pid}_"
   local log=""

   while [ "$count" -le "10" ]; do
      curTime=`date "+%Y%m%d_%H:%M:%S"`
      log="${prePath}jstack_${count}.log"

      echo "${curTime}: Running command: ${cmd} ${pid} >> ${log} 2>&1" >> $processFile
      echo "Running for $curTime" >> $log
      ${cmd} ${pid} >> ${log} 2>&1

      if [[ "$?" != "0" && $FORCE != "y" ]]; then
         curTime=`date "+%Y%m%d_%H:%M:%S"`
         echo "${curTime}: jstack for $pid failed to connect, rerunning with -F" >> $processFile
         ${cmd} -F ${pid} >> ${log} 2>&1
      fi
      let "count+=1"
   done

   curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: jstacks captured"
}

# runs jmap -heap
runJmapHeap() {
   local curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: Capturing jmap -heap"
   local pid=$1
   local prePath="${dataPath}/pid_${pid}_"

   local log="${prePath}jmapHeap.log"
   local cmd="/awips2/java/bin/jmap -heap"
   echo "${curTime}: Running command: $cmd $pid >> $log 2>&1" >> $processFile
   $cmd $pid >> $log 2>&1

   if [[ "$?" != "0" && $FORCE != "y" ]]; then
      curTime=`date "+%Y%m%d_%H:%M:%S"`
      echo "${curTime}: jmap for $pid failed to connect, rerunning with -F" >> $processFile
      $cmd -F $pid >> $log 2>&1
   fi

   curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: jmap -heap captured"
}

# runs jmap, if it fails will run again with -F
runJmap() {
   local curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: Capturing jmap -dump"
   local pid=$1
   local prePath="${dataPath}/pid_${pid}_jmap"

   local log="${prePath}.log"
   local dumpPath="${prePath}.hprof"
   local cmd="/awips2/java/bin/jmap -dump:format=b,file=${dumpPath}"
   echo "${curTime}: Running command: $cmd $pid >> $log 2>&1" >> $processFile
   $cmd $pid >> $log 2>&1

   if [[ "$?" != "0" && $FORCE != "y" ]]; then
      curTime=`date "+%Y%m%d_%H:%M:%S"`
      echo "${curTime}: jmap for $pid failed to connect, rerunning with -F" >> $processFile
      $cmd -F $pid >> $log 2>&1
   fi

   curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: jmap -dump captured"
}



if [[ "$pid" != "-1" ]]; then
   process=`ps -ef | grep $pid | grep java`

   if [[ "$process" != "" ]]; then
   hostName=`hostname -s`
   dataPath="${basePath}/${hostName}/wrapperCaptureData_${curTime}_pid_$pid"
   checkDir $dataPath
   processFile=${dataPath}/capture_info.log
   echo "Wrapper running $0 due to state transition for pid $pid.  New State $state|$string_state" >> $processFile
   echo "Process information:" >> $processFile
   ps -ef | grep $pid >> $processFile
   runTop &
   runJstack $pid &
   runJmapHeap $pid &
   # TODO: Double check if jvm already dumped one
   runJmap $pid &
   wait

   curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: Data captured to $dataPath"
   else
      curTime=`date +%Y%m%d_%H%M%S`
      echo "$curTime: PID $pid is no longer running, nothing to capture"
   fi
else
   curTime=`date +%Y%m%d_%H%M%S`
   echo "$curTime: PID was -1, process no longer running, nothing to capture"
fi
