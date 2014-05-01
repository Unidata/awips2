#!/bin/bash
#
# 
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
#   http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
# 
#

#
# This script starts a broker and then starts additional logging as required.
#  *.pid files are generated in the LOG_DIR for later use by the stop-monitored-broker 
#  script. 
# 
# Currently this process starts:
#   - The broker with additional QPID_OPTS for gc logging
#   - Top to monitoring the CPU usage
#
# Additional processes can be started and as long as they write a PID into LOG_DIR/*.pid
# it will be shutdown with the stop script
#

#
# Output the broker log file to aid problem diagnosis
# then exit.
#
brokerFailExit()
{
   echo "Broker failed to start up."
   cat $BROKER_LOG
   exit 1 
}

showUsageExit()
{
  echo "Usage $0 <Path to Test Broker> <LOG DIR> <CPU Monitor Rate (s)> [Additional options to
  pass to Qpid broker startup]"
  exit 1
}

#
# Perform 3 attempts to get the broker PID via ps and grep
# if unable the output broker log and exit
#
getBrokerPID()
{
 attempts=3
 ready=0
 while [ $ready == 0 ] ; do
 
  PID=`ps auxwww| grep java | grep Xloggc | awk '{print $2}'`

  if [ ! $PID == 0 ] ; then
   ready=1 
  else  
   attempts=$[ $attempts - 1 ]
 
   if [ $attempts == 0 ] ; then
     brokerFailExit
   fi
   
   sleep 1
  fi
 done

}


#
# Additional Check to ensure that the broker process
# has correctly written 'Ready' to the log file.
#
checkBrokerStarted()
{
 attempts=3
 ready=0
 while [ $ready == 0 ] ; do
 
  grep Ready $BROKER_LOG > /dev/null

  if [ $? == 0 ] ; then
   ready=1 
  else  
   attempts=$[ $attempts - 1 ]
 
   if [ $attempts == 0 ] ; then
     brokerFailExit
   fi
   
   echo "Broker not ready sleeping 1s"
   sleep 1
  fi
 done
}

#
# Command Line setup
#

# Ensure we have minimum of three arguments
if [[ $# > 2 ]] ; then
  BROKER_VERSION=$1 
  LOG_DIR=$2
  CPU_MONITOR_RATE=$3
  # Remove these arguments from the $@ variable
  shift 
  shift 
  shift
else
  # If we have no arguments then use these as the default  
  CPU_MONITOR_RATE=0.5
  LOG_DIR=$QPID_WORK/logging
  BROKER_VERSION=qpid-0.5
fi


#
# Check the specified broker is reachable
# it it is not the log and show usage
#
if [ ! -d $BROKER_VERSION ] ; then
  echo "Broker not available at: $BROKER_VERSION"
  showUsageExit
fi

#
# Check to see if we have an absolute path for logging
#
logStart=`echo $LOG_DIR|cut -c 1`


#
# If we don't have an absolute path then add the current 
# directory path to the start.
#
if [[ $logStart != '/' ]] ; then
 echo -n "$LOG_DIR is not absolute, using "
 LOG_DIR=`pwd`/$LOG_DIR
 echo $LOG_DIR
fi

#
# Validate that the directory does not exist
#  - this is so we can guarrantee a clean run.
# If it does exit then log and show usage
#
if [ -d $LOG_DIR ] ; then
  echo "Log directory already exists : $LOG_DIR"
  showUsageExit
fi

#
# Create the logging directory
#
mkdir -p $LOG_DIR

#
# Variable for broker log 
#
BROKER_LOG=$LOG_DIR/broker.log

# Variable to hold broker PID
PID=0

export QPID_OPTS="-Xloggc:$LOG_DIR/gc.log -verbose:gc -XX:+PrintGCDetails -XX:+PrintGCTimeStamps" 

#
# Start Qpid Broker
#
echo "Starting Broker : $BROKER_VERSION"
pushd $BROKER_VERSION/bin > /dev/null
./qpid-server $@ 2> $BROKER_LOG >&2 &
popd > /dev/null

# Wait and check startup was ok
echo "Waiting for broker startup"
getBrokerPID

checkBrokerStarted

echo $PID > $LOG_DIR/broker.pid

# 
# Start CPU Monitoring via TOP
#
echo "Starting CPU Monitor at RATE:$CPU_MONITOR_RATE on $SERVER1"
pushd $LOG_DIR > /dev/null

echo $CPU_MONITOR_RATE > top.rate 

top -d $CPU_MONITOR_RATE -S -c -p $PID -b > broker_cpu.log &

#
# Get top pid using $!
#
echo $! > $LOG_DIR/top.pid

popd > /dev/null


#
# Generate Stat files
#
echo "Generating Stat data"
stat $BROKER_LOG > $BROKER_LOG.stat
stat $LOG_DIR/broker_cpu.log > $LOG_DIR/broker_cpu.log.stat
stat $LOG_DIR/gc.log > $LOG_DIR/gc.log.stat

