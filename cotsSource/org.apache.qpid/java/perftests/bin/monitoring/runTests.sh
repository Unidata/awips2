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
# Run specified performance tests and 
# gather details about the test run
#


runTest()
{
 echo "$@"
 echo "$@ --csv -o $LOG_DIR" >> $LOG_DIR/TestRun.log 2>&1
 ./$@ --csv -o $LOG_DIR >> $LOG_DIR/TestRun.log 2>&1
}

showUsageExit()
{
  echo "Usage $0 <Path to Test Pack> <LOG DIR> <TEST LIST FILE>"
  exit 1
}

# Ensure we have minimum of three arguments
if [[ $# > 2 ]] ; then
  TEST_VERSION=$1 
  LOG_DIR=$2
  TEST_LIST=$3
  # Remove these arguments from the $@ variable
  shift 
  shift
  shift
else
  showUsageExit
fi

#
# Check the specified broker is reachable
# it it is not the log and show usage
#
if [ ! -d $TEST_VERSION ] ; then
  echo "Tests not available at: $TEST_VERSION"
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
# Check to see if we have an absolute path for test list
#
testListStart=`echo $TEST_LIST|cut -c 1`

#
# If we don't have an absolute path then add the current 
# directory path to the start.
#
if [[ $testListStart != '/' ]] ; then
 echo -n "$TEST_LIST is not absolute, using "
 TEST_LIST=`pwd`/$TEST_LIST
 echo $TEST_LIST
fi

#
# Validate that the directory does not exist
#  - this is so we can guarrantee a clean run.
# If it does exit then log and show usage
#
# -r Check file exists and is readable
if [ ! -r $TEST_LIST ] ; then
  echo "Test file is not readable : $TEST_LIST"
  showUsageExit
fi



#
# Create the logging directory
#
mkdir -p $LOG_DIR



echo "Starting Test Run in : $TEST_VERSION"
pushd $TEST_VERSION/bin > /dev/null

#
# Run tests
#


while read testCommand
do
 runTest $testCommand
done < "$TEST_LIST"


popd > /dev/null


#
# Generate Stat files
#
echo "Generating Stat data"
for file in `find $LOG_DIR -name "*.csv"` ; do
  stat $file > $file.stat
done
