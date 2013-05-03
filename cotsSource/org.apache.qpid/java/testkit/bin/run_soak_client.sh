#!/bin/bash
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

# This is a sample script for a soak test on
# linux environment.
# This will start n of Producers processors and record their CPU and memory stats.
# Also the Producer out will be saved to a file as well.

if [ "$JAR_PATH" = "" ] ; then
    echo "ERROR: Please set JAR_PATH to point to the Qpid libraries ...."
    exit 1
fi

#1 PID, $2 freq, $3 count
calc_stats(){

for ((  i = 0 ;  i <= $3; i++  ))
 do
     cpu=`ps auxw | grep $1 | grep -v 'grep' | awk '{print $3}'`
     mem=`pmap $1 | grep total | grep -v 'grep' | awk '{print substr($2,0,length($2)-1)}'`
     echo $i","$mem","$cpu
     sleep $2
     cpu="0.0"
     mem="0"
 done
 kill -9 $1
}

# Num of producer processors to start
num=$1
# Log frequency in seconds
log_freq=$2
# Num of iterations
log_iter=$3

class_name=$4
log_file_name=`echo $class_name | cut -d. -f6`

# The total time for the test is determined by the
# log_freq * log_iter.

shift 4
CLASSPATH=`find $JAR_PATH -name '*.jar' | tr '\n' ":"`

JVM_ARGS="-Xmx1500M $@"
echo "Starting $log_file_name with the following params $JVM_ARGS"

for ((  c = 1 ;  c <= $num;  c++  ))
do
  $JAVA_HOME/bin/java $JVM_ARGS -cp $CLASSPATH $class_name > ${log_file_name}_${c}.log &
  pid=`jobs -l %% | awk '{print $2}'`
  calc_stats $pid $log_freq $log_iter > ${log_file_name}_process_${c}.log &
done
