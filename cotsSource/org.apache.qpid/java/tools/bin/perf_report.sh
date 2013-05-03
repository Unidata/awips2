#!/bin/sh
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

# This will run the 8 use cases defined below and produce
# a report in tabular format. Refer to the documentation
# for more details.

SUB_MEM=-Xmx1024M
PUB_MEM=-Xmx1024M
LOG_CONFIG="-Damqj.logging.level=WARN"

. setenv.sh

waitfor() { until grep -a -l "$2" $1 >/dev/null 2>&1 ; do sleep 1 ; done ; }
cleanup()
{
  pids=`ps aux | grep java | grep Perf | awk '{print $2}'`
  if [ "$pids" != "" ]; then
    kill -3 $pids
    kill -9 $pids >/dev/null 2>&1
  fi
}

# $1 test name
# $2 consumer options
# $3 producer options
run_testcase()
{
  sh run_sub.sh $LOG_CONFIG $SUB_MEM $2 > sub.out &
  waitfor sub.out "Warming up"
  sh run_pub.sh $LOG_CONFIG $PUB_MEM $3 > pub.out &
  waitfor sub.out "Completed the test"
  waitfor pub.out "Consumer has completed the test"
  sleep 2 #give a grace period to shutdown
  print_result $1  
}

print_result()
{
  prod_rate=`cat pub.out | grep "Producer rate" | awk '{print $3}'`
  sys_rate=`cat sub.out | grep "System Throughput" | awk '{print $4}'`
  cons_rate=`cat sub.out | grep "Consumer rate" | awk '{print $4}'` 
  avg_latency=`cat sub.out | grep "Avg Latency" | awk '{print $4}'`
  min_latency=`cat sub.out | grep "Min Latency" | awk '{print $4}'`
  max_latency=`cat sub.out | grep "Max Latency" | awk '{print $4}'`

  printf "|%-15s|%15.2f|%13.2f|%13.2f|%11.2f|%11d|%11d|\n" $1 $sys_rate $prod_rate $cons_rate $avg_latency $min_latency $max_latency
  echo "------------------------------------------------------------------------------------------------"
}

trap cleanup EXIT

echo "Test report on " `date +%F`
echo "================================================================================================"
echo "|Test           |System throuput|Producer rate|Consumer Rate|Avg Latency|Min Latency|Max Latency|"
echo "------------------------------------------------------------------------------------------------"

# Test 1 Trans Queue
run_testcase "Trans_Queue" "" "-Dwarmup_count=1 -Dmsg_count=10"

# Test 2 Dura Queue
run_testcase "Dura_Queue" "-Ddurable=true" "-Ddurable=true -Dwarmup_count=1 -Dmsg_count=10"

# Test 3 Dura Queue Sync
run_testcase "Dura_Queue_Sync" "-Ddurable=true" "-Ddurable=true -Dwarmup_count=1 -Dmsg_count=10 -Dsync_persistence=true"

# Test 4 Dura Queue Sync Publish and Ack
run_testcase "Dura_SyncPubAck" "-Ddurable=true -Dsync_ack=true" "-Ddurable=true -Dwarmup_count=1 -Dmsg_count=10 -Dsync_publish=persistent"

# Test 5 Topic
run_testcase "Topic" "-DtransDest=transientTopic" "-DtransDest=transientTopic -Dwarmup_count=1 -Dmsg_count=10"

# Test 6 Durable Topic
run_testcase "Dura_Topic" "-Ddurable=true -DtransDest=durableTopic" "-Ddurable=true -DtransDest=durableTopic -Dwarmup_count=1 -Dmsg_count=10"

# Test 7 Fanout
run_testcase "Fanout" "-DtransDest=fanoutQueue" "-DtransDest=fanoutQueue -Dwarmup_count=1 -Dmsg_count=10"

# Test 8 Small TX
run_testcase "Small_Txs_2" "-Ddurable=true -Dtransacted=true -Dtrans_size=1" \
 "-Ddurable=true -Dwarmup_count=1 -Dmsg_count=10 -Dtransacted=true -Dtrans_size=1"

# Test 9 Large TX
run_testcase "Large_Txs_1000" "-Ddurable=true -Dtransacted=true -Dtrans_size=10" \
 "-Ddurable=true -Dwarmup_count=1 -Dmsg_count=10 -Dtransacted=true -Dtrans_size=10"

# Test 10 256 MSG
run_testcase "Msg_256b" "" "-Dmsg_size=256 -Dwarmup_count=1 -Dmsg_count=10"

# Test 11 512 MSG
run_testcase "Msg_512b" "" "-Dmsg_size=512 -Dwarmup_count=1 -Dmsg_count=10"

# Test 12 2048 MSG
run_testcase "Msg_2048b" "" "-Dmsg_size=2048 -Dwarmup_count=1 -Dmsg_count=10"

# Test 13 Random size MSG
run_testcase "Random_Msg_Size" "" "-Drandom_msg_size=true -Dwarmup_count=1 -Dmsg_count=10"

# Test 14 Random size MSG Durable
run_testcase "Rand_Msg_Dura" "-Ddurable=true" "-Ddurable=true -Drandom_msg_size=true -Dwarmup_count=1 -Dmsg_count=10"

# Test 15 64K MSG
run_testcase "Msg_64K" "-Damqj.tcpNoDelay=true" "-Damqj.tcpNoDelay=true -Dmsg_size=64000 -Dwarmup_count=1 -Dmsg_count=10"

# Test 16 Durable 64K MSG
run_testcase "Msg_Durable_64K" "-Ddurable=true -Damqj.tcpNoDelay=true" \
 "-Damqj.tcpNoDelay=true -Dmsg_size=64000 -Ddurable=true -Dwarmup_count=1 -Dmsg_count=10"

# Test 17 500K MSG
run_testcase "Msg_500K" "-Damqj.tcpNoDelay=true" "-Damqj.tcpNoDelay=true -Dmsg_size=500000 -Dwarmup_count=1 -Dmsg_count=10"

# Test 18 Durable 500K MSG
run_testcase "Msg_Dura_500K" "-Damqj.tcpNoDelay=true -Ddurable=true" \
 "-Damqj.tcpNoDelay=true -Dmsg_size=500000 -Ddurable=true -Dwarmup_count=1 -Dmsg_count=10"
