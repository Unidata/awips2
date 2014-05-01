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

# Sample script to run a soak test with MultiThreadedProducer/Consumer.
# You need to provide the log freq and no of iterations
# Ex to run 10 hours and collect 1 second samples 
# soak_report.sh 1 36000

# This script assumes that a suitable broker instance is started.

log_freq=$1
log_iter=$2
shift 2
JVM_ARGS=$@

if [ "$QPID_TEST_HOME" = "" ] ; then
    echo "ERROR: Please set QPID_TEST_HOME ...."
    exit 1
fi

print_rates()
{
 cat $1 | awk '{
 FS = ",";
 count = 0;
 total_latency = 0; 
 min_latency = 9223372036854775807;
 max_latency = 0;

 total_tp = 0;
 min_tp = 50000;
 max_tp = 0;

 while ((getline) == 1) 
 { 
     total_latency = total_latency + $3
     total_tp = total_tp + $2

     if ($3 > 0)
     { 
        min_latency = (($3 < min_latency) ? $3 : min_latency);
        max_latency = (($3 > max_latency) ? $3 : max_latency);
     }
     if ($2 > 0)
     { 
        min_tp = (($2 < min_tp) ? $2 : min_tp);
        max_tp = (($2 > max_tp) ? $2 : max_tp);
     }

     count =  count + 1
 }

 print "Avg Latency (ms) : " total_latency/count
 print "Max Latency (ms) : " max_latency
 print "Min Latency (ms) : " min_latency

 print ""
 print "Avg Throughput (msg/sec) : " total_tp/count
 print "Max Throughput (msg/sec) : " max_tp
 print "Min Throughput (msg/sec) : " min_tp

 print ""
 print "Total Iteratons " count

 }'
}

print_system_stats()
{
 cat $1 | awk '{
 FS = ",";
 count = 0;
 total_memory = 0;
 min_memory = 9223372036854775807;
 max_memory = 0;

 total_cp = 0;
 min_cp = 50000;
 max_cp = 0;

 while ((getline) == 1) 
 { 
     total_memory = total_memory + $2
     total_cp = total_cp + $3

     if ($2 > 0)
     { 
        min_memory = (($2 < min_memory) ? $2 : min_memory);
        max_memory = (($2 > max_memory) ? $2 : max_memory);
     }
     if ($3 > 0)
     { 
        min_cp = (($3 < min_cp) ? $3 : min_cp);
        max_cp = (($3 > max_cp) ? $3 : max_cp);
     }

     count =  count + 1
 }

 print "Avg Memory (MB) : " total_memory/(count*1024)
 print "Max Memory (MB) : " max_memory/1024
 print "Min Memory (MB) : " min_memory/1024

 print ""
 print "Avg CPU         : " total_cp/count
 print "Max CPU         : " max_cp
 print "Min CPU         : " min_cp

 print ""
 print "Total Iteratons " count

 }'
}


cleanup()
{
  kill -9 `ps aux | grep java | grep soak | awk '{ print $2 }'`
}

print_results()
{  
  printf "\n======================================================= \n" 
  print_rates MultiThreadedConsumer_1.log
  printf "\nConsumer process stats "
  printf "\n----------------------- \n"
  print_system_stats MultiThreadedConsumer_process_1.log
  printf "\nProducer process stats "
  printf "\n----------------------- \n"
  print_system_stats MultiThreadedProducer_process_1.log
  printf "\n------------------------------------------------------- \n"
}

trap cleanup EXIT

# runs a single instance of the MultiThreadedConsumer and MultiThreadedProducer
sh $QPID_TEST_HOME/bin/run_soak_client.sh 1 $log_freq $log_iter org.apache.qpid.testkit.soak.MultiThreadedConsumer $JVM_ARGS
sh $QPID_TEST_HOME/bin/run_soak_client.sh 1 $log_freq $log_iter org.apache.qpid.testkit.soak.MultiThreadedProducer $JVM_ARGS

sleep_time=$((log_freq * log_iter))
echo "sleep time : " $sleep_time
sleep $((log_freq * log_iter))

print_results
