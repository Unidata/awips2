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

cleanup(){
  echo "kill any existing broker instance"
  stopBroker
  rm -rf $CC_HOME/jmstck-data/*
}

runBroker(){
  echo "******************************************************"
  echo "Starting C++ broker"
  ulimit -c unlimited
  $CC_HOME/cpp/src/qpidd -t -d --data-dir $CC_HOME/jmstck-data --load-module=$CPPSTORE_HOME/lib/.libs/libbdbstore.so --port 0 --auth no --log-output $CC_HOME/jmstck-broker.log --no-module-dir
  export QPID_PORT=`grep "Listening on TCP port" $CC_HOME/jmstck-broker.log | tail -n 1 | awk '{print $8}'`
  echo " broker running on port: " $QPID_PORT
  echo "******************************************************"
  sed "s/qpid_port/$QPID_PORT/g"  $CC_HOME/cc/config/java/jndi.properties > "$TS_HOME/classes"/jndi.properties
}

runTck(){
  echo "******************************************************"
  echo "Starting the TCK for the $1 iteration"
  echo "******************************************************"
  cd $TS_HOME/bin
  $TS_HOME/bin/tsant runclient -Dwork.dir=work -Dreport.dir=report 2&>1 > $TS_HOME/tck$1.log
  echo "******************************************************"
  echo "TCK finished the $1 iteration"
  echo "******************************************************"
}

printResults(){
  TESTS_STR=`grep -a "\[java\] Completed running [0-9]* tests" $TS_HOME/tck$1.log`
  PASSED_STR=`grep -a "\[java\] Number of Tests Passed =" $TS_HOME/tck$1.log`
  FAILED_STR=`grep -a "Some tests did not pass" $TS_HOME/tck$1.log`
  echo "-----------------------------------------"
  echo "TCK run #$1 results:"
  echo $TESTS_STR
  echo $PASSED_STR
  echo $FAILED_STR
  if [ "$FAILED_STR" != "" ]; then
    echo "SOME TCK FAILURES DETECTED: "
  fi
  echo "------------------------------------------"
}

stopBroker(){
  echo "************************"
  echo "Stopping the C++ broker"
  echo "************************"
  $CC_HOME/cpp/src/qpidd -q -p $QPID_PORT
}

cleanup
counter=0
runBroker
for j in 1 2
do
   counter=`expr $counter + 1`
   runTck $counter
   printResults $counter
done
cleanup
