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

# Parse arguements taking all - prefixed args as JAVA_OPTS
for arg in "$@"; do
    if [[ $arg == -java:* ]]; then
        JAVA_OPTS="${JAVA_OPTS}-`echo $arg|cut -d ':' -f 2`  "
    else
        ARGS="${ARGS}$arg "
    fi
done
echo "Starting 6 parallel tests"

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-14 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=ping1 batchSize=250 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-14 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=ping2 batchSize=250 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-14 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=ping3 batchSize=250 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-14 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256destinationname=ping4  batchSize=250 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-14 -s [250] -c[100] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=ping5 batchSize=250 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-14 -s [250] -c[100] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=ping6 batchSize=250 -o $QPID_WORK/results ${ARGS}
