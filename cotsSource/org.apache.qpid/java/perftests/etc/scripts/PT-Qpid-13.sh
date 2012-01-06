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
java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-13.1 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=newd1 uniqueDests=true batchSize=250 transacted=true commitBatchSize=50 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-13.2 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=newd2 uniqueDests=true batchSize=250 transacted=true commitBatchSize=50 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-13.3 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=newd3 uniqueDests=true batchSize=250 transacted=true commitBatchSize=50 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-13.4 -s [250] -c[200] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinatioNname=newd4 uniqueDests=true batchSize=250 transacted=true commitBatchSize=50 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-13.5 -s [250] -c[100] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=newd5 uniqueDests=true batchSize=250 transacted=true commitBatchSize=50 -o $QPID_WORK/results ${ARGS} &

java -Xms256m -Dlog4j.configuration=perftests.log4j -Xmx3072m -Dbadger.level=warn -Damqj.test.logging.level=info -Damqj.logging.level=warn ${JAVA_OPTS} -cp qpid-perftests-1.0-incubating-M2-SNAPSHOT-all-test-deps.jar org.apache.qpid.junit.extensions.TKTestRunner -n PT-Qpid-13.6 -s [250] -c[100] -t testAsyncPingOk org.apache.qpid.ping.PingAsyncTestPerf pubsub=true messageSize=256 destinationName=newd6 uniqueDests=true batchSize=250 transacted=true commitBatchSize=50 -o $QPID_WORK/results ${ARGS}

