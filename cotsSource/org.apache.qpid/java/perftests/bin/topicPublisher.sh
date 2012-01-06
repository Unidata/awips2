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

# XXX -Xmx512m -Xms512m -XX:NewSize=150m
QPID_LIBS=$QPID_HOME/lib/qpid-all.jar

# Set other variables used by the qpid-run script before calling
export JAVA=java \
       JAVA_VM=-server \
       JAVA_MEM=-Xmx128m \
       JAVA_GC="-XX:-UseConcMarkSweepGC -XX:+HeapDumpOnOutOfMemoryError" \
       QPID_CLASSPATH=$QPID_LIBS

. qpid-run -Damqj.logging.level="INFO" org.apache.qpid.oldtopic.Publisher $*
