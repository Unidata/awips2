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

# Compiles the test classes and sets the CLASSPATH

# check for QPID_TEST_HOME
if [ "$QPID_TEST_HOME" = "" ] ; then
    echo "ERROR: Please set QPID_TEST_HOME ...."
    exit 1
fi

# check for JAVA_HOME
if [ "$JAVA_HOME" = "" ] ; then
    echo "ERROR: Please set JAVA_HOME ...."
    exit 1
fi

# VENDOR_LIB path needs to be set
# for Qpid set this to {qpid_checkout}/java/build/lib
if [ "$VENDOR_LIB" = "" ] ; then
    echo "ERROR: Please set VENDOR_LIB path in the script ...."
    exit 1
fi


[ -d $QPID_TEST_HOME/classes ] || mkdir $QPID_TEST_HOME/classes

CLASSPATH=`find $VENDOR_LIB -name *.jar* | tr '\n' ":"`
$JAVA_HOME/bin/javac -cp $CLASSPATH -d $QPID_TEST_HOME/classes -sourcepath $QPID_TEST_HOME/src `find $QPID_TEST_HOME/src -name '*.java'`

export CLASSPATH=$QPID_TEST_HOME/classes:$CLASSPATH

