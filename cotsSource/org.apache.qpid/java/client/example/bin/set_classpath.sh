#!/bin/sh -xv
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

# Helper script to set classpath for running Qpid example classes
# NB: You must add the Qpid client and common jars to your CLASSPATH
# before running this script


cygwin=false
if [[ "$(uname -a | fgrep Cygwin)" != "" ]]; then
    cygwin=true
fi

#Should have set the QPID_HOME var after install to the working dir e.g. home/qpid/qpid-1.0-incubating-M2-SNAPSHOT
if [ "$QPID_HOME" = "" ] ; then
    echo "ERROR: Please set QPID_HOME variable. Exiting ...."
    exit 1
else
    QPIDLIB=$QPID_HOME/lib
fi

if $cygwin; then
    QPIDLIB=$(cygpath -w $QPIDLIB)
fi

if [ "$CLASSPATH" = "" ] ; then
    echo "ERROR: Please set set your CLASSPATH variable to include the Qpid client and common jars. Exiting ...."
    exit 2
fi

#Converts paths for cygwin if req
#Some nasty concatenation to get round cygpath line limits
if $cygwin; then
    SEP=";"
    CLASSPATH=`cygpath -w $CLASSPATH`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/backport-util-concurrent-2.2.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/geronimo-jms_1.1_spec-1.0.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/commons-collections-3.1.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/commons-configuration-1.2.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/commons-cli-1.0.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/commons-lang-2.1.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/commons-logging-api-1.0.4.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/commons-logging-1.0.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/log4j-1.2.12.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/mina-core-1.0.0.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/mina-filter-ssl-1.0.0.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/mina-java5-1.0.0.jar`
    CLASSPATH=$CLASSPATH$SEP`cygpath -w $QPIDLIB/slf4j-simple-1.0.jar`
    export CLASSPATH
else
    CLASSPATH=$CLASSPATH:$QPIDLIB/backport-util-concurrent-2.2.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/geronimo-jms_1.1_spec-1.0.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/commons-collections-3.1.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/commons-configuration-1.2.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/commons-cli-1.0.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/commons-lang-2.1.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/commons-logging-api-1.0.4.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/commons-logging-1.0.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/log4j-1.2.12.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/mina-core-1.0.0.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/mina-filter-ssl-1.0.0.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/mina-java5-1.0.0.jar
    CLASSPATH=$CLASSPATH:$QPIDLIB/slf4j-simple-1.0.jar
    export CLASSPATH
fi

