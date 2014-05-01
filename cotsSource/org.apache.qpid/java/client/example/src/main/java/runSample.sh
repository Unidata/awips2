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


# Work out the CLASSPATH divider
UNAME=`uname -s`
case $UNAME in
    CYGWIN*)
	DIVIDER=";"
    ;;
    *)
	DIVIDER=":"
;;
esac

if test "'x$QPID_HOME'" != "'x'"
then
    QPID_HOME=$QPID_HOME
else
    QPID_HOME="/usr/share/java/"
fi
echo "Using QPID_HOME: $QPID_HOME"

if test "'x$QPID_SAMPLE'" != "'x'"
then
    QPID_SAMPLE=$QPID_SAMPLE
else
    QPID_SAMPLE=$PWD
fi
echo "Using QPID_SAMPLE: $QPID_SAMPLE"


# set the CLASSPATH
CLASSPATH=`find "$QPID_HOME" -name '*.jar' | tr '\n' "$DIVIDER"`


# compile the samples
javac -cp  "$CLASSPATH" -sourcepath "$QPID_SAMPLE" -d . `find $QPID_SAMPLE -name '*.java'`

# Add output classes to CLASSPATH
CLASSPATH="$CLASSPATH$DIVIDER$."

# Set VM parameters
QPID_PARAM="$QPID_PARAM -Dlog4j.configuration=file://$PWD/log4j.xml"


# Check if the user supplied a sample classname
if test "'x$1'" = "'x'"
then
    echo "No sample classname specified"
    exit;
else
    java -cp $CLASSPATH $QPID_PARAM $*
fi
