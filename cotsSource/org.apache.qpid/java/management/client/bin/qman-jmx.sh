#!/bin/sh

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
# *************************************************************************
# This script is used to initialize  environment to start QMan JMX Adapter. 
# It uses several environment variables described below. 
#  You can edit this file according to your environment or (reccommended) set their 
# values outside this script.
# 
# It sets (or retrieve from the environment if defined) the following variables:
#
#  QMAN_HOME - The home directory of your QMan installation.
# JAVA_HOME  - Location of the version of Java runtime used to start QMan. 
#  QMAN_CONFIG_FILE - Location of the QMan configuration file.
# **************************************************************************

clear

JAVA=$JAVA_HOME/bin/java
JAVA_OPTS="-Xms128m -Xmx512m"

if [ "$JAVA_HOME" = "" ] ; then
    echo "JAVA_HOME is not set.  Unexpected results may occur."
    echo "Set JAVA_HOME to the directory of your local JDK to avoid this message."
    JAVA=java
fi

if [ "$QMAN_HOME" = "" ] ; then
	QMAN_HOME=..
fi

if [ "$QMAN_CONFIG_FILE" = "" ] ; then
	QMAN_CONFIG_FILE=$QMAN_HOME/etc/qman-config.xml
fi

if [ "$QMAN_LIBS" = "" ] ; then
	QMAN_LIBS=$QMAN_HOME/lib
fi
QMAN_CLASSPATH=`find $QMAN_LIBS | tr '\n' ":"`
QMAN_CLASSPATH=$QMAN_HOME/etc:$QMAN_CLASSPATH

echo "==============================================================================="
echo ""
echo	 "QMan JMX Bridge Bootstrap Environment"
echo	 "--------------------------------------------------"
echo "" 
echo	 "QMan HOME : $QMAN_HOME"
echo ""
echo	 "Java executable : $JAVA"
echo ""
echo	 "Java Opts : $JAVA_OPTS"
echo ""
echo	 "Configuration file : $QMAN_CONFIG_FILE"
echo ""
echo "Bootstrap classpath :  $QMAN_CLASSPATH"
echo ""
echo "==============================================================================="
echo ""

"$JAVA" $JAVA_OPTS -cp $QMAN_CLASSPATH -Dcom.sun.management.jmxremote -Dqman-config=$QMAN_CONFIG_FILE org.apache.qpid.management.domain.services.QMan
