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
# This script is used to initialize  environment to start QMan WS-DM Adapter. 
# It uses several environment variables described below. 
#  You can edit this file according to your environment or (reccommended) set their 
# values outside this script.
# 
# It sets (or retrieve from the environment if defined) the following variables:
#
#  QMAN_HOME - The home directory of your QMan installation.
# JAVA_HOME  - Location of the version of Java runtime used to start QMan. 
# QMAN_WSDM_ADAPTER_PORT  - The TCP port that QMan will use to listen for incoming connections. 
# QMAN_WSDM_ADAPTER_HOST  - The IP address or DNS name QMan will use to listen for incoming connections
# QMAN_CONFIG_FILE - Location of the QMan configuration file.
# **************************************************************************

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
if [ "$QMAN_WSDM_ADAPTER_PORT" = "" ] ; then
	QMAN_WSDM_ADAPTER_PORT=8080
fi
if [ "$QMAN_WSDM_ADAPTER_HOST" = "" ] ; then
	QMAN_WSDM_ADAPTER_HOST=$HOSTNAME
fi
if [ "$QMAN_CONFIG_FILE" = "" ] ; then
	QMAN_CONFIG_FILE=$QMAN_HOME/etc/qman-config.xml
fi

ADMIN_PORT=8079
ADMIN_KEY=gazzax 
QMAN_LIBS=$QMAN_HOME/lib
JETTY_CONFIG_FILE=$QMAN_HOME/etc/jetty.xml
 
QMAN_CLASSPATH=$QMAN_HOME/etc:$QMAN_LIBS/start.jar:$QMAN_LIBS/jetty-6.1.14.jar:$QMAN_LIBS/jetty-util-6.1.14.jar:$QMAN_LIBS/geronimo-servlet_2.5_spec-1.2.jar:$QMAN_LIBS/slf4j-api-1.4.0.jar:$QMAN_LIBS/slf4j-log4j12-1.4.0.jar:$QMAN_LIBS/log4j-1.2.12.jar

echo "==============================================================================="
echo""
echo	"QMan WS-DM Bridge Bootstrap Environment"
echo	"------------------------------------------------------"
echo""
echo	"QMan HOME: $QMAN_HOME"
echo""
echo	"Java executable : $JAVA"
echo""
echo	"Java Opts : $JAVA_OPTS"
echo""
echo	"Configuration file : $QMAN_CONFIG_FILE"
echo""
echo	"Web Server Configuration File : $JETTY_CONFIG_FILE"
echo""
echo	"Web Server HTTP port : $QMAN_WSDM_ADAPTER_PORT"
echo""
echo	"Web Server HTTP host : $QMAN_WSDM_ADAPTER_HOST"
echo""
echo	"Web Server Admin port : $ADMIN_PORT"
echo""
echo	"Bootstrap classpath : $QMAN_CLASSPATH"
echo""
echo "==============================================================================="
echo""

"$JAVA" $JAVA_OPTS -cp $QMAN_CLASSPATH -DQMAN_HOME=$QMAN_HOME -Djetty.home=$QMAN_HOME -Dqman.host=$QMAN_WSDM_ADAPTER_HOST -Dqman.port=$QMAN_WSDM_ADAPTER_PORT -DSTOP.PORT=$ADMIN_PORT -DSTOP.KEY=$ADMIN_KEY -Dqman-config=$QMAN_CONFIG_FILE org.mortbay.start.Main $JETTY_CONFIG_FILE