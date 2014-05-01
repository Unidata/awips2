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

JAVA=$JAVA_HOME/bin/java

if [ "$JAVA_HOME" = "" ] ; then
    echo "JAVA_HOME is not set.  Unexpected results may occur."
    echo "Set JAVA_HOME to the directory of your local JDK to avoid this message."
    JAVA=java
fi

if [ "$QMAN_HOME" = "" ] ; then
	QMAN_HOME=..
fi

ADMIN_PORT=8079
ADMIN_KEY=gazzax 

"$JAVA" -DSTOP.PORT=$ADMIN_PORT -DSTOP.KEY=$ADMIN_KEY -jar $QMAN_HOME/lib/start.jar --stop

echo "QMan WS-DM Adapter shut down successfully."