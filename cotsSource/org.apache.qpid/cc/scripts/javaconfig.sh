#!/bin/bash
###########################################################
#Licensed to the Apache Software Foundation (ASF) under one
#or more contributor license agreements. See the NOTICE file
#distributed with this work for additional information
#regarding copyright ownership. The ASF licenses this file
#to you under the Apache License, Version 2.0 (the
#"License"); you may not use this file except in compliance
#with the License. You may obtain a copy of the License at
#
#http://www.apache.org/licenses/LICENSE-2.0
#
#Unless required by applicable law or agreed to in writing,
#software distributed under the License is distributed on an
#"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#KIND, either express or implied. See the License for the
#specific language governing permissions and limitations
#under the License.
###########################################################

# copy the profiles
sed "s#store_home#$CPPSTORE_HOME#g" $CC_HOME/cc/config/java/cpp.noprefetch.testprofile > "$CC_HOME/java/"/cpp.noprefetch.testprofile
sed "s#store_home#$CPPSTORE_HOME#g" $CC_HOME/cc/config/java/cpp.testprofile > "$CC_HOME/java"/cpp.testprofile
QPID_JARS=`find "$CC_HOME/java/build/lib" -name '*.jar' | tr '\n' ":"`
QPID_JARS=local.classes=$QPID_JARS
sed "s#local.classes=.*#$QPID_JARS#g" $TS_HOME/bin/build.properties > "$TS_HOME/bin"/build.properties-new
mv $TS_HOME/bin/build.properties-new $TS_HOME/bin/build.properties