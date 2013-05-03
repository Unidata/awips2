@REM
@REM Licensed to the Apache Software Foundation (ASF) under one
@REM or more contributor license agreements.  See the NOTICE file
@REM distributed with this work for additional information
@REM regarding copyright ownership.  The ASF licenses this file
@REM to you under the Apache License, Version 2.0 (the
@REM "License"); you may not use this file except in compliance
@REM with the License.  You may obtain a copy of the License at
@REM 
@REM   http://www.apache.org/licenses/LICENSE-2.0
@REM 
@REM Unless required by applicable law or agreed to in writing,
@REM software distributed under the License is distributed on an
@REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
@REM KIND, either express or implied.  See the License for the
@REM specific language governing permissions and limitations
@REM under the License.
@REM
set CLASSPATH=%CLASSPATH%;%QPID_HOME%/lib/jline-0.9.94.jar
set CLASSPATH=%CLASSPATH%;%QPID_HOME%/lib/junit-4.4.jar
set CLASSPATH=%CLASSPATH%;%QPID_HOME%/lib/qpid-cli-1.0.jar
set CLASSPATH=%CLASSPATH%;%QPID_HOME%/lib/qpid-management-common-M4.jar
set CLASSPATH=%CLASSPATH%;%QPID_HOME%/management/tools/qpid-cli/main/classes/
java -classpath %CLASSPATH% org.apache.qpid.CommandLineInterpreter %1




