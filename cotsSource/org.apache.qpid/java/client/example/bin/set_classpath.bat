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

@REM Helper script to set classpath for running Qpid example classes
@REM NB: You must add the Qpid client and common jars to your CLASSPATH
@REM before running this script

@echo off

if "%QPID_HOME%" == "" GOTO ERROR_QPID_HOME

set QPIDLIB=%QPID_HOME%\lib

if "%CLASSPATH%" == "" GOTO ERROR_CLASSPATH

set CLASSPATH=%CLASSPATH%;%QPIDLIB%\backport-util-concurrent-2.2.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\geronimo-jms_1.1_spec-1.0.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\commons-collections-3.1.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\commons-configuration-1.2.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\commons-cli-1.0.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\commons-lang-2.1.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\commons-logging-api-1.0.4.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\commons-logging-1.0.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\log4j-1.2.12.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\mina-core-1.0.0.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\mina-filter-ssl-1.0.0.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\mina-java5-1.0.0.jar
set CLASSPATH=%CLASSPATH%;%QPIDLIB%\slf4j-simple-1.0.jar

GOTO END

:ERROR_CLASSPATH
Echo Please set set your CLASSPATH variable to include the Qpid client and common jars. Exiting ....
:ERROR_QPID_HOME
Echo Please set QPID_HOME variable. Exiting ....
:END
