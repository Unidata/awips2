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

@echo off
REM Script to run the Qpid Java Broker

rem Guess QPID_HOME if not defined
set CURRENT_DIR=%cd%
if not "%QPID_HOME%" == "" goto gotHome
set QPID_HOME=%CURRENT_DIR%
echo %QPID_HOME%
if exist "%QPID_HOME%\bin\qpid-server.bat" goto okHome
cd ..
set QPID_HOME=%cd%
cd %CURRENT_DIR%
:gotHome
if exist "%QPID_HOME%\bin\qpid-server.bat" goto okHome
echo The QPID_HOME environment variable is not defined correctly
echo This environment variable is needed to run this program
goto end
:okHome

REM set QPID_WORK if not set
if not "%QPID_WORK%" == "" goto okQpidWork
if "%HOME%" == "" goto noHome
set QPID_WOKR=%HOME%
goto okQpidWork

:noHome
set QPID_WORK=c:\Temp
if not exist %QPID_WORK% md %QPID_WORK%
:okQpidWork

if not "%JAVA_HOME%" == "" goto gotJavaHome
echo The JAVA_HOME environment variable is not defined
echo This environment variable is needed to run this program
goto end
:gotJavaHome
if not exist "%JAVA_HOME%\bin\java.exe" goto noJavaHome
goto okJavaHome
:noJavaHome
echo The JAVA_HOME environment variable is not defined correctly
echo This environment variable is needed to run this program.
goto end
:okJavaHome

REM set loggin level if not set
if "%AMQJ_LOGGING_LEVEL%" == "" set AMQJ_LOGGING_LEVEL=info

REM Set the default system properties that we'll use now that they have
REM all been initialised
set SYSTEM_PROPS=-Damqj.logging.level=%AMQJ_LOGGING_LEVEL% -DQPID_HOME=%QPID_HOME% -DQPID_WORK=%QPID_WORK%

if "%EXTERNAL_CLASSPATH%" == "" set EXTERNAL_CLASSPATH=%CLASSPATH%

REM Use QPID_CLASSPATH if set
if "%QPID_CLASSPATH%" == "" goto noQpidClasspath
set CLASSPATH=%QPID_CLASSPATH%
echo Using CLASSPATH: %CLASSPATH%
goto afterQpidClasspath

:noQpidClasspath
echo Warning: Qpid classpath not set. CLASSPATH set to %QPID_HOME%\lib\qpid-all.jar
set CLASSPATH=%QPID_HOME%\lib\qpid-all.jar
:afterQpidClasspath

REM start parsing -run arguments
set QPID_ARGS=
if "%1" == "" goto endRunArgs
:runLoop
set var=%1
if "%var:~0,5%" == "-run:" goto runFound
set QPID_ARGS=%QPID_ARGS% %1 
:beforeRunShift
shift
if not "%1"=="" goto runLoop
goto endRunArgs

:runFound
if "%var%" == "-run:debug" goto runDebug
if "%var%" == "-run:jpda" goto runJdpa
if "%var:~0,24%" == "-run:external-classpath-" goto runExternalClasspath
if "%var%" == "-run:print-classpath" goto runPrintCP
if "%var%" == "-run:help" goto runHelp
echo "unrecognized -run option '%var%'. For using external classpaths use -run:external-classpath-option"
goto end
  
:runDebug
REM USAGE: print the classpath and command before running it
set debug=true
goto beforeRunShift

:runJdpa
REM USAGE: adds debugging options to the java command, use
REM USAGE: JDPA_TRANSPORT and JPDA_ADDRESS to customize the debugging
REM USAGE: behavior and use JPDA_OPTS to override it entirely
if "%JPDA_OPTS%" == "" goto beforeRunShift
if "%JPDA_TRANSPORT%" == "" set JPDA_TRANSPORT=-dt_socket
if "%JPDA_ADDRESS%" == "" set JPDA_ADDRESS=8000
set JPDA_OPTS="-Xdebug -Xrunjdwp:transport=%JPDA_TRANSPORT%,address=%JPDA_ADDRESS%,server=y,suspend=n"
set QPID_OPTS="%QPID_OPTS% %JPDA_OPTS%"
goto beforeRunShift

:runExternalClasspath
echo Using external classpath %var%
REM USAGE: Format is -run:external-classpath-first/last/ignore/only as equals special in DOS
REM USAGE: controls how the CLASSPATH environment variable is used by
REM USAGE: this script, value can be one of ignore (the default), first,
REM USAGE: last, and only
if "%var%" == "-run:external-classpath-ignore" goto beforeRunShift
if "%var%" == "-run:external-classpath-first" goto extCPFirst
if "%var%" == "-run:external-classpath-last" goto extCPLast
if "%var%" == "-run:external-classpath-only" goto extCPOnly
echo Invalid value provided for external classpath.
goto end

:extCPFirst
set CLASSPATH=%EXTERNAL_CLASSPATH%;%CLASSPATH%
goto beforeRunShift

:extCPLast
set CLASSPATH=%CLASSPATH%;%EXTERNAL_CLASSPATH%
goto beforeRunShift

:extCPonly
set CLASSPATH=%EXTERNAL_CLASSPATH%
goto beforeRunShift

:runPrintCP
REM USAGE: print the classpath
echo %CLASSPATH%
goto beforeRunShift

:runHelp
REM USAGE: print this message
echo -------------------------------------------------------------------------------------------
echo -run:option where option can be the following.
echo debug : Prints classpath and command before running it
echo jpda : Adds remote debugging info using JPDA_OPTS. Use JPDA_TRANSPORT and JPDA_ADDRESS to
echo        customize, JPDA_OPTS to override
echo external-classpath : Valid values are: ignore, first, last and only.
echo print-classpath : Prints classpath before running command
echo help : Prints this message
echo --------------------------------------------------------------------------------------------
goto end

REM end parsing -run arguments
:endRunArgs

set JAVA_VM=-server
set JAVA_MEM=-Xmx1024m
set JAVA_GC=-XX:+UseConcMarkSweepGC 
rem removing the following vm arg from JAVA_GC  as it is supported on ly in Java 1.6
rem -XX:+HeapDumpOnOutOfMemoryError"

REM Use QPID_JAVA_GC if set
if "%QPID_JAVA_GC%" == "" goto noQpidJavaGC
set JAVA_GC=%QPID_JAVA_GC%
echo Using QPID_JAVA_GC setting: %QPID_JAVA_GC%
goto afteQpidJavaGC

:noQPidJavaGC
echo Info: QPID_JAVA_GC not set. Defaulting to JAVA_GC %JAVA_GC%
:afterQpidJavaGC

REM Use QPID_JAVA_MEM if set
if "%QPID_JAVA_MEM%" == "" goto noQpidJavaMem
set JAVA_MEM=%QPID_JAVA_MEM%
echo Using QPID_JAVA_MEM setting: %QPID_JAVA_MEM%
goto afterQpidJavaMem

:noQpidJavaMem
echo Info: QPID_JAVA_MEM not set. Defaulting to JAVA_MEM %JAVA_MEM%
:after QpidJavaMem


rem QPID_OPTS intended to hold any -D props for use
rem user must enclose any value for QPID_OPTS in double quotes
:runCommand
set MODULE_JARS=%QPID_MODULE_JARS%
set COMMAND="%JAVA_HOME%\bin\java" %JAVA_VM% %JAVA_MEM% %JAVA_GC% %QPID_OPTS% %SYSTEM_PROPS% -cp "%CLASSPATH%;%MODULE_JARS%" org.apache.qpid.server.Main %QPID_ARGS%

if "%debug%" == "true" echo %CLASSPATH%;%LAUNCH_JAR%;%MODULE_JARS%
if "%debug%" == "true" echo %COMMAND%
%COMMAND%

:end
