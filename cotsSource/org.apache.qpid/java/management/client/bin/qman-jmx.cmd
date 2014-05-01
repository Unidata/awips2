@echo off

@rem  Licensed to the Apache Software Foundation (ASF) under one
@rem or more contributor license agreements.  See the NOTICE file
@rem distributed with this work for additional information
@rem regarding copyright ownership.  The ASF licenses this file
@rem to you under the Apache License, Version 2.0 (the
@rem "License"); you may not use this file except in compliance
@rem with the License.  You may obtain a copy of the License at
@rem
@rem   http://www.apache.org/licenses/LICENSE-2.0
@rem
@rem Unless required by applicable law or agreed to in writing,
@rem software distributed under the License is distributed on an
@rem "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
@rem KIND, either express or implied.  See the License for the
@rem specific language governing permissions and limitations
@rem under the License.

@rem *************************************************************************
@rem This script is used to initialize  environment to start QMan JMX Adapter. 
@rem It uses several environment variables described below. 
@rem  You can edit this file according to your environment or (reccommended) set their 
@rem values outside this script.
@rem 
@rem It sets (or retrieve from the environment if defined) the following variables:
@rem
@rem  QMAN_HOME - The home directory of your QMan installation.
@rem JAVA_HOME  - Location of the version of Java runtime used to start QMan. 
@rem QMAN_CONFIG_FILE - Location of the QMan configuration file. 
@rem **************************************************************************

cls

:CHECK JVM
set JAVA=%JAVA_HOME%\bin\java
set JAVA_OPTS=-Xms128m -Xmx512m

if not "%JAVA_HOME%" == "" goto CONFIGURE AND START

set JAVA=java

echo.
echo WARNING : JAVA_HOME is not set so unexpected results may occur.
echo Please set JAVA_HOME to the directory of your local JDK / JRE to avoid this message.

:CONFIGURE AND START

if "%QMAN_HOME%" == "" SET QMAN_HOME=..
if "%QMAN_CONFIG_FILE%" == "" SET QMAN_CONFIG_FILE=%QMAN_HOME%\etc\qman-config.xml

SET QMAN_LIBS=%QMAN_HOME%\lib
SET QMAN_CLASSPATH=%QMAN_HOME%\etc

setlocal ENABLEDELAYEDEXPANSION

FOR /R %QMAN_LIBS% %%G IN (*.jar) DO set QMAN_CLASSPATH=!QMAN_CLASSPATH!;%%G

:START
echo ===============================================================================
echo.
echo	QMan JMX Bridge Bootstrap Environment
echo	--------------------------------------------------
echo. 
echo	QMan HOME : %QMAN_HOME%
echo.
echo	Java executable : %JAVA%
echo.
echo	Java Opts : %JAVA_OPTS%
echo.
echo	Configuration file : %QMAN_CONFIG_FILE%
echo.
echo	Bootstrap classpath : %QMAN_CLASSPATH%
echo.
echo ===============================================================================
echo.

"%JAVA%" %JAVA_OPTS% -Dcom.sun.management.jmxremote -Dqman-config=%QMAN_CONFIG_FILE% -classpath "%QMAN_CLASSPATH%" org.apache.qpid.management.domain.services.QMan