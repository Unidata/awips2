@echo off

@rem   Licensed to the Apache Software Foundation (ASF) under one
@rem  or more contributor license agreements.  See the NOTICE file
@rem  distributed with this work for additional information
@rem  regarding copyright ownership.  The ASF licenses this file
@rem  to you under the Apache License, Version 2.0 (the
@rem  "License"); you may not use this file except in compliance
@rem  with the License.  You may obtain a copy of the License at
@rem 
@rem    http://www.apache.org/licenses/LICENSE-2.0
@rem 
@rem  Unless required by applicable law or agreed to in writing,
@rem  software distributed under the License is distributed on an
@rem  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
@rem  KIND, either express or implied.  See the License for the
@rem  specific language governing permissions and limitations
@rem  under the License.

@rem *************************************************************************
@rem This script is used to initialize  environment to start QMan WS-DM Adapter. 
@rem It uses several environment variables described below. 
@rem  You can edit this file according to your environment or (reccommended) set their 
@rem values outside this script.
@rem 
@rem It sets (or retrieve from the environment if defined) the following variables:
@rem
@rem  QMAN_HOME - The home directory of your QMan installation.
@rem JAVA_HOME  - Location of the version of Java runtime used to start QMan. 
@rem QMAN_WSDM_ADAPTER_PORT  - The TCP port that QMan will use to listen for incoming connections. 
@rem QMAN_WSDM_ADAPTER_HOST  - The IP address or DNS name QMan will use to listen for incoming connections
@rem QMAN_CONFIG_FILE - Location of the QMan configuration file.
@rem **************************************************************************

set JAVA=%JAVA_HOME%\bin\java
set JAVA_OPTS=-Xms128m -Xmx512m
SET CLASSPATH=

if not "%JAVA_HOME%" == "" goto CONFIGURE AND START
set JAVA=java

echo JAVA_HOME is not set.  Unexpected results may occur.
echo Set JAVA_HOME to the directory of your local JDK to avoid this message.

:CONFIGURE AND START

if  "%QMAN_HOME%" == "" SET QMAN_HOME=..
if  "%QMAN_WSDM_ADAPTER_PORT%" == "" SET QMAN_WSDM_ADAPTER_PORT=8080
if  "%QMAN_WSDM_ADAPTER_HOST%" == "" SET QMAN_WSDM_ADAPTER_HOST=%COMPUTERNAME%
if  "%QMAN_CONFIG_FILE%" == "" SET QMAN_CONFIG_FILE=%QMAN_HOME%\etc\qman-config.xml

SET ADMIN_PORT=8079
SET ADMIN_KEY=gazzax 
SET QMAN_LIBS=%QMAN_HOME%\lib
SET JETTY_CONFIG_FILE=%QMAN_HOME%\etc\jetty.xml

SET CLASSPATH=%QMAN_HOME%\etc
SET CLASSPATH=%CLASSPATH%;%QMAN_LIBS%\start.jar
SET CLASSPATH=%CLASSPATH%;%QMAN_LIBS%\jetty-6.1.14.jar
SET CLASSPATH=%CLASSPATH%;%QMAN_LIBS%\jetty-util-6.1.14.jar
SET CLASSPATH=%CLASSPATH%;%QMAN_LIBS%\geronimo-servlet_2.5_spec-1.2.jar
SET CLASSPATH=%CLASSPATH%;%QMAN_LIBS%\slf4j-api-1.4.0.jar
SET CLASSPATH=%CLASSPATH%;%QMAN_LIBS%\slf4j-log4j12-1.4.0.jar
SET CLASSPATH=%CLASSPATH%;%QMAN_LIBS%\log4j-1.2.12.jar

echo ===============================================================================
echo.
echo	QMan WS-DM Bridge Bootstrap Environment
echo	------------------------------------------------------
echo.
echo	QMan HOME: %QMAN_HOME%
echo.
echo	Java executable : %JAVA%
echo.
echo	QMan configuration file : %QMAN_CONFIG_FILE%
echo.
echo	Web Server configuration file : %JETTY_CONFIG_FILE%
echo.
echo	Web Server HTTP port : %QMAN_WSDM_ADAPTER_PORT%
echo.
echo	Web Server Admin port : %ADMIN_PORT%
echo.
echo	Bootstrap classpath : %CLASSPATH%
echo.
echo ===============================================================================
echo.

%JAVA% -cp %CLASSPATH% -DQMAN_HOME=%QMAN_HOME% -Djetty.home=%QMAN_HOME% -Dqman.host=%QMAN_WSDM_ADAPTER_HOST%  -Dqman.port=%QMAN_WSDM_ADAPTER_PORT% -DSTOP.PORT=%ADMIN_PORT% -DSTOP.KEY=%ADMIN_KEY% -Dqman-config=%QMAN_CONFIG_FILE%  org.mortbay.start.Main %JETTY_CONFIG_FILE%