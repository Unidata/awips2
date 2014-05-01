@echo OFF

REM Licensed to the Apache Software Foundation (ASF) under one
REM or more contributor license agreements.  See the NOTICE file
REM distributed with this work for additional information
REM regarding copyright ownership.  The ASF licenses this file
REM to you under the Apache License, Version 2.0 (the
REM "License"); you may not use this file except in compliance
REM with the License.  You may obtain a copy of the License at
REM 
REM   http://www.apache.org/licenses/LICENSE-2.0
REM 
REM Unless required by applicable law or agreed to in writing,
REM software distributed under the License is distributed on an
REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
REM KIND, either express or implied.  See the License for the
REM specific language governing permissions and limitations
REM under the License.


set nunit_exe=%programfiles%\NUnit 2.5.1\bin\net-2.0\nunit-console.exe
set qpid_dll_location=%QPID_BUILD_ROOT%\src\Debug
set configuration_name=bin\Debug
set qcreate_location=..\..\..\..\..\..\tools\QCreate\Debug

copy %qpid_dll_location%\qpidclientd.dll %configuration_name%
copy %qpid_dll_location%\qpidcommond.dll %configuration_name%

copy %qpid_dll_location%\qpidclientd.dll %qcreate_location%
copy %qpid_dll_location%\qpidcommond.dll %qcreate_location%

%qcreate_location%\QCreate.exe amq.direct routing_key message_queue

"%nunit_exe%" %configuration_name%\Apache.Qpid.Test.Channel.Functional.dll
