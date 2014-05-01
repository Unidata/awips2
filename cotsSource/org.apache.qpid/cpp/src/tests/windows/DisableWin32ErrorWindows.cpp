/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

// This file intends to prevent Windows from throwing up error boxes and
// offering to debug when serious errors happen. The errors are displayed
// on stderr instead. The purpose of this is to allow the tests to proceed
// scripted and catch the text for logging. If this behavior is desired,
// include this file with the executable being built. If the default
// behaviors are desired, don't include this file in the build.

#include <crtdbg.h>
#include <windows.h>
#include <iostream>

namespace {

// Instead of popping up a window for exceptions, just print something out
LONG _stdcall UnhandledExceptionFilter (PEXCEPTION_POINTERS pExceptionInfo)
{
    DWORD dwExceptionCode = pExceptionInfo->ExceptionRecord->ExceptionCode;

    if (dwExceptionCode == EXCEPTION_ACCESS_VIOLATION)
        std::cerr << "\nERROR: ACCESS VIOLATION\n" << std::endl;
    else
        std::cerr << "\nERROR: UNHANDLED EXCEPTION\n" << std::endl;

    return EXCEPTION_EXECUTE_HANDLER;
}

struct redirect_errors_to_stderr {
    redirect_errors_to_stderr ();
};

static redirect_errors_to_stderr block;

redirect_errors_to_stderr::redirect_errors_to_stderr()
{
    _CrtSetReportMode (_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportFile (_CRT_WARN, _CRTDBG_FILE_STDERR);
    _CrtSetReportMode (_CRT_ERROR, _CRTDBG_MODE_FILE);
    _CrtSetReportFile (_CRT_ERROR, _CRTDBG_FILE_STDERR);
    _CrtSetReportMode (_CRT_ASSERT, _CRTDBG_MODE_FILE);
    _CrtSetReportFile (_CRT_ASSERT, _CRTDBG_FILE_STDERR);

    // Prevent the system from displaying the critical-error-handler
    // and can't-open-file message boxes.
    SetErrorMode(SEM_FAILCRITICALERRORS);
    SetErrorMode(SEM_NOOPENFILEERRORBOX);

    // And this will catch all unhandled exceptions.
    SetUnhandledExceptionFilter (&UnhandledExceptionFilter);
}

}  // namespace
