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

#include "qpid/sys/Thread.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/windows/check.h"

#include <process.h>
#include <windows.h>

namespace {
unsigned __stdcall runRunnable(void* p)
{
    static_cast<qpid::sys::Runnable*>(p)->run();
    _endthreadex(0);
    return 0;
}
}

namespace qpid {
namespace sys {

class ThreadPrivate {
    friend class Thread;

    HANDLE threadHandle;
    unsigned threadId;
    
    ThreadPrivate(Runnable* runnable) {
        uintptr_t h =  _beginthreadex(0,
                                      0,
                                      runRunnable,
                                      runnable,
                                      0,
                                      &threadId);
        QPID_WINDOWS_CHECK_CRT_NZ(h);
        threadHandle = reinterpret_cast<HANDLE>(h);
    }
    
    ThreadPrivate()
      : threadHandle(GetCurrentThread()), threadId(GetCurrentThreadId()) {}
};

Thread::Thread() {}

Thread::Thread(Runnable* runnable) : impl(new ThreadPrivate(runnable)) {}

Thread::Thread(Runnable& runnable) : impl(new ThreadPrivate(&runnable)) {}

void Thread::join() {
    if (impl) {
        DWORD status = WaitForSingleObject (impl->threadHandle, INFINITE);
        QPID_WINDOWS_CHECK_NOT(status, WAIT_FAILED);
        CloseHandle (impl->threadHandle);
        impl->threadHandle = 0;
    }
}

unsigned long Thread::id() {
    return impl ? impl->threadId : 0;
}

/* static */
Thread Thread::current() {
    Thread t;
    t.impl.reset(new ThreadPrivate());
    return t;
}

}}  /* qpid::sys */
