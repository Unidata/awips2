#ifndef _sys_apr_Thread_h
#define _sys_apr_Thread_h

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

#include "qpid/sys/apr/APRPool.h"
#include "qpid/sys/apr/APRBase.h"

#include <apr_thread_proc.h>
#include <apr_portable.h>

namespace qpid {
namespace sys {

class Runnable;

class Thread
{
  public:
    inline static Thread current();

    /** ID of current thread for logging.
     * Workaround for broken Thread::current() in APR 
     */
    inline static long logId();

    inline static void yield();

    inline Thread();
    inline explicit Thread(qpid::sys::Runnable*);
    inline explicit Thread(qpid::sys::Runnable&);
    
    inline void join();

    inline long id();

  private:
    static void* APR_THREAD_FUNC runRunnable(apr_thread_t* thread, void *data);
    inline Thread(apr_thread_t* t);
    apr_thread_t* thread;
};

Thread::Thread() : thread(0) {}

Thread::Thread(Runnable* runnable) {
    CHECK_APR_SUCCESS(
        apr_thread_create(&thread, 0, runRunnable, runnable, APRPool::get()));
}

Thread::Thread(Runnable& runnable) {
    CHECK_APR_SUCCESS(
        apr_thread_create(&thread, 0, runRunnable, &runnable, APRPool::get()));
}

void Thread::join(){
    apr_status_t status;
    if (thread != 0) 
        CHECK_APR_SUCCESS(apr_thread_join(&status, thread));
}

long Thread::id() {
    return long(thread);
}

/** ID of current thread for logging.
 * Workaround for broken Thread::current() in APR
 */
long Thread::logId() {
    return static_cast<long>(apr_os_thread_current());
}

Thread::Thread(apr_thread_t* t) : thread(t) {}

Thread Thread::current(){
    apr_thread_t* thr;
    apr_os_thread_t osthr = apr_os_thread_current();
    CHECK_APR_SUCCESS(apr_os_thread_put(&thr, &osthr, APRPool::get()));
    return Thread(thr);
}

void Thread::yield() 
{
    apr_thread_yield();
}

}}
#endif  /*!_sys_apr_Thread_h*/
