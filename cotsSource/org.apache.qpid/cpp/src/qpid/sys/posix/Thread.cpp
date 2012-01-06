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
#include "qpid/sys/posix/check.h"

#include <pthread.h>

namespace qpid {
namespace sys {

namespace {
void* runRunnable(void* p)
{
    static_cast<Runnable*>(p)->run();
    return 0;
}
}

struct ThreadPrivate {
    pthread_t thread;
    
    ThreadPrivate(Runnable* runnable) {
        QPID_POSIX_ASSERT_THROW_IF(::pthread_create(&thread, NULL, runRunnable, runnable));
    }
    
    ThreadPrivate() : thread(::pthread_self()) {}
};

Thread::Thread() {}

Thread::Thread(Runnable* runnable) : impl(new ThreadPrivate(runnable)) {}

Thread::Thread(Runnable& runnable) : impl(new ThreadPrivate(&runnable)) {}

void Thread::join(){
    if (impl) {
        QPID_POSIX_ASSERT_THROW_IF(::pthread_join(impl->thread, 0));
    }
}

unsigned long Thread::id() {
    if (impl)
        return impl->thread;
    else
        return 0;
}

Thread Thread::current() {
    Thread t;
    t.impl.reset(new ThreadPrivate());
    return t;
}

}}
