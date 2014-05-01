#ifndef _sys_apr_Condition_h
#define _sys_apr_Condition_h

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
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Time.h"

#include <sys/errno.h>
#include <boost/noncopyable.hpp>
#include <apr_thread_cond.h>

namespace qpid {
namespace sys {

/**
 * A condition variable for thread synchronization.
 */
class Condition
{
  public:
    inline Condition();
    inline ~Condition();
    inline void wait(Mutex&);
    inline bool wait(Mutex&, const AbsTime& absoluteTime);
    inline void notify();
    inline void notifyAll();

  private:
    apr_thread_cond_t* condition;
};


Condition::Condition() {
    CHECK_APR_SUCCESS(apr_thread_cond_create(&condition, APRPool::get()));
}

Condition::~Condition() {
    CHECK_APR_SUCCESS(apr_thread_cond_destroy(condition));
}

void Condition::wait(Mutex& mutex) {
    CHECK_APR_SUCCESS(apr_thread_cond_wait(condition, mutex.mutex));
}

bool Condition::wait(Mutex& mutex, const AbsTime& absoluteTime){
    // APR uses microseconds.
    apr_status_t status =
        apr_thread_cond_timedwait(
            condition, mutex.mutex, Duration(now(), absoluteTime)/TIME_USEC);
    if(status != APR_TIMEUP) CHECK_APR_SUCCESS(status);
    return status == 0;
}

void Condition::notify(){
    CHECK_APR_SUCCESS(apr_thread_cond_signal(condition));
}

void Condition::notifyAll(){
    CHECK_APR_SUCCESS(apr_thread_cond_broadcast(condition));
}

}}
#endif  /*!_sys_apr_Condition_h*/
