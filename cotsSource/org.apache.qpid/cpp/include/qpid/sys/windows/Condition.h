#ifndef _sys_windows_Condition_h
#define _sys_windows_Condition_h

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

#include "qpid/sys/Mutex.h"
#include "qpid/sys/Time.h"

#include <time.h>
#include <boost/noncopyable.hpp>
#include <boost/thread/condition.hpp>
#include <boost/thread/thread_time.hpp>
#include <windows.h>

namespace qpid {
namespace sys {

// Private Time related implementation details
void toPtime(boost::posix_time::ptime& pt, const AbsTime& t);

/**
 * A condition variable for thread synchronization.
 */
class Condition : private boost::noncopyable
{
  public:
    inline Condition();
    inline ~Condition();
    inline void wait(Mutex&);
    inline bool wait(Mutex&, const AbsTime& absoluteTime);
    inline void notify();
    inline void notifyAll();

  private:
    boost::condition_variable_any condition;
};

Condition::Condition() {
}

Condition::~Condition() {
}

void Condition::wait(Mutex& mutex) {
    condition.wait(mutex.mutex);
}

bool Condition::wait(Mutex& mutex, const AbsTime& absoluteTime){
    return condition.timed_wait(mutex.mutex, absoluteTime.getPrivate());
}

void Condition::notify(){
    condition.notify_one();
}

void Condition::notifyAll(){
    condition.notify_all();
}

}}
#endif  /*!_sys_windows_Condition_h*/
