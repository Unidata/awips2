#ifndef _sys_Semaphore_h
#define _sys_Semaphore_h

/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/sys/Monitor.h"

namespace qpid {
namespace sys {

class Semaphore
{
public:
    Semaphore(uint c = 1) : count(c) {}

    void lock() { acquire(); }
    void unlock() { release(); }
    bool trylock() { return tryAcquire(); }

    bool tryAcquire() 
    {
        Monitor::ScopedLock l(monitor);
        if (count) {
            count--;
            return true;
        } else {
            return false;
        }
    }

    void acquire() 
    {
        Monitor::ScopedLock l(monitor);
        while (count == 0) monitor.wait();
        count--;
    }

    void release(uint n)
    {
        Monitor::ScopedLock l(monitor);
        if (count==0) monitor.notifyAll();
        count+=n;
    }

    void release()
    {
        release(1);
    }

    void forceLock()
    {
        Monitor::ScopedLock l(monitor);
        count = 0;
    }

private:
    Monitor monitor;
    uint count;
};

}}

#endif  /*!_sys_Semaphore_h*/
