#ifndef QPID_SYS_WAITABLE_H
#define QPID_SYS_WAITABLE_H

/*
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

#include "qpid/sys/Monitor.h"
#include "qpid/sys/ExceptionHolder.h"
#include <assert.h>

namespace qpid {
namespace sys {

/**
 * A monitor that keeps track of waiting threads.  Threads declare a
 * ScopedWait around wait() inside a ScopedLock to be considered
 * waiters. 
 *
 * Allows waiting threads to be interrupted by an exception. 
 */
class Waitable : public Monitor {
  public:
    Waitable() : waiters(0) {}

    ~Waitable() { assert(waiters == 0); }

    /** Use this inside a scoped lock around the
     * call to wait() to be counted as a waiter.
     */
    struct ScopedWait {
        Waitable& w;
        ScopedWait(Waitable& w_) : w(w_) { ++w.waiters; }
        ~ScopedWait() { if (--w.waiters==0) w.notifyAll(); }
    };

    /** Block till there are no more waiters in ScopedWaits.
     * waitWaiters() does not raise an exception even if waiters
     * were interrupted by one.
     *@pre Must be called inside a ScopedLock but NOT a ScopedWait.
     */
    void waitWaiters() {
        while (waiters != 0) 
            Monitor::wait();
    }

    /** Returns the number of outstanding ScopedWaits.
     * Must be called with the lock held.
     */
    size_t hasWaiters() const {
        return waiters;
    }

    /** Set an execption to interrupt waiters in ScopedWait.
     * Must be called with the lock held.
     */
    void setException(const ExceptionHolder& e) {
        exception = e;
        notifyAll();
        
    }

    /** True if the waitable has an exception */
    bool hasException() const { return exception; }

    /** Clear the exception if any */
    void resetException() { exception.reset(); }

    /** Throws an exception if one is set before or during the wait. */
    void wait() {
        ExCheck e(exception);
        Monitor::wait();
    }

    /** Throws an exception if one is set before or during the wait. */
    bool wait(const AbsTime& absoluteTime) {
        ExCheck e(exception);
        return Monitor::wait(absoluteTime);
    }

  private:
    struct ExCheck {
        const ExceptionHolder& exception;
        ExCheck(const ExceptionHolder& e) : exception(e) { e.raise(); }
        ~ExCheck() { exception.raise(); }
    };
        
    size_t waiters;
    ExceptionHolder exception;

  friend struct ScopedWait;
};

}} // namespace qpid::sys



#endif  /*!QPID_SYS_WAITABLE_H*/
