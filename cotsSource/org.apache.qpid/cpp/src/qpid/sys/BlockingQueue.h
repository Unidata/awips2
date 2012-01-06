#ifndef QPID_SYS_BLOCKINGQUEUE_H
#define QPID_SYS_BLOCKINGQUEUE_H

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

#include "qpid/sys/Waitable.h"

#include <queue>

namespace qpid {
namespace sys {

/**
 * A simple blocking queue template
 */
template <class T>
class BlockingQueue
{
    mutable sys::Waitable waitable;
    std::queue<T> queue;

public:
    BlockingQueue() {}
    ~BlockingQueue() { close(); }

    /** Pop from the queue, block up to timeout if empty.
     *@param result Set to value popped from queue.
     *@param timeout Defaults to infinite.
     *@return true if result was set, false if queue empty after timeout.
     */
    bool pop(T& result, Duration timeout=TIME_INFINITE) {
        Mutex::ScopedLock l(waitable);
        {
            Waitable::ScopedWait w(waitable);
            if (timeout == TIME_INFINITE) {
                while (queue.empty()) waitable.wait();
            } else {
                AbsTime deadline(now(),timeout);
                while (queue.empty() && deadline > now()) waitable.wait(deadline);
            }
        }
        if (queue.empty()) return false;
        result = queue.front();
        queue.pop();
        if (!queue.empty())
            waitable.notify();  // Notify another waiter.
        return true;
    }

    T pop(Duration timeout=TIME_INFINITE) {
        T result;
        bool ok = pop(result, timeout);
        if (!ok)
            throw Exception("Timed out waiting on a blocking queue");
        return result;
    }
        
    /** Push a value onto the queue.
     * Note it is not an error to push onto a closed queue.
     */
    void push(const T& t) {
        Mutex::ScopedLock l(waitable);
        queue.push(t);
        waitable.notify();      // Notify a waiter.
    }

    /**
     * Close the queue.
     *@ex exception to throw to waiting threads. ClosedException by default.
     */ 
    void close(const ExceptionHolder& ex=ExceptionHolder(new ClosedException()))
    {
        Mutex::ScopedLock l(waitable);
        if (!waitable.hasException()) {
            waitable.setException(ex);
            waitable.notifyAll();
            waitable.waitWaiters(); // Ensure no threads are still waiting.
        }
    }

    /** Open a closed queue. */
    void open() {
        Mutex::ScopedLock l(waitable);
        waitable.resetException();
    }

    bool isClosed() const { 
        Mutex::ScopedLock l(waitable);
        return waitable.hasException();
    }

    bool empty() const {
        Mutex::ScopedLock l(waitable);
        return queue.empty();
    }    
    size_t size() const {
        Mutex::ScopedLock l(waitable);
        return queue.size();
    }    
};

}}



#endif  /*!QPID_SYS_BLOCKINGQUEUE_H*/
