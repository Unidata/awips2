#ifndef QPID_SYS_POLLABLEQUEUE_H
#define QPID_SYS_POLLABLEQUEUE_H

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

#include "qpid/sys/PollableCondition.h"
#include "qpid/sys/Monitor.h"
#include "qpid/sys/Thread.h"
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <algorithm>
#include <vector>

namespace qpid {
namespace sys {

class Poller;

/**
 * A queue whose item processing is dispatched by sys::Poller.
 * Any thread can push to the queue; items pushed trigger an event the Poller
 * recognizes. When a Poller I/O thread dispatches the event, a
 * user-specified callback is invoked with all items on the queue.
 */
template <class T>
class PollableQueue {
  public:
    typedef std::vector<T> Batch;
    typedef T value_type;

    /**
     * Callback to process a batch of items from the queue.
     *
     * @param batch  Queue of values to process. Any items remaining
     *                on return from Callback are put back on the queue.
     * @return iterator pointing to the first un-processed item in batch.
     * Items from this point up to batch.end() are put back on the queue.
     */
    typedef boost::function<typename Batch::const_iterator (const Batch& batch)> Callback;

    /**
     * Constructor; sets necessary parameters.
     *
     * @param cb      Callback that will be called to process items on the
     *                queue. Will be called from a Poller I/O thread.
     * @param poller  Poller to use for dispatching queue events.
     */
    PollableQueue(const Callback& cb,
                  const boost::shared_ptr<sys::Poller>& poller);

    ~PollableQueue();
    
    /** Push a value onto the queue. Thread safe */
    void push(const T& t);

    /** Start polling. */ 
    void start();

    /** Stop polling and wait for the current callback, if any, to complete. */
    void stop();

    /** Are we currently stopped?*/
    bool isStopped() const { ScopedLock l(lock); return stopped; }

    size_t size() { ScopedLock l(lock); return queue.size(); }
    bool empty() { ScopedLock l(lock); return queue.empty(); }

    /**
     * Allow any queued events to be processed; intended for calling
     * after all dispatch threads exit the Poller loop in order to
     * ensure clean shutdown with no events left on the queue.
     */
    void shutdown();
    
  private:
    typedef sys::Monitor::ScopedLock ScopedLock;
    typedef sys::Monitor::ScopedUnlock ScopedUnlock;

    void dispatch(PollableCondition& cond);
    void process();
    
    mutable sys::Monitor lock;
    Callback callback;
    PollableCondition condition;
    Batch queue, batch;
    Thread dispatcher;
    bool stopped;
};

template <class T> PollableQueue<T>::PollableQueue(
    const Callback& cb, const boost::shared_ptr<sys::Poller>& p) 
  : callback(cb),
    condition(boost::bind(&PollableQueue<T>::dispatch, this, _1), p),
    stopped(true)
{
}

template <class T> void PollableQueue<T>::start() {
    ScopedLock l(lock);
    if (!stopped) return;
    stopped = false;
    if (!queue.empty()) condition.set();
}

template <class T> PollableQueue<T>::~PollableQueue() {
}

template <class T> void PollableQueue<T>::push(const T& t) {
    ScopedLock l(lock);
    if (queue.empty()) condition.set();
    queue.push_back(t);
}

template <class T> void PollableQueue<T>::dispatch(PollableCondition& cond) {
    ScopedLock l(lock);
    assert(dispatcher.id() == 0);
    dispatcher = Thread::current();
    process();
    dispatcher = Thread();
    if (queue.empty()) cond.clear();
    if (stopped) lock.notifyAll();
}

template <class T> void PollableQueue<T>::process() {
    // Called with lock held
    while (!stopped && !queue.empty()) {
        assert(batch.empty());
        batch.swap(queue);
        typename Batch::const_iterator putBack;
        {
            ScopedUnlock u(lock);   // Allow concurrent push to queue.
            putBack = callback(batch);
        }
        // put back unprocessed items.
        queue.insert(queue.begin(), putBack, typename Batch::const_iterator(batch.end())); 
        batch.clear();
    }
}

template <class T> void PollableQueue<T>::shutdown() {
    ScopedLock l(lock);
    process();
}

template <class T> void PollableQueue<T>::stop() {
    ScopedLock l(lock);
    if (stopped) return;
    condition.clear();
    stopped = true;
    // Avoid deadlock if stop is called from the dispatch thread
    if (dispatcher.id() != Thread::current().id())
        while (dispatcher.id()) lock.wait();
}

}} // namespace qpid::sys

#endif  /*!QPID_SYS_POLLABLEQUEUE_H*/
