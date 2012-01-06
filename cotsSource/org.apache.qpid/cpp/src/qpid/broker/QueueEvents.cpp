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
#include "qpid/broker/QueueEvents.h"
#include "qpid/Exception.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace broker {

QueueEvents::QueueEvents(const boost::shared_ptr<sys::Poller>& poller, bool isSync) : 
    eventQueue(boost::bind(&QueueEvents::handle, this, _1), poller), enabled(true), sync(isSync) 
{
    if (!sync) eventQueue.start();
}

QueueEvents::~QueueEvents() 
{
    if (!sync) eventQueue.stop();
}

void QueueEvents::enqueued(const QueuedMessage& m)
{
    if (enabled) {
        Event enq(ENQUEUE, m);
        if (sync) {
            for (Listeners::iterator j = listeners.begin(); j != listeners.end(); j++) 
                j->second(enq);
        } else {
            eventQueue.push(enq);
        }
    }
}

void QueueEvents::dequeued(const QueuedMessage& m)
{
    if (enabled) {
        Event deq(DEQUEUE, m);
        if (sync) {
            for (Listeners::iterator j = listeners.begin(); j != listeners.end(); j++) 
                j->second(deq);
        } else {
            eventQueue.push(Event(DEQUEUE, m));
        }
    }
}

void QueueEvents::registerListener(const std::string& id, const EventListener& listener)
{
    qpid::sys::Mutex::ScopedLock l(lock);
    if (listeners.find(id) == listeners.end()) {
        listeners[id] = listener;
    } else {
        throw Exception(QPID_MSG("Event listener already registered for '" << id << "'"));
    }
}

void QueueEvents::unregisterListener(const std::string& id)
{
    qpid::sys::Mutex::ScopedLock l(lock);
    if (listeners.find(id) == listeners.end()) {
        throw Exception(QPID_MSG("No event listener registered for '" << id << "'"));
    } else {
        listeners.erase(id);
    }
}

QueueEvents::EventQueue::Batch::const_iterator
QueueEvents::handle(const EventQueue::Batch& events) {
    qpid::sys::Mutex::ScopedLock l(lock);
    for (EventQueue::Batch::const_iterator i = events.begin(); i != events.end(); ++i) {
        for (Listeners::iterator j = listeners.begin(); j != listeners.end(); j++) {
             j->second(*i);
        }
    }
    return events.end();
}

void QueueEvents::shutdown()
{
    if (!sync && !eventQueue.empty() && !listeners.empty()) eventQueue.shutdown();
}

void QueueEvents::enable()
{
    enabled = true;
    QPID_LOG(debug, "Queue events enabled");
}

void QueueEvents::disable()
{
    enabled = false;
    QPID_LOG(debug, "Queue events disabled");
}

bool QueueEvents::isSync()
{
    return sync;
}


QueueEvents::Event::Event(EventType t, const QueuedMessage& m) : type(t), msg(m) {}


}} // namespace qpid::broker
