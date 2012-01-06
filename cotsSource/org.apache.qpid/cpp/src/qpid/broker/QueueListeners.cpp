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
#include "qpid/broker/QueueListeners.h"
#include <boost/bind.hpp>

namespace qpid {
namespace broker {

void QueueListeners::addListener(Consumer::shared_ptr c)
{
    if (c->preAcquires()) {
        add(consumers, c);
    } else {
        add(browsers, c);
    }
}

void QueueListeners::removeListener(Consumer::shared_ptr c)
{
    if (c->preAcquires()) {
        remove(consumers, c);
    } else {
        remove(browsers, c);
    }
}

void QueueListeners::populate(NotificationSet& set)
{
    if (consumers.size()) {
        set.consumer = consumers.front();
        consumers.erase(consumers.begin());
    } else {
        // Don't swap the vectors, hang on to the memory allocated.
        set.browsers = browsers;
        browsers.clear();
    }
}

void QueueListeners::add(Listeners& listeners, Consumer::shared_ptr c)
{
    Listeners::iterator i = std::find(listeners.begin(), listeners.end(), c);
    if (i == listeners.end()) listeners.push_back(c);
}

void QueueListeners::remove(Listeners& listeners, Consumer::shared_ptr c)
{
    Listeners::iterator i = std::find(listeners.begin(), listeners.end(), c);
    if (i != listeners.end()) listeners.erase(i);
}

void QueueListeners::NotificationSet::notify()
{
    if (consumer) consumer->notify();
    else std::for_each(browsers.begin(), browsers.end(), boost::mem_fn(&Consumer::notify));
}

bool QueueListeners::contains(Consumer::shared_ptr c) const {
    return
        std::find(browsers.begin(), browsers.end(), c) != browsers.end() ||
        std::find(consumers.begin(), consumers.end(), c) != consumers.end();
}

}} // namespace qpid::broker
