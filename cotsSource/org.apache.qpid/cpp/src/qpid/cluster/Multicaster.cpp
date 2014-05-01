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

#include "qpid/cluster/Multicaster.h"
#include "qpid/cluster/Cpg.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/AMQBody.h"
#include "qpid/framing/AMQFrame.h"

namespace qpid {
namespace cluster {

Multicaster::Multicaster(Cpg& cpg_, 
                         const boost::shared_ptr<sys::Poller>& poller,
                         boost::function<void()> onError_) :
    onError(onError_), cpg(cpg_), 
    queue(boost::bind(&Multicaster::sendMcast, this, _1), poller),
    ready(false)
{
    queue.start();
}

void Multicaster::mcastControl(const framing::AMQBody& body, const ConnectionId& id) {
    mcast(Event::control(body, id));
}

void Multicaster::mcastControl(const framing::AMQFrame& frame, const ConnectionId& id) {
    mcast(Event::control(frame, id));
}

void Multicaster::mcastBuffer(const char* data, size_t size, const ConnectionId& id) {
    Event e(DATA, id, size);
    memcpy(e.getData(), data, size);
    mcast(e);
}

void Multicaster::mcast(const Event& e) {
    {
        sys::Mutex::ScopedLock l(lock);
        if (!ready) {
            if (e.isConnection()) holdingQueue.push_back(e);
            else {
                iovec iov = e.toIovec();
                // FIXME aconway 2009-11-23: configurable retry --cluster-retry
                if (!cpg.mcast(&iov, 1))
                    throw Exception("CPG flow control error during initialization");
                QPID_LOG(trace, "MCAST (direct) " << e);
            }
            return;
        }
    }
    QPID_LOG(trace, "MCAST " << e);
    queue.push(e);
}


Multicaster::PollableEventQueue::Batch::const_iterator Multicaster::sendMcast(const PollableEventQueue::Batch& values) {
    try {
        PollableEventQueue::Batch::const_iterator i = values.begin();
        while( i != values.end()) {
            iovec iov = i->toIovec();
            if (!cpg.mcast(&iov, 1)) {
                // cpg didn't send because of CPG flow control.
                break; 
            }
            ++i;
        }
        return i;
    }
    catch (const std::exception& e) {
        QPID_LOG(critical, "Multicast error: " << e.what());
        queue.stop();
        onError();
        return values.end();
    }
}

void Multicaster::setReady() {
    sys::Mutex::ScopedLock l(lock);
    ready = true;
    std::for_each(holdingQueue.begin(), holdingQueue.end(), boost::bind(&Multicaster::mcast, this, _1));
    holdingQueue.clear();
}

}} // namespace qpid::cluster
