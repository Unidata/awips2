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

#include "qpid/sys/DispatchHandle.h"

#include <algorithm>

#include <boost/cast.hpp>

#include <assert.h>

namespace qpid {
namespace sys {

DispatchHandle::DispatchHandle(const IOHandle& h, Callback rCb, Callback wCb, Callback dCb) :
  PollerHandle(h),
  readableCallback(rCb),
  writableCallback(wCb),
  disconnectedCallback(dCb),
  state(IDLE)
{
}


DispatchHandle::~DispatchHandle() {
}

void DispatchHandle::startWatch(Poller::shared_ptr poller0) {
    bool r = readableCallback;
    bool w = writableCallback;

    ScopedLock<Mutex> lock(stateLock);
    assert(state == IDLE);

    poller = poller0;
    poller->registerHandle(*this);
    state = WAITING;
    Poller::Direction dir = r ? 
        ( w ? Poller::INOUT : Poller::INPUT ) : 
        ( w ? Poller::OUTPUT : Poller::NONE ); 
    poller->monitorHandle(*this, dir);
}

void DispatchHandle::rewatch() {
    bool r = readableCallback;
    bool w = writableCallback;
    if (!r && !w) {
        return;
    }
    Poller::Direction dir = r ? 
        ( w ? Poller::INOUT : Poller::INPUT ) : 
        ( w ? Poller::OUTPUT : Poller::NONE ); 

    ScopedLock<Mutex> lock(stateLock);
    switch(state) {
    case IDLE:
    case STOPPING:
    case DELETING:
        return;
    default:
        break;
    }
    assert(poller);
    poller->monitorHandle(*this, dir);
}

void DispatchHandle::rewatchRead() {
    if (!readableCallback) {
        return;
    }

    ScopedLock<Mutex> lock(stateLock);
    switch(state) {
    case IDLE:
    case STOPPING:
    case DELETING:
        return;
    default:
        break;
    }
    assert(poller);
    poller->monitorHandle(*this, Poller::INPUT);
}

void DispatchHandle::rewatchWrite() {
    if (!writableCallback) {
        return;
    }
    
    ScopedLock<Mutex> lock(stateLock);
    switch(state) {
    case IDLE:
    case STOPPING:
    case DELETING:
        return;
    default:
        break;
    }
    assert(poller);
    poller->monitorHandle(*this, Poller::OUTPUT);
}

void DispatchHandle::unwatchRead() {
    if (!readableCallback) {
        return;
    }
    
    ScopedLock<Mutex> lock(stateLock);
    switch(state) {
    case IDLE:
    case STOPPING:
    case DELETING:
        return;
    default:
        break;
    }
    assert(poller);
    poller->unmonitorHandle(*this, Poller::INPUT);
}

void DispatchHandle::unwatchWrite() {
    if (!writableCallback) {
        return;
    }
    
    ScopedLock<Mutex> lock(stateLock);
    switch(state) {
    case IDLE:
    case STOPPING:
    case DELETING:
        return;
    default:
        break;
    }
    assert(poller);
    poller->unmonitorHandle(*this, Poller::OUTPUT);
}

void DispatchHandle::unwatch() {
    ScopedLock<Mutex> lock(stateLock);
    switch(state) {
    case IDLE:
    case STOPPING:
    case DELETING:
        return;
    default:
        break;
    }
    assert(poller);
    poller->unmonitorHandle(*this, Poller::INOUT);
}

void DispatchHandle::stopWatch() {
    ScopedLock<Mutex> lock(stateLock);
    switch (state) {
    case IDLE:
        assert(state != IDLE);
        return;
    case STOPPING:
        assert(state != STOPPING);
        return;
    case CALLING:
        state = STOPPING;
        break;
    case WAITING:
        state = IDLE;
        break;
    case DELETING:
        return;
    }
    assert(poller);
    poller->unregisterHandle(*this);
    poller.reset();
}

// If we are in the IDLE/STOPPING state we can't do the callback as we've
// not/no longer got the fd registered in any poller
void DispatchHandle::call(Callback iCb) {
    assert(iCb);
    ScopedLock<Mutex> lock(stateLock);
    switch (state) {
    case IDLE:
    case STOPPING:
    case DELETING:
        return;
    default:
        interruptedCallbacks.push(iCb);
        assert(poller);
        (void) poller->interrupt(*this);
    }
}

// The slightly strange switch structure
// is to ensure that the lock is released before
// we do the delete
void DispatchHandle::doDelete() {
    {
    ScopedLock<Mutex> lock(stateLock);
    // Ensure that we're no longer watching anything
    switch (state) {
    case IDLE:
        state = DELETING;
        break;
    case STOPPING:
        state = DELETING;
        return;
    case WAITING:
        state = DELETING;
        assert(poller);
        (void) poller->interrupt(*this);
        poller->unregisterHandle(*this);
        return;
    case CALLING:
        state = DELETING;
        assert(poller);
        poller->unregisterHandle(*this);
        return;
    case DELETING:
        return;
    }
    }
    // If we're IDLE we can do this right away
    delete this;
}

void DispatchHandle::processEvent(Poller::EventType type) {

    // Phase I
    {
    ScopedLock<Mutex> lock(stateLock);
    
    switch(state) {
    case IDLE:
        // Can get here if a non connection thread stops watching
        // whilst we were stuck in the above lock 
        return;
    case WAITING:
        state = CALLING;
        break;
    case CALLING:
        assert(state!=CALLING);
        return;
    case STOPPING:
        assert(state!=STOPPING);
        return;
    case DELETING:
        // Need to make sure we clean up any pending callbacks in this case
        std::swap(callbacks, interruptedCallbacks);
        goto saybyebye;
    }
    
    std::swap(callbacks, interruptedCallbacks);
    }

    // Do callbacks - whilst we are doing the callbacks we are prevented from processing
    // the same handle until we re-enable it. To avoid rentering the callbacks for a single
    // handle re-enabling in the callbacks is actually deferred until they are complete.
    switch (type) {
    case Poller::READABLE:
        readableCallback(*this);
        break;
    case Poller::WRITABLE:
        writableCallback(*this);
        break;
    case Poller::READ_WRITABLE:
        readableCallback(*this);
        writableCallback(*this);
        break;
    case Poller::DISCONNECTED:
        if (disconnectedCallback) {
            disconnectedCallback(*this);
        }
        break;
    case Poller::INTERRUPTED:
        {
        // We could only be interrupted if we also had a callback to do
        assert(callbacks.size() > 0);
        // We'll actually do the interrupt below
        }
        break;
    default:
        assert(false);
    }

    // If we have any callbacks do them now -
    // (because we use a copy from before the previous callbacks we won't
    //  do anything yet that was just added) 
    while (callbacks.size() > 0) {
        Callback cb = callbacks.front();
        assert(cb);
        cb(*this);
        callbacks.pop();
    }

    {
    ScopedLock<Mutex> lock(stateLock);
    switch (state) {
    case IDLE:
        assert(state!=IDLE);
        return;
    case STOPPING:
        state = IDLE;
        return;
    case WAITING:
        assert(state!=WAITING);
        return;
    case CALLING:
        state = WAITING;
        return;
    case DELETING:
        break;
    }
    }

saybyebye:
    delete this;
}

}}
