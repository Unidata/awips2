#ifndef _sys_Poller_h
#define _sys_Poller_h

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

#include "qpid/sys/Time.h"
#include "qpid/sys/Runnable.h"
#include "qpid/CommonImportExport.h"
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace sys {

/**
 * Poller: abstract class to encapsulate a file descriptor poll to be used
 * by a reactor.
 *
 * @see DispatchHandler for more details of normal use.
 */
class PollerHandle;
class PollerPrivate;
class Poller : public Runnable {
    PollerPrivate* const impl;

public:
    typedef boost::shared_ptr<Poller> shared_ptr;

    enum Direction {
        NONE = 0,
        INPUT,
        OUTPUT,
        INOUT
    };

    enum EventType {
        INVALID = 0,
        READABLE,
        WRITABLE,
        READ_WRITABLE,
        DISCONNECTED,
        SHUTDOWN,
        TIMEOUT,
        INTERRUPTED
    };

    struct Event {
        PollerHandle* handle;
        EventType type;
        
        Event(PollerHandle* handle0, EventType type0) :
          handle(handle0),
          type(type0) {
        }
        
        void process();
    };
    
    QPID_COMMON_EXTERN Poller();
    QPID_COMMON_EXTERN ~Poller();
    /** Note: this function is async-signal safe */
    QPID_COMMON_EXTERN void shutdown();
    
    // Interrupt waiting for a specific poller handle
    // returns true if we could interrupt the handle
    // - in this case on return the handle is no longer being monitored,
    //   but we will receive an event from some invocation of poller::wait
    //   with the handle and the INTERRUPTED event type
    // if it returns false then the handle is not being monitored by the poller
    // - This can either be because it has just received an event which has been
    //   reported and has not been reenabled since.
    // - Because it was removed from the monitoring set
    // - Or because it is already being interrupted 
    QPID_COMMON_EXTERN bool interrupt(PollerHandle& handle);
    
    // Poller run loop
    QPID_COMMON_EXTERN void run();

    QPID_COMMON_EXTERN void registerHandle(PollerHandle& handle);
    QPID_COMMON_EXTERN void unregisterHandle(PollerHandle& handle);
    QPID_COMMON_EXTERN void monitorHandle(PollerHandle& handle, Direction dir);
    QPID_COMMON_EXTERN void unmonitorHandle(PollerHandle& handle, Direction dir);
    QPID_COMMON_EXTERN Event wait(Duration timeout = TIME_INFINITE);
};

/**
 * Handle class to use for polling
 */
class IOHandle;
class PollerHandlePrivate;
class PollerHandle {
    friend class Poller;
    friend class PollerPrivate;
    friend struct Poller::Event;

    PollerHandlePrivate* const impl;
    QPID_COMMON_EXTERN virtual void processEvent(Poller::EventType) {};

public:
    QPID_COMMON_EXTERN PollerHandle(const IOHandle& h);
    QPID_COMMON_EXTERN virtual ~PollerHandle();
};

inline void Poller::Event::process() {
    handle->processEvent(type);
}

}}
#endif // _sys_Poller_h
