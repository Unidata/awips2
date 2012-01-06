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

#include "qpid/log/Logger.h"
#include "qpid/sys/Poller.h"
#include "qpid/sys/IOHandle.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/DeletionManager.h"
#include "qpid/sys/posix/check.h"
#include "qpid/sys/posix/PrivatePosix.h"

#include <port.h>
#include <poll.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>

#include <assert.h>
#include <queue>
#include <exception>


//TODO: Remove this
#include "qpid/sys/Dispatcher.h"

namespace qpid {
namespace sys {

// Deletion manager to handle deferring deletion of PollerHandles to when they definitely aren't being used 
DeletionManager<PollerHandlePrivate> PollerHandleDeletionManager;

//  Instantiate (and define) class static for DeletionManager
template <>
DeletionManager<PollerHandlePrivate>::AllThreadsStatuses DeletionManager<PollerHandlePrivate>::allThreadsStatuses(0);

class PollerHandlePrivate {
    friend class Poller;
    friend class PollerHandle;

    enum FDStat {
        ABSENT,
        MONITORED,
        INACTIVE,
        HUNGUP,
        MONITORED_HUNGUP,
        DELETED
    };

    int fd;
    uint32_t events;
    FDStat stat;
    Mutex lock;

    PollerHandlePrivate(int f) :
      fd(f),
      events(0),
      stat(ABSENT) {
    }
    
    bool isActive() const {
        return stat == MONITORED || stat == MONITORED_HUNGUP;
    }

    void setActive() {
        stat = (stat == HUNGUP) ? MONITORED_HUNGUP : MONITORED;
    }

    bool isInactive() const {
        return stat == INACTIVE || stat == HUNGUP;
    }

    void setInactive() {
        stat = INACTIVE;
    }

    bool isIdle() const {
        return stat == ABSENT;
    }

    void setIdle() {
        stat = ABSENT;
    }

    bool isHungup() const {
        return stat == MONITORED_HUNGUP || stat == HUNGUP;
    }

    void setHungup() {
        assert(stat == MONITORED);
        stat = HUNGUP;
    }

    bool isDeleted() const {
        return stat == DELETED;
    }

    void setDeleted() {
        stat = DELETED;
    }
};

PollerHandle::PollerHandle(const IOHandle& h) :
    impl(new PollerHandlePrivate(toFd(h.impl)))
{}

PollerHandle::~PollerHandle() {
    {
    ScopedLock<Mutex> l(impl->lock);
    if (impl->isDeleted()) {
    	return;
    }
    if (impl->isActive()) {
        impl->setDeleted();
    }
    }
    PollerHandleDeletionManager.markForDeletion(impl);
}

/**
 * Concrete implementation of Poller to use the Solaris Event Completion
 * Framework interface
 */
class PollerPrivate {
    friend class Poller;

    class InterruptHandle: public PollerHandle {
        std::queue<PollerHandle*> handles;

        void processEvent(Poller::EventType) {
            PollerHandle* handle = handles.front();
            handles.pop();
            assert(handle);

            //Synthesise event
            Poller::Event event(handle, Poller::INTERRUPTED);

            //Process synthesised event
            event.process();
        }

    public:
        InterruptHandle() : PollerHandle(DummyIOHandle) {}

        void addHandle(PollerHandle& h) {
            handles.push(&h);
        }

        PollerHandle *getHandle() {
            PollerHandle* handle = handles.front();
            handles.pop();
            return handle;
        }

        bool queuedHandles() {
            return handles.size() > 0;
        }
    };

    const int portId;
    bool isShutdown;
    InterruptHandle interruptHandle;
    
    static uint32_t directionToPollEvent(Poller::Direction dir) {
        switch (dir) {
        case Poller::INPUT:  return POLLIN;
        case Poller::OUTPUT: return POLLOUT;
        case Poller::INOUT:  return POLLIN | POLLOUT;
        default: return 0;
        }
    }

    static Poller::EventType pollToDirection(uint32_t events) {
        uint32_t e = events & (POLLIN | POLLOUT);
        switch (e) {
        case POLLIN: return Poller::READABLE;
        case POLLOUT: return Poller::WRITABLE;
        case POLLIN | POLLOUT: return Poller::READ_WRITABLE;
        default:
            return (events & (POLLHUP | POLLERR)) ?
                Poller::DISCONNECTED : Poller::INVALID;
        }
    }
        
    PollerPrivate() :
        portId(::port_create()),
        isShutdown(false) {
        QPID_POSIX_CHECK(portId);
        QPID_LOG(trace, "port_create returned port Id: " << portId);
    }

    ~PollerPrivate() {
    }

    void interrupt() {
        //Send an Alarm to the port
        //We need to send a nonzero event mask, using POLLHUP,
        //nevertheless the wait method will only look for a PORT_ALERT_SET
        QPID_LOG(trace, "Sending a port_alert to " << portId);
        QPID_POSIX_CHECK(::port_alert(portId, PORT_ALERT_SET, POLLHUP,
                                      &static_cast<PollerHandle&>(interruptHandle)));        
    }
};

void Poller::addFd(PollerHandle& handle, Direction dir) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);

    uint32_t events = 0;
  
    if (eh.isIdle()) {
        events = PollerPrivate::directionToPollEvent(dir);
    } else {
        assert(eh.isActive());
        events = eh.events | PollerPrivate::directionToPollEvent(dir);
    }

    //port_associate can be used to add an association or modify an
    //existing one
    QPID_POSIX_CHECK(::port_associate(impl->portId, PORT_SOURCE_FD, (uintptr_t) eh.fd, events, &handle));
    eh.events = events;
    eh.setActive();
    QPID_LOG(trace, "Poller::addFd(handle=" << &handle
             << "[" << typeid(&handle).name()
             << "], fd=" << eh.fd << ")");
}

void Poller::delFd(PollerHandle& handle) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);
    assert(!eh.isIdle());
    int rc = ::port_dissociate(impl->portId, PORT_SOURCE_FD, (uintptr_t) eh.fd);
    //Allow closing an invalid fd, allowing users to close fd before
    //doing delFd()
    if (rc == -1 && errno != EBADFD) {
        QPID_POSIX_CHECK(rc);
    }
    eh.setIdle();
    QPID_LOG(trace, "Poller::delFd(handle=" << &handle
             << ", fd=" << eh.fd << ")");
}

// modFd is equivalent to delFd followed by addFd
void Poller::modFd(PollerHandle& handle, Direction dir) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);
    assert(!eh.isIdle());

    eh.events = PollerPrivate::directionToPollEvent(dir);
  
    //If fd is already associated, events and user arguments are updated
    //So, no need to check if fd is already associated
    QPID_POSIX_CHECK(::port_associate(impl->portId, PORT_SOURCE_FD, (uintptr_t) eh.fd, eh.events, &handle));
    eh.setActive();
    QPID_LOG(trace, "Poller::modFd(handle=" << &handle
             << ", fd=" << eh.fd << ")");
}

void Poller::rearmFd(PollerHandle& handle) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);
    assert(eh.isInactive());
  
    QPID_POSIX_CHECK(::port_associate(impl->portId, PORT_SOURCE_FD, (uintptr_t) eh.fd, eh.events, &handle));
    eh.setActive();
    QPID_LOG(trace, "Poller::rearmdFd(handle=" << &handle
             << ", fd=" << eh.fd << ")");
}

void Poller::shutdown() {
    //Allow sloppy code to shut us down more than once
    if (impl->isShutdown)
        return;

    impl->isShutdown = true;
    impl->interrupt();
}

bool Poller::interrupt(PollerHandle& handle) {
    PollerPrivate::InterruptHandle& ih = impl->interruptHandle;
    PollerHandlePrivate& eh = *static_cast<PollerHandle&>(ih).impl;
    ScopedLock<Mutex> l(eh.lock);
    ih.addHandle(handle);
    impl->interrupt();
    eh.setActive();
    return true;
}
 
void Poller::run() {
    // Make sure we can't be interrupted by signals at a bad time
    ::sigset_t ss;
    ::sigfillset(&ss);
    ::pthread_sigmask(SIG_SETMASK, &ss, 0);

    do {
        Event event = wait();

        // If can read/write then dispatch appropriate callbacks        
        if (event.handle) {
            event.process();
        } else {
            // Handle shutdown
            switch (event.type) {
            case SHUTDOWN:
                return;
            default:
                // This should be impossible
                assert(false);
            }
        }
    } while (true);
}

Poller::Event Poller::wait(Duration timeout) {
    timespec_t tout;
    timespec_t* ptout = NULL;
    port_event_t pe;

    AbsTime targetTimeout = (timeout == TIME_INFINITE) ? FAR_FUTURE :
        AbsTime(now(), timeout);
    
    if (timeout != TIME_INFINITE) {
      tout.tv_sec = 0;
      tout.tv_nsec = timeout;
      ptout = &tout;
    }

    do {
        PollerHandleDeletionManager.markAllUnusedInThisThread();
        QPID_LOG(trace, "About to enter port_get on " << impl->portId
                 << ". Thread " << pthread_self()
                 << ", timeout=" << timeout);


        int rc = ::port_get(impl->portId, &pe, ptout);

        QPID_LOG(trace, "port_get on " << impl->portId
                 << " returned " << rc);
        
        if (impl->isShutdown) {
            PollerHandleDeletionManager.markAllUnusedInThisThread();
            return Event(0, SHUTDOWN);
        }

        if (rc < 0) {
            switch (errno) {
            case EINTR:
                continue;
            case ETIME:
                return Event(0, TIMEOUT);
            default:
                QPID_POSIX_CHECK(rc);
            }
        } else {
            PollerHandle* handle = static_cast<PollerHandle*>(pe.portev_user);
            PollerHandlePrivate& eh = *handle->impl;
            ScopedLock<Mutex> l(eh.lock);

            if (eh.isActive()) {
                QPID_LOG(trace, "Handle is active");
                //We use alert mode to notify interrupts
                if (pe.portev_source == PORT_SOURCE_ALERT &&
                    handle == &impl->interruptHandle) {
                    QPID_LOG(trace, "Interrupt notified");
                    
                    PollerHandle* wrappedHandle = impl->interruptHandle.getHandle();

                    if (impl->interruptHandle.queuedHandles()) {
                        impl->interrupt();
                        eh.setActive();
                    } else {
                        eh.setInactive();
                    }
                    return Event(wrappedHandle, INTERRUPTED);
                }
                
                if (pe.portev_source == PORT_SOURCE_FD) {
                    QPID_LOG(trace, "About to send handle: " << handle);
                    if (pe.portev_events & POLLHUP) {
                        if (eh.isHungup()) {
                            return Event(handle, DISCONNECTED);
                        }
                        eh.setHungup();
                    } else {
                        eh.setInactive();
                    }
                    QPID_LOG(trace, "Sending event (thread: "
                             << pthread_self() << ") for handle " << handle
                             << ", direction= "
                             << PollerPrivate::pollToDirection(pe.portev_events));
                    return Event(handle, PollerPrivate::pollToDirection(pe.portev_events));
                }
            } else if (eh.isDeleted()) {
                //Remove the handle from the poller
                int rc = ::port_dissociate(impl->portId, PORT_SOURCE_FD,
                                           (uintptr_t) eh.fd);
                if (rc == -1 && errno != EBADFD) {
                    QPID_POSIX_CHECK(rc);
                }
            }
        }

        if (timeout == TIME_INFINITE) {
            continue;
        }
        if (rc == 0 && now() > targetTimeout) {
            PollerHandleDeletionManager.markAllUnusedInThisThread();
            return Event(0, TIMEOUT);
        }
    } while (true);
}

// Concrete constructors
Poller::Poller() :
    impl(new PollerPrivate())
{}

Poller::~Poller() {
    delete impl;
}

}}
