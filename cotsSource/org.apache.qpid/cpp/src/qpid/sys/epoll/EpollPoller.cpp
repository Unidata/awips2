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

#include "qpid/sys/Poller.h"
#include "qpid/sys/IOHandle.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/DeletionManager.h"
#include "qpid/sys/posix/check.h"
#include "qpid/sys/posix/PrivatePosix.h"
#include "qpid/log/Statement.h"

#include <sys/epoll.h>
#include <errno.h>
#include <signal.h>

#include <assert.h>
#include <queue>
#include <exception>

namespace qpid {
namespace sys {

// Deletion manager to handle deferring deletion of PollerHandles to when they definitely aren't being used
DeletionManager<PollerHandlePrivate> PollerHandleDeletionManager;

//  Instantiate (and define) class static for DeletionManager
template <>
DeletionManager<PollerHandlePrivate>::AllThreadsStatuses DeletionManager<PollerHandlePrivate>::allThreadsStatuses(0);

class PollerHandlePrivate {
    friend class Poller;
    friend class PollerPrivate;
    friend class PollerHandle;

    enum FDStat {
        ABSENT,
        MONITORED,
        INACTIVE,
        HUNGUP,
        MONITORED_HUNGUP,
        INTERRUPTED,
        INTERRUPTED_HUNGUP,
        DELETED
    };

    ::__uint32_t events;
    const IOHandlePrivate* ioHandle;
    PollerHandle* pollerHandle;
    FDStat stat;
    Mutex lock;

    PollerHandlePrivate(const IOHandlePrivate* h, PollerHandle* p) :
      events(0),
      ioHandle(h),
      pollerHandle(p),
      stat(ABSENT) {
    }

    int fd() const {
        return toFd(ioHandle);
    }

    bool isActive() const {
        return stat == MONITORED || stat == MONITORED_HUNGUP;
    }

    void setActive() {
        stat = (stat == HUNGUP || stat == INTERRUPTED_HUNGUP) 
            ? MONITORED_HUNGUP
            : MONITORED;
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
        return
            stat == MONITORED_HUNGUP ||
            stat == HUNGUP ||
            stat == INTERRUPTED_HUNGUP;
    }

    void setHungup() {
        assert(stat == MONITORED);
        stat = HUNGUP;
    }

    bool isInterrupted() const {
        return stat == INTERRUPTED || stat == INTERRUPTED_HUNGUP;
    }

    void setInterrupted() {
        stat = (stat == MONITORED_HUNGUP || stat == HUNGUP)
            ? INTERRUPTED_HUNGUP
            : INTERRUPTED;
    }

    bool isDeleted() const {
        return stat == DELETED;
    }

    void setDeleted() {
        stat = DELETED;
    }
};

PollerHandle::PollerHandle(const IOHandle& h) :
    impl(new PollerHandlePrivate(h.impl, this))
{}

PollerHandle::~PollerHandle() {
    {
    ScopedLock<Mutex> l(impl->lock);
    if (impl->isDeleted()) {
        return;
    }
    impl->pollerHandle = 0;
    if (impl->isInterrupted()) {
        impl->setDeleted();
        return;
    }
    assert(impl->isIdle());
    impl->setDeleted();
    }
    PollerHandleDeletionManager.markForDeletion(impl);
}

/**
 * Concrete implementation of Poller to use the Linux specific epoll
 * interface
 */
class PollerPrivate {
    friend class Poller;

    static const int DefaultFds = 256;

    struct ReadablePipe {
        int fds[2];

        /**
         * This encapsulates an always readable pipe which we can add
         * to the epoll set to force epoll_wait to return
         */
        ReadablePipe() {
            QPID_POSIX_CHECK(::pipe(fds));
            // Just write the pipe's fds to the pipe
            QPID_POSIX_CHECK(::write(fds[1], fds, 2));
        }

        ~ReadablePipe() {
            ::close(fds[0]);
            ::close(fds[1]);
        }

        int getFD() {
            return fds[0];
        }
    };

    static ReadablePipe alwaysReadable;
    static int alwaysReadableFd;

    class InterruptHandle: public PollerHandle {
        std::queue<PollerHandle*> handles;

        void processEvent(Poller::EventType) {
            PollerHandle* handle = handles.front();
            handles.pop();
            assert(handle);

            // Synthesise event
            Poller::Event event(handle, Poller::INTERRUPTED);

            // Process synthesised event
            event.process();
        }

    public:
        InterruptHandle() :
            PollerHandle(DummyIOHandle)
        {}

        void addHandle(PollerHandle& h) {
            handles.push(&h);
        }

        PollerHandle* getHandle() {
            PollerHandle* handle = handles.front();
            handles.pop();
            return handle;
        }

        bool queuedHandles() {
            return handles.size() > 0;
        }
    };

    const int epollFd;
    bool isShutdown;
    InterruptHandle interruptHandle;
    ::sigset_t sigMask;

    static ::__uint32_t directionToEpollEvent(Poller::Direction dir) {
        switch (dir) {
            case Poller::INPUT:  return ::EPOLLIN;
            case Poller::OUTPUT: return ::EPOLLOUT;
            case Poller::INOUT:  return ::EPOLLIN | ::EPOLLOUT;
            default: return 0;
        }
    }

    static Poller::EventType epollToDirection(::__uint32_t events) {
        // POLLOUT & POLLHUP are mutually exclusive really, but at least socketpairs
        // can give you both!
        events = (events & ::EPOLLHUP) ? events & ~::EPOLLOUT : events;
        ::__uint32_t e = events & (::EPOLLIN | ::EPOLLOUT);
        switch (e) {
            case ::EPOLLIN: return Poller::READABLE;
            case ::EPOLLOUT: return Poller::WRITABLE;
            case ::EPOLLIN | ::EPOLLOUT: return Poller::READ_WRITABLE;
            default:
              return (events & (::EPOLLHUP | ::EPOLLERR)) ?
                    Poller::DISCONNECTED : Poller::INVALID;
        }
    }

    PollerPrivate() :
        epollFd(::epoll_create(DefaultFds)),
        isShutdown(false) {
        QPID_POSIX_CHECK(epollFd);
        ::sigemptyset(&sigMask);
        // Add always readable fd into our set (but not listening to it yet)
        ::epoll_event epe;
        epe.events = 0;
        epe.data.u64 = 1;
        QPID_POSIX_CHECK(::epoll_ctl(epollFd, EPOLL_CTL_ADD, alwaysReadableFd, &epe));
    }

    ~PollerPrivate() {
        // It's probably okay to ignore any errors here as there can't be data loss
        ::close(epollFd);

        // Need to put the interruptHandle in idle state to delete it
        static_cast<PollerHandle&>(interruptHandle).impl->setIdle();
    }

    void resetMode(PollerHandlePrivate& handle);

    void interrupt() {
        ::epoll_event epe;
        // Use EPOLLONESHOT so we only wake a single thread
        epe.events = ::EPOLLIN | ::EPOLLONESHOT;
        epe.data.u64 = 0; // Keep valgrind happy
        epe.data.ptr = &static_cast<PollerHandle&>(interruptHandle);
        QPID_POSIX_CHECK(::epoll_ctl(epollFd, EPOLL_CTL_MOD, alwaysReadableFd, &epe));	
    }

    void interruptAll() {
        ::epoll_event epe;
        // Not EPOLLONESHOT, so we eventually get all threads
        epe.events = ::EPOLLIN;
        epe.data.u64 = 2; // Keep valgrind happy
        QPID_POSIX_CHECK(::epoll_ctl(epollFd, EPOLL_CTL_MOD, alwaysReadableFd, &epe));  
    }
};

PollerPrivate::ReadablePipe PollerPrivate::alwaysReadable;
int PollerPrivate::alwaysReadableFd = alwaysReadable.getFD();

void Poller::registerHandle(PollerHandle& handle) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);
    assert(eh.isIdle());

    ::epoll_event epe;
    epe.events = ::EPOLLONESHOT;
    epe.data.u64 = 0; // Keep valgrind happy
    epe.data.ptr = &eh;

    QPID_POSIX_CHECK(::epoll_ctl(impl->epollFd, EPOLL_CTL_ADD, eh.fd(), &epe));

    eh.setActive();
}

void Poller::unregisterHandle(PollerHandle& handle) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);
    assert(!eh.isIdle());

    int rc = ::epoll_ctl(impl->epollFd, EPOLL_CTL_DEL, eh.fd(), 0);
    // Ignore EBADF since deleting a nonexistent fd has the overall required result!
    // And allows the case where a sloppy program closes the fd and then does the delFd()
    if (rc == -1 && errno != EBADF) {
        QPID_POSIX_CHECK(rc);
    }

    eh.setIdle();
}

void PollerPrivate::resetMode(PollerHandlePrivate& eh) {
    PollerHandle* ph;
    {
    ScopedLock<Mutex> l(eh.lock);
    assert(!eh.isActive());

    if (eh.isIdle() || eh.isDeleted()) {
        return;
    }

    if (eh.events==0) {
        eh.setActive();
        return;
    }

    if (!eh.isInterrupted()) {
        ::epoll_event epe;
        epe.events = eh.events | ::EPOLLONESHOT;
        epe.data.u64 = 0; // Keep valgrind happy
        epe.data.ptr = &eh;

        QPID_POSIX_CHECK(::epoll_ctl(epollFd, EPOLL_CTL_MOD, eh.fd(), &epe));

        eh.setActive();
        return;
    }
    ph = eh.pollerHandle;
    }

    PollerHandlePrivate& ihp = *static_cast<PollerHandle&>(interruptHandle).impl;
    ScopedLock<Mutex> l(ihp.lock);
    interruptHandle.addHandle(*ph);
    ihp.setActive();
    interrupt();
}

void Poller::monitorHandle(PollerHandle& handle, Direction dir) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);
    assert(!eh.isIdle());

    ::__uint32_t oldEvents = eh.events;
    eh.events |= PollerPrivate::directionToEpollEvent(dir);

    // If no change nothing more to do - avoid unnecessary system call
    if (oldEvents==eh.events) {
        return;
    }

    // If we're not actually listening wait till we are to perform change
    if (!eh.isActive()) {
        return;
    }

    ::epoll_event epe;
    epe.events = eh.events | ::EPOLLONESHOT;
    epe.data.u64 = 0; // Keep valgrind happy
    epe.data.ptr = &eh;

    QPID_POSIX_CHECK(::epoll_ctl(impl->epollFd, EPOLL_CTL_MOD, eh.fd(), &epe));
}

void Poller::unmonitorHandle(PollerHandle& handle, Direction dir) {
    PollerHandlePrivate& eh = *handle.impl;
    ScopedLock<Mutex> l(eh.lock);
    assert(!eh.isIdle());

    ::__uint32_t oldEvents = eh.events;
    eh.events &= ~PollerPrivate::directionToEpollEvent(dir);

    // If no change nothing more to do - avoid unnecessary system call
    if (oldEvents==eh.events) {
        return;
    }

    // If we're not actually listening wait till we are to perform change
    if (!eh.isActive()) {
        return;
    }

    ::epoll_event epe;
    epe.events = eh.events | ::EPOLLONESHOT;
    epe.data.u64 = 0; // Keep valgrind happy
    epe.data.ptr = &eh;

    QPID_POSIX_CHECK(::epoll_ctl(impl->epollFd, EPOLL_CTL_MOD, eh.fd(), &epe));
}

void Poller::shutdown() {
    // NB: this function must be async-signal safe, it must not
    // call any function that is not async-signal safe.

    // Allow sloppy code to shut us down more than once
    if (impl->isShutdown)
        return;

    // Don't use any locking here - isShutdown will be visible to all
    // after the epoll_ctl() anyway (it's a memory barrier)
    impl->isShutdown = true;

    impl->interruptAll();
}

bool Poller::interrupt(PollerHandle& handle) {
    {
        PollerHandlePrivate& eh = *handle.impl;
        ScopedLock<Mutex> l(eh.lock);
        if (eh.isIdle() || eh.isDeleted()) {
            return false;
        }

        if (eh.isInterrupted()) {
            return true;
        }

        // Stop monitoring handle for read or write
        ::epoll_event epe;
        epe.events = 0;
        epe.data.u64 = 0; // Keep valgrind happy
        epe.data.ptr = &eh;
        QPID_POSIX_CHECK(::epoll_ctl(impl->epollFd, EPOLL_CTL_MOD, eh.fd(), &epe));

        if (eh.isInactive()) {
            eh.setInterrupted();
            return true;
        }
        eh.setInterrupted();
    }

    PollerPrivate::InterruptHandle& ih = impl->interruptHandle;
    PollerHandlePrivate& eh = *static_cast<PollerHandle&>(ih).impl;
    ScopedLock<Mutex> l(eh.lock);
    ih.addHandle(handle);

    impl->interrupt();
    eh.setActive();
    return true;
}

void Poller::run() {
    // Ensure that we exit thread responsibly under all circumstances
    try {
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
                    PollerHandleDeletionManager.destroyThreadState();
                    return;
                default:
                    // This should be impossible
                    assert(false);
                }
            }
        } while (true);
    } catch (const std::exception& e) {
        QPID_LOG(error, "IO worker thread exiting with unhandled exception: " << e.what());
    }
    PollerHandleDeletionManager.destroyThreadState();
}

Poller::Event Poller::wait(Duration timeout) {
    static __thread PollerHandlePrivate* lastReturnedHandle = 0;
    epoll_event epe;
    int timeoutMs = (timeout == TIME_INFINITE) ? -1 : timeout / TIME_MSEC;
    AbsTime targetTimeout = 
        (timeout == TIME_INFINITE) ?
            FAR_FUTURE :
            AbsTime(now(), timeout); 

    if (lastReturnedHandle) {
        impl->resetMode(*lastReturnedHandle);
        lastReturnedHandle = 0;
    }

    // Repeat until we weren't interrupted by signal
    do {
        PollerHandleDeletionManager.markAllUnusedInThisThread();
        // Need to run on kernels without epoll_pwait()
        // - fortunately in this case we don't really need the atomicity of epoll_pwait()
#if 1
        sigset_t os;
        pthread_sigmask(SIG_SETMASK, &impl->sigMask, &os);
        int rc = ::epoll_wait(impl->epollFd, &epe, 1, timeoutMs);
        pthread_sigmask(SIG_SETMASK, &os, 0);
#else
        int rc = ::epoll_pwait(impl->epollFd, &epe, 1, timeoutMs, &impl->sigMask);
#endif

        if (rc ==-1 && errno != EINTR) {
            QPID_POSIX_CHECK(rc);
        } else if (rc > 0) {
            assert(rc == 1);
            void* dataPtr = epe.data.ptr;

            // Check if this is an interrupt
            PollerPrivate::InterruptHandle& interruptHandle = impl->interruptHandle;
            if (dataPtr == &interruptHandle) {
                PollerHandle* wrappedHandle = 0;
                {
                ScopedLock<Mutex> l(interruptHandle.impl->lock);
                if (interruptHandle.impl->isActive()) {
                    wrappedHandle = interruptHandle.getHandle();
                    // If there is an interrupt queued behind this one we need to arm it
                    // We do it this way so that another thread can pick it up
                    if (interruptHandle.queuedHandles()) {
                        impl->interrupt();
                        interruptHandle.impl->setActive();
                    } else {
                        interruptHandle.impl->setInactive();
                    }
                }
                }
                if (wrappedHandle) {
                    PollerHandlePrivate& eh = *wrappedHandle->impl;
                    {
                    ScopedLock<Mutex> l(eh.lock);
                    if (!eh.isDeleted()) {
                        if (!eh.isIdle()) {
                            eh.setInactive();
                        }
                        lastReturnedHandle = &eh;
                        assert(eh.pollerHandle == wrappedHandle);
                        return Event(wrappedHandle, INTERRUPTED);
                    }
                    }
                    PollerHandleDeletionManager.markForDeletion(&eh);
                }
                continue;
            }

            // Check for shutdown
            if (impl->isShutdown) {
                PollerHandleDeletionManager.markAllUnusedInThisThread();
                return Event(0, SHUTDOWN);
            }

            PollerHandlePrivate& eh = *static_cast<PollerHandlePrivate*>(dataPtr);
            ScopedLock<Mutex> l(eh.lock);

            // the handle could have gone inactive since we left the epoll_wait
            if (eh.isActive()) {
                PollerHandle* handle = eh.pollerHandle;
                assert(handle);

                // If the connection has been hungup we could still be readable
                // (just not writable), allow us to readable until we get here again
                if (epe.events & ::EPOLLHUP) {
                    if (eh.isHungup()) {
                        eh.setInactive();
                        // Don't set up last Handle so that we don't reset this handle
                        // on re-entering Poller::wait. This means that we will never
                        // be set active again once we've returned disconnected, and so
                        // can never be returned again.
                        return Event(handle, DISCONNECTED);
                    }
                    eh.setHungup();
                } else {
                    eh.setInactive();
                }
                lastReturnedHandle = &eh;
                return Event(handle, PollerPrivate::epollToDirection(epe.events));
            }
        }
        // We only get here if one of the following:
        // * epoll_wait was interrupted by a signal
        // * epoll_wait timed out
        // * the state of the handle changed after being returned by epoll_wait
        //
        // The only things we can do here are return a timeout or wait more.
        // Obviously if we timed out we return timeout; if the wait was meant to
        // be indefinite then we should never return with a time out so we go again.
        // If the wait wasn't indefinite, we check whether we are after the target wait
        // time or not
        if (timeoutMs == -1) {
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
