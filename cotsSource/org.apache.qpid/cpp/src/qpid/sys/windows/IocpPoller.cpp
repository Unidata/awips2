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
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Dispatcher.h"

#include "qpid/sys/windows/AsynchIoResult.h"
#include "qpid/sys/windows/IoHandlePrivate.h"
#include "qpid/sys/windows/check.h"

#include <winsock2.h>
#include <windows.h>

#include <assert.h>
#include <vector>
#include <exception>

namespace qpid {
namespace sys {

class PollerHandlePrivate {
    friend class Poller;
    friend class PollerHandle;

    SOCKET fd;
    windows::AsynchIoResult::Completer cb;
    AsynchIO::RequestCallback cbRequest;

    PollerHandlePrivate(SOCKET f,
                        windows::AsynchIoResult::Completer cb0 = 0,
                        AsynchIO::RequestCallback rcb = 0)
      : fd(f), cb(cb0), cbRequest(rcb)
    {
    }
    
};

PollerHandle::PollerHandle(const IOHandle& h) :
  impl(new PollerHandlePrivate(toSocketHandle(static_cast<const Socket&>(h)), h.impl->event, h.impl->cbRequest))
{}

PollerHandle::~PollerHandle() {
    delete impl;
}

/**
 * Concrete implementation of Poller to use the Windows I/O Completion
 * port (IOCP) facility.
 */
class PollerPrivate {
    friend class Poller;
    
    const HANDLE iocp;

    // The number of threads running the event loop.
    volatile LONG threadsRunning;

    // Shutdown request is handled by setting isShutdown and injecting a
    // well-formed completion event into the iocp.
    bool isShutdown;

    PollerPrivate() :
        iocp(::CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0)),
        threadsRunning(0),
        isShutdown(false) {
        QPID_WINDOWS_CHECK_NULL(iocp);
    }

    ~PollerPrivate() {
        // It's probably okay to ignore any errors here as there can't be
        // data loss
        ::CloseHandle(iocp);
    }
};

void Poller::shutdown() {
    // Allow sloppy code to shut us down more than once.
    if (impl->isShutdown)
        return;
    ULONG_PTR key = 1;    // Tell wait() it's a shutdown, not I/O
    PostQueuedCompletionStatus(impl->iocp, 0, key, 0);
}

bool Poller::interrupt(PollerHandle&) {
    return false;  // There's no concept of a registered handle.
}

void Poller::run() {
    do {
        Poller::Event event = this->wait();

        // Handle shutdown
        switch (event.type) {
        case Poller::SHUTDOWN:
            return;
            break;
        case Poller::INVALID:  // On any type of success or fail completion
            break;
        default:
          // This should be impossible
          assert(false);
        }
    } while (true);
}

void Poller::monitorHandle(PollerHandle& handle, Direction dir) {
    HANDLE h = (HANDLE)(handle.impl->fd);
    if (h != INVALID_HANDLE_VALUE) {
        HANDLE iocpHandle = ::CreateIoCompletionPort (h, impl->iocp, 0, 0);
        QPID_WINDOWS_CHECK_NULL(iocpHandle);
    }
    else {
        // INPUT is used to request a callback; OUTPUT to request a write
        assert(dir == Poller::INPUT || dir == Poller::OUTPUT);

        if (dir == Poller::OUTPUT) {
            windows::AsynchWriteWanted *result =
                new windows::AsynchWriteWanted(handle.impl->cb);
            PostQueuedCompletionStatus(impl->iocp, 0, 0, result->overlapped());
        }
        else {
            windows::AsynchCallbackRequest *result =
                new windows::AsynchCallbackRequest(handle.impl->cb,
                                                   handle.impl->cbRequest);
            PostQueuedCompletionStatus(impl->iocp, 0, 0, result->overlapped());
        }
    }
}

// All no-ops...
void Poller::unmonitorHandle(PollerHandle& handle, Direction dir) {}
void Poller::registerHandle(PollerHandle& handle) {}
void Poller::unregisterHandle(PollerHandle& handle) {}

Poller::Event Poller::wait(Duration timeout) {
    DWORD timeoutMs = 0;
    DWORD numTransferred = 0;
    ULONG_PTR completionKey = 0;
    OVERLAPPED *overlapped = 0;
    windows::AsynchResult *result = 0;

    // Wait for either an I/O operation to finish (thus signaling the
    // IOCP handle) or a shutdown request to be made (thus signaling the
    // shutdown event).
    if (timeout == TIME_INFINITE)
        timeoutMs = INFINITE;
    else
        timeoutMs = static_cast<DWORD>(timeout / TIME_MSEC);

    InterlockedIncrement(&impl->threadsRunning);
    bool goodOp = ::GetQueuedCompletionStatus (impl->iocp,
                                               &numTransferred,
                                               &completionKey,
                                               &overlapped,
                                               timeoutMs);
    LONG remainingThreads = InterlockedDecrement(&impl->threadsRunning);
    if (goodOp) {
        // Dequeued a successful completion. If it's a posted packet from
        // shutdown() the overlapped ptr is 0 and key is 1. Else downcast
        // the OVERLAPPED pointer to an AsynchIoResult and call the
        // completion handler.
        if (overlapped == 0 && completionKey == 1) {
            // If there are other threads still running this wait, re-post
            // the completion.
            if (remainingThreads > 0)
                PostQueuedCompletionStatus(impl->iocp, 0, completionKey, 0);
            return Event(0, SHUTDOWN);
        }

        result = windows::AsynchResult::from_overlapped(overlapped);
        result->success (static_cast<size_t>(numTransferred));
    }
    else {
      if (overlapped != 0) {
        // Dequeued a completion for a failed operation. Downcast back
        // to the result object and inform it that the operation failed.
        DWORD status = ::GetLastError();
        result = windows::AsynchResult::from_overlapped(overlapped);
        result->failure (static_cast<int>(status));
      }
    }
    return Event(0, INVALID);   // TODO - this may need to be changed.

}

// Concrete constructors
Poller::Poller() :
    impl(new PollerPrivate())
{}

Poller::~Poller() {
    delete impl;
}

}}
