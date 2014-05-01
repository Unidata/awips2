#ifndef _sys_DispatchHandle_h
#define _sys_DispatchHandle_h

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
#include "qpid/CommonImportExport.h"
#include <boost/function.hpp>

#include <queue>

namespace qpid {
namespace sys {

class DispatchHandleRef;
/**
 * In order to have your own handle (file descriptor on Unix) watched by the poller
 * you need to:
 * 
 * - Subclass IOHandle, in the constructor supply an appropriate
 *   IOHandlerPrivate object for the platform.
 *
 * - Construct a DispatchHandle passing it your IOHandle and 
 *   callback functions for read, write and disconnect events.
 *
 * - Ensure the DispatchHandle is not deleted until the poller is no longer using it.
 *   TODO: astitcher document DispatchHandleRef to simplify this.
 *
 * When an event occurs on the handle, the poller calls the relevant callback and
 * stops watching that handle. Your callback can call rewatch() or related functions
 * to re-enable polling.
 */
class DispatchHandle : public PollerHandle {
    friend class DispatchHandleRef;
public:
    typedef boost::function1<void, DispatchHandle&> Callback;
    typedef std::queue<Callback> CallbackQueue;

private:
    Callback readableCallback;
    Callback writableCallback;
    Callback disconnectedCallback;
    CallbackQueue interruptedCallbacks;
    CallbackQueue callbacks; // Double buffer
    Poller::shared_ptr poller;
    Mutex stateLock;
    enum {
        IDLE,
        STOPPING,
        WAITING,
        CALLING,
        DELETING
    } state;

public:
    /**
     * Provide a handle to poll and a set of callbacks.  Note
     * callbacks can be 0, meaning you are not interested in that
     * event.
     * 
     *@param h: the handle to watch. The IOHandle encapsulates a
     * platfrom-specific handle to an IO object (e.g. a file descriptor
     * on Unix.)
     *@param rCb Callback called when the handle is readable.
     *@param wCb Callback called when the handle is writable.
     *@param dCb Callback called when the handle is disconnected.
     */
    QPID_COMMON_EXTERN DispatchHandle(const IOHandle& h, Callback rCb, Callback wCb, Callback dCb);
    QPID_COMMON_EXTERN ~DispatchHandle();

    /** Add this DispatchHandle to the poller to be watched. */
    QPID_COMMON_EXTERN void startWatch(Poller::shared_ptr poller);

    /** Resume watching for all non-0 callbacks. */
    QPID_COMMON_EXTERN void rewatch();
    /** Resume watching for read only. */
    QPID_COMMON_EXTERN void rewatchRead();

    /** Resume watching for write only. */
    QPID_COMMON_EXTERN void rewatchWrite();

    /** Stop watching temporarily. The DispatchHandle remains
        associated with the poller and can be re-activated using
        rewatch. */
    QPID_COMMON_EXTERN void unwatch();
    /** Stop watching for read */
    QPID_COMMON_EXTERN void unwatchRead();
    /** Stop watching for write */
    QPID_COMMON_EXTERN void unwatchWrite();

    /** Stop watching permanently. Disassociates from the poller. */
    QPID_COMMON_EXTERN void stopWatch();
    
    /** Interrupt watching this handle and make a serialised callback that respects the
     * same exclusivity guarantees as the other callbacks
     */
    QPID_COMMON_EXTERN void call(Callback iCb);

protected:
    QPID_COMMON_EXTERN void doDelete();

private:
    QPID_COMMON_EXTERN void processEvent(Poller::EventType dir);
};

class DispatchHandleRef {
    DispatchHandle* ref;

public:
    typedef boost::function1<void, DispatchHandle&> Callback;
    DispatchHandleRef(const IOHandle& h, Callback rCb, Callback wCb, Callback dCb) :
      ref(new DispatchHandle(h, rCb, wCb, dCb))
    {}

    ~DispatchHandleRef() { ref->doDelete(); }

    void startWatch(Poller::shared_ptr poller) { ref->startWatch(poller); }
    void rewatch() { ref->rewatch(); }
    void rewatchRead() { ref->rewatchRead(); }
    void rewatchWrite() { ref->rewatchWrite(); }
    void unwatch() { ref->unwatch(); }
    void unwatchRead() { ref->unwatchRead(); }
    void unwatchWrite() { ref->unwatchWrite(); }
    void stopWatch() { ref->stopWatch(); }
    void call(Callback iCb) { ref->call(iCb); }
};

}}

#endif // _sys_DispatchHandle_h
