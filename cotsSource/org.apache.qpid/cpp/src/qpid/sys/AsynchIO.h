#ifndef _sys_AsynchIO
#define _sys_AsynchIO
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

#include "qpid/sys/IntegerTypes.h"
#include "qpid/CommonImportExport.h"
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace sys {
    
class Socket;
class Poller;

/*
 * Asynchronous acceptor: accepts connections then does a callback with the
 * accepted fd
 */
class AsynchAcceptor {
public:
    typedef boost::function1<void, const Socket&> Callback;

    QPID_COMMON_EXTERN static AsynchAcceptor* create(const Socket& s, Callback callback);
    virtual ~AsynchAcceptor() {};
    virtual void start(boost::shared_ptr<Poller> poller) = 0;
};

/*
 * Asynchronous connector: starts the process of initiating a connection and
 * invokes a callback when completed or failed.
 */
class AsynchConnector {
public:
    typedef boost::function1<void, const Socket&> ConnectedCallback;
    typedef boost::function3<void, const Socket&, int, const std::string&> FailedCallback;

    // Call create() to allocate a new AsynchConnector object with the
    // specified poller, addressing, and callbacks.
    // This method is implemented in platform-specific code to
    // create a correctly typed object. The platform code also manages
    // deletes. To correctly manage heaps when needed, the allocate and
    // delete should both be done from the same class/library.
    QPID_COMMON_EXTERN static AsynchConnector* create(const Socket& s,
                                   boost::shared_ptr<Poller> poller,
                                   std::string hostname,
                                   uint16_t port,
                                   ConnectedCallback connCb,
                                   FailedCallback failCb);

protected:
    AsynchConnector() {}
    virtual ~AsynchConnector() {}
};

struct AsynchIOBufferBase {
    char* const bytes;
    const int32_t byteCount;
    int32_t dataStart;
    int32_t dataCount;
    
    AsynchIOBufferBase(char* const b, const int32_t s) :
        bytes(b),
        byteCount(s),
        dataStart(0),
        dataCount(0)
    {}
    
    virtual ~AsynchIOBufferBase()
    {}
};

/*
 * Asychronous reader/writer: 
 * Reader accepts buffers to read into; reads into the provided buffers
 * and then does a callback with the buffer and amount read. Optionally it
 * can callback when there is something to read but no buffer to read it into.
 * 
 * Writer accepts a buffer and queues it for writing; can also be given
 * a callback for when writing is "idle" (ie fd is writable, but nothing
 * to write).
 */
class AsynchIO {
public:
    typedef AsynchIOBufferBase BufferBase;

    typedef boost::function2<void, AsynchIO&, BufferBase*> ReadCallback;
    typedef boost::function1<void, AsynchIO&> EofCallback;
    typedef boost::function1<void, AsynchIO&> DisconnectCallback;
    typedef boost::function2<void, AsynchIO&, const Socket&> ClosedCallback;
    typedef boost::function1<void, AsynchIO&> BuffersEmptyCallback;
    typedef boost::function1<void, AsynchIO&> IdleCallback;
    typedef boost::function1<void, AsynchIO&> RequestCallback;

    // Call create() to allocate a new AsynchIO object with the specified
    // callbacks. This method is implemented in platform-specific code to
    // create a correctly typed object. The platform code also manages
    // deletes. To correctly manage heaps when needed, the allocate and
    // delete should both be done from the same class/library.
    QPID_COMMON_EXTERN static AsynchIO* create(const Socket& s,
                            ReadCallback rCb,
                            EofCallback eofCb,
                            DisconnectCallback disCb,
                            ClosedCallback cCb = 0,
                            BuffersEmptyCallback eCb = 0,
                            IdleCallback iCb = 0);
public:
    virtual void queueForDeletion() = 0;

    virtual void start(boost::shared_ptr<Poller> poller) = 0;
    virtual void queueReadBuffer(BufferBase* buff) = 0;
    virtual void unread(BufferBase* buff) = 0;
    virtual void queueWrite(BufferBase* buff) = 0;
    virtual void notifyPendingWrite() = 0;
    virtual void queueWriteClose() = 0;
    virtual bool writeQueueEmpty() = 0;
    virtual void startReading() = 0;
    virtual void stopReading() = 0;
    virtual void requestCallback(RequestCallback) = 0;
    virtual BufferBase* getQueuedBuffer() = 0;

protected:
    // Derived class manages lifetime; must be constructed using the
    // static create() method. Deletes not allowed from outside.
    AsynchIO() {}
    virtual ~AsynchIO() {}
};

}}

#endif // _sys_AsynchIO
