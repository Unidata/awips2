#ifndef _sys_ssl_SslIO
#define _sys_ssl_SslIO
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

#include <boost/function.hpp>
#include <deque>

namespace qpid {
namespace sys {
namespace ssl {
    
class SslSocket;

/*
 * Asynchronous ssl acceptor: accepts connections then does a callback
 * with the accepted fd
 */
class SslAcceptor {
public:
    typedef boost::function1<void, const SslSocket&> Callback;

private:
    Callback acceptedCallback;
    qpid::sys::DispatchHandle handle;
    const SslSocket& socket;

public:
    SslAcceptor(const SslSocket& s, Callback callback);
    ~SslAcceptor();
    void start(qpid::sys::Poller::shared_ptr poller);

private:
    void readable(qpid::sys::DispatchHandle& handle);
};

/*
 * Asynchronous ssl connector: starts the process of initiating a
 * connection and invokes a callback when completed or failed.
 */
class SslConnector : private qpid::sys::DispatchHandle {
public:
    typedef boost::function1<void, const SslSocket&> ConnectedCallback;
    typedef boost::function2<void, int, std::string> FailedCallback;

private:
    ConnectedCallback connCallback;
    FailedCallback failCallback;
    const SslSocket& socket;

public:
    SslConnector(const SslSocket& socket,
                    Poller::shared_ptr poller,
                    std::string hostname,
                    uint16_t port,
                    ConnectedCallback connCb,
                    FailedCallback failCb = 0);

private:
    void connComplete(DispatchHandle& handle);
    void failure(int, std::string);
};

struct SslIOBufferBase {
    char* const bytes;
    const int32_t byteCount;
    int32_t dataStart;
    int32_t dataCount;
    
    SslIOBufferBase(char* const b, const int32_t s) :
        bytes(b),
        byteCount(s),
        dataStart(0),
        dataCount(0)
    {}
    
    virtual ~SslIOBufferBase()
    {}
};

/*
 * Asychronous reader/writer: 
 * Reader accepts buffers to read into; reads into the provided buffers
 * and then does a callback with the buffer and amount read. Optionally it can callback
 * when there is something to read but no buffer to read it into.
 * 
 * Writer accepts a buffer and queues it for writing; can also be given
 * a callback for when writing is "idle" (ie fd is writable, but nothing to write)
 * 
 * The class is implemented in terms of DispatchHandle to allow it to be deleted by deleting
 * the contained DispatchHandle
 */
class SslIO : private qpid::sys::DispatchHandle {
public:
    typedef SslIOBufferBase BufferBase;

    typedef boost::function2<void, SslIO&, BufferBase*> ReadCallback;
    typedef boost::function1<void, SslIO&> EofCallback;
    typedef boost::function1<void, SslIO&> DisconnectCallback;
    typedef boost::function2<void, SslIO&, const SslSocket&> ClosedCallback;
    typedef boost::function1<void, SslIO&> BuffersEmptyCallback;
    typedef boost::function1<void, SslIO&> IdleCallback;


private:
    ReadCallback readCallback;
    EofCallback eofCallback;
    DisconnectCallback disCallback;
    ClosedCallback closedCallback;
    BuffersEmptyCallback emptyCallback;
    IdleCallback idleCallback;
    const SslSocket& socket;
    std::deque<BufferBase*> bufferQueue;
    std::deque<BufferBase*> writeQueue;
    bool queuedClose;
    /**
     * This flag is used to detect and handle concurrency between
     * calls to notifyPendingWrite() (which can be made from any thread) and
     * the execution of the writeable() method (which is always on the
     * thread processing this handle.
     */
    volatile bool writePending;

public:
    SslIO(const SslSocket& s,
        ReadCallback rCb, EofCallback eofCb, DisconnectCallback disCb,
        ClosedCallback cCb = 0, BuffersEmptyCallback eCb = 0, IdleCallback iCb = 0);
    void queueForDeletion();

    void start(qpid::sys::Poller::shared_ptr poller);
    void queueReadBuffer(BufferBase* buff);
    void unread(BufferBase* buff);
    void queueWrite(BufferBase* buff);
    void notifyPendingWrite();
    void queueWriteClose();
    bool writeQueueEmpty() { return writeQueue.empty(); }
    BufferBase* getQueuedBuffer();

    int getKeyLen();

private:
    ~SslIO();
    void readable(qpid::sys::DispatchHandle& handle);
    void writeable(qpid::sys::DispatchHandle& handle);
    void disconnected(qpid::sys::DispatchHandle& handle);
    void close(qpid::sys::DispatchHandle& handle);
};

}}}

#endif // _sys_ssl_SslIO
