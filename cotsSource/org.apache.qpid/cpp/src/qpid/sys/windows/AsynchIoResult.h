#ifndef _windows_asynchIoResult_h
#define _windows_asynchIoResult_h

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

#include "qpid/sys/AsynchIO.h"
#include "qpid/sys/Socket.h"
#include <memory.h>
#include <winsock2.h>
#include <ws2tcpip.h>

namespace qpid {
namespace sys {
namespace windows {

/*
 * AsynchIoResult defines the class that receives the result of an
 * asynchronous I/O operation, either send/recv or accept/connect.
 *
 * Operation factories should set one of these up before beginning the
 * operation. Poller knows how to dispatch completion to this class.
 * This class must be subclassed for needed operations; this class provides
 * an interface only and cannot be instantiated.
 *
 * This class is tied to Windows; it inherits from OVERLAPPED so that the
 * IocpPoller can cast OVERLAPPED pointers back to AsynchIoResult and call
 * the completion handler.
 */
class AsynchResult : private OVERLAPPED {
public:
    LPOVERLAPPED overlapped(void) { return this; }
    static AsynchResult* from_overlapped(LPOVERLAPPED ol) {
        return static_cast<AsynchResult*>(ol);
    }
    virtual void success (size_t bytesTransferred) {
        bytes = bytesTransferred;
        status = 0;
        complete();
    }
    virtual void failure (int error) {
        bytes = 0;
        status = error;
        complete();
    }
    size_t getTransferred(void) const { return bytes; }
    int getStatus(void) const { return status; }

protected:
    AsynchResult() : bytes(0), status(0)
      { memset(overlapped(), 0, sizeof(OVERLAPPED)); }
    ~AsynchResult() {}
    virtual void complete(void) = 0;

    size_t bytes;
    int status;
};

class AsynchAcceptor;

class AsynchAcceptResult : public AsynchResult {

    friend class AsynchAcceptor;

public:
    AsynchAcceptResult(qpid::sys::AsynchAcceptor::Callback cb,
                       AsynchAcceptor *acceptor,
                       SOCKET listener);
    virtual void success (size_t bytesTransferred);
    virtual void failure (int error);

private:
    virtual void complete(void) {}  // No-op for this class.

    std::auto_ptr<qpid::sys::Socket> newSocket;
    qpid::sys::AsynchAcceptor::Callback callback;
    AsynchAcceptor *acceptor;
    SOCKET listener;

    // AcceptEx needs a place to write the local and remote addresses
    // when accepting the connection. Place those here; get enough for
    // IPv6 addresses, even if the socket is IPv4.
    enum { SOCKADDRMAXLEN = sizeof sockaddr_in6 + 16,
           SOCKADDRBUFLEN = 2 * SOCKADDRMAXLEN };
    char addressBuffer[SOCKADDRBUFLEN];
};

class AsynchIoResult : public AsynchResult {
public:
    typedef boost::function1<void, AsynchIoResult *> Completer;

    virtual ~AsynchIoResult() {}
    qpid::sys::AsynchIO::BufferBase *getBuff(void) const { return iobuff; }
    size_t getRequested(void) const { return requested; }
    const WSABUF *getWSABUF(void) const { return &wsabuf; }

protected:
    void setBuff (qpid::sys::AsynchIO::BufferBase *buffer) { iobuff = buffer; }

protected:
    AsynchIoResult(Completer cb,
                   qpid::sys::AsynchIO::BufferBase *buff, size_t length)
      : completionCallback(cb), iobuff(buff), requested(length) {}

    virtual void complete(void) = 0;
    WSABUF wsabuf;
    Completer completionCallback;

private:
    qpid::sys::AsynchIO::BufferBase *iobuff;
    size_t  requested;     // Number of bytes in original I/O request
};

class AsynchReadResult : public AsynchIoResult {

    // complete() updates buffer then does completion callback.
    virtual void complete(void) {
        getBuff()->dataCount += bytes;
        completionCallback(this);
    }

public:
    AsynchReadResult(AsynchIoResult::Completer cb,
                     qpid::sys::AsynchIO::BufferBase *buff,
                     size_t length)
      : AsynchIoResult(cb, buff, length) {
        wsabuf.buf = buff->bytes + buff->dataCount;
        wsabuf.len = length;
    }
};

class AsynchWriteResult : public AsynchIoResult {

    // complete() updates buffer then does completion callback.
    virtual void complete(void) {
        qpid::sys::AsynchIO::BufferBase *b = getBuff();
        b->dataStart += bytes;
        b->dataCount -= bytes;
        completionCallback(this);
    }

public:
    AsynchWriteResult(AsynchIoResult::Completer cb,
                      qpid::sys::AsynchIO::BufferBase *buff,
                      size_t length)
      : AsynchIoResult(cb, buff, length) {
        wsabuf.buf = buff ? buff->bytes : 0;
        wsabuf.len = length;
    }
};

class AsynchWriteWanted : public AsynchWriteResult {

    // complete() just does completion callback; no buffers used.
    virtual void complete(void) {
        completionCallback(this);
    }

public:
    AsynchWriteWanted(AsynchIoResult::Completer cb)
      : AsynchWriteResult(cb, 0, 0) {
        wsabuf.buf = 0;
        wsabuf.len = 0;
    }
};

class AsynchCallbackRequest : public AsynchIoResult {
    // complete() needs to simply call the completionCallback; no buffers.
    virtual void complete(void) {
        completionCallback(this);
    }

public:
    AsynchCallbackRequest(AsynchIoResult::Completer cb,
                          qpid::sys::AsynchIO::RequestCallback reqCb)
      : AsynchIoResult(cb, 0, 0), reqCallback(reqCb) {
        wsabuf.buf = 0;
        wsabuf.len = 0;
    }

    qpid::sys::AsynchIO::RequestCallback reqCallback;
};

}}}  // qpid::sys::windows

#endif  /*!_windows_asynchIoResult_h*/
