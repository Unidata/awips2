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

#include "qpid/sys/windows/AsynchIoResult.h"
#include "qpid/sys/windows/IoHandlePrivate.h"
#include "qpid/sys/AsynchIO.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Socket.h"
#include "qpid/sys/Poller.h"
#include "qpid/sys/Thread.h"
#include "qpid/sys/Time.h"
#include "qpid/log/Statement.h"

#include "qpid/sys/windows/check.h"

#include <boost/thread/once.hpp>

#include <queue>
#include <winsock2.h>
#include <mswsock.h>
#include <windows.h>

#include <boost/bind.hpp>

namespace {

    typedef qpid::sys::ScopedLock<qpid::sys::Mutex>  QLock;

/*
 * The function pointers for AcceptEx and ConnectEx need to be looked up
 * at run time. Make sure this is done only once.
 */
boost::once_flag lookUpAcceptExOnce = BOOST_ONCE_INIT;
LPFN_ACCEPTEX fnAcceptEx = 0;
typedef void (*lookUpFunc)(const qpid::sys::Socket &);

void lookUpAcceptEx() {
    SOCKET h = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    GUID guidAcceptEx = WSAID_ACCEPTEX;
    DWORD dwBytes = 0;
    WSAIoctl(h,
             SIO_GET_EXTENSION_FUNCTION_POINTER,
             &guidAcceptEx,
             sizeof(guidAcceptEx),
             &fnAcceptEx,
             sizeof(fnAcceptEx),
             &dwBytes,
             NULL,
             NULL);
    closesocket(h);
    if (fnAcceptEx == 0)
        throw qpid::Exception(QPID_MSG("Failed to look up AcceptEx"));
}

}

namespace qpid {
namespace sys {
namespace windows {

/*
 * Asynch Acceptor
 *
 */
class AsynchAcceptor : public qpid::sys::AsynchAcceptor {

    friend class AsynchAcceptResult;

public:
    AsynchAcceptor(const Socket& s, AsynchAcceptor::Callback callback);
    ~AsynchAcceptor();
    void start(Poller::shared_ptr poller);

private:
    void restart(void);

    AsynchAcceptor::Callback acceptedCallback;
    const Socket& socket;
};

AsynchAcceptor::AsynchAcceptor(const Socket& s, Callback callback)
  : acceptedCallback(callback),
    socket(s) {

    s.setNonblocking();
#if (BOOST_VERSION >= 103500)   /* boost 1.35 or later reversed the args */
    boost::call_once(lookUpAcceptExOnce, lookUpAcceptEx);
#else
    boost::call_once(lookUpAcceptEx, lookUpAcceptExOnce);
#endif
}

AsynchAcceptor::~AsynchAcceptor()
{
    socket.close();
}

void AsynchAcceptor::start(Poller::shared_ptr poller) {
    poller->monitorHandle(PollerHandle(socket), Poller::INPUT);
    restart ();
}

void AsynchAcceptor::restart(void) {
    DWORD bytesReceived = 0;  // Not used, needed for AcceptEx API
    AsynchAcceptResult *result = new AsynchAcceptResult(acceptedCallback,
                                                        this,
                                                        toSocketHandle(socket));
    BOOL status;
    status = ::fnAcceptEx(toSocketHandle(socket),
                          toSocketHandle(*result->newSocket),
                          result->addressBuffer,
                          0,
                          AsynchAcceptResult::SOCKADDRMAXLEN,
                          AsynchAcceptResult::SOCKADDRMAXLEN,
                          &bytesReceived,
                          result->overlapped());
    QPID_WINDOWS_CHECK_ASYNC_START(status);
}


AsynchAcceptResult::AsynchAcceptResult(AsynchAcceptor::Callback cb,
                                       AsynchAcceptor *acceptor,
                                       SOCKET listener)
  : callback(cb), acceptor(acceptor), listener(listener) {
    newSocket.reset (new Socket());
}

void AsynchAcceptResult::success(size_t /*bytesTransferred*/) {
    ::setsockopt (toSocketHandle(*newSocket),
                  SOL_SOCKET,
                  SO_UPDATE_ACCEPT_CONTEXT,
                  (char*)&listener,
                  sizeof (listener));
    callback(*(newSocket.release()));
    acceptor->restart ();
    delete this;
}

void AsynchAcceptResult::failure(int status) {
    //if (status != WSA_OPERATION_ABORTED)
    // Can there be anything else?  ;
    delete this;
}

/*
 * AsynchConnector does synchronous connects for now... to do asynch the
 * IocpPoller will need some extension to register an event handle as a
 * CONNECT-type "direction", the connect completion/result will need an
 * event handle to associate with the connecting handle. But there's no
 * time for that right now...
 */
class AsynchConnector : public qpid::sys::AsynchConnector {
private:
    ConnectedCallback connCallback;
    FailedCallback failCallback;
    const Socket& socket;

public:
    AsynchConnector(const Socket& socket,
                    Poller::shared_ptr poller,
                    std::string hostname,
                    uint16_t port,
                    ConnectedCallback connCb,
                    FailedCallback failCb = 0);
};

AsynchConnector::AsynchConnector(const Socket& sock,
                                 Poller::shared_ptr poller,
                                 std::string hostname,
                                 uint16_t port,
                                 ConnectedCallback connCb,
                                 FailedCallback failCb)
  : connCallback(connCb), failCallback(failCb), socket(sock) {
    try {
        socket.connect(hostname, port);
        socket.setNonblocking();
        connCallback(socket);
    } catch(std::exception& e) {
        if (failCallback)
            failCallback(socket, -1, std::string(e.what()));
        socket.close();
        delete &socket;
    }
}

} // namespace windows

AsynchAcceptor* AsynchAcceptor::create(const Socket& s, 
                                       Callback callback)
{
    return new windows::AsynchAcceptor(s, callback);
}

AsynchConnector* qpid::sys::AsynchConnector::create(const Socket& s,
                                                    Poller::shared_ptr poller,
                                                    std::string hostname,
                                                    uint16_t port,
                                                    ConnectedCallback connCb,
                                                    FailedCallback failCb)
{
    return new windows::AsynchConnector(s,
                                        poller,
                                        hostname,
                                        port,
                                        connCb,
                                        failCb);
}


/*
 * Asynch reader/writer
 */

namespace windows {

class AsynchIO : public qpid::sys::AsynchIO {
public:
    AsynchIO(const Socket& s,
             ReadCallback rCb,
             EofCallback eofCb,
             DisconnectCallback disCb,
             ClosedCallback cCb = 0,
             BuffersEmptyCallback eCb = 0,
             IdleCallback iCb = 0);
    ~AsynchIO();

    // Methods inherited from qpid::sys::AsynchIO

    /**
     * Notify the object is should delete itself as soon as possible.
     */
    virtual void queueForDeletion();

    /// Take any actions needed to prepare for working with the poller.
    virtual void start(Poller::shared_ptr poller);
    virtual void queueReadBuffer(BufferBase* buff);
    virtual void unread(BufferBase* buff);
    virtual void queueWrite(BufferBase* buff);
    virtual void notifyPendingWrite();
    virtual void queueWriteClose();
    virtual bool writeQueueEmpty();
    virtual void startReading();
    virtual void stopReading();
    virtual void requestCallback(RequestCallback);

    /**
     * getQueuedBuffer returns a buffer from the buffer queue, if one is
     * available.
     *
     * @retval Pointer to BufferBase buffer; 0 if none is available.
     */
    virtual BufferBase* getQueuedBuffer();

private:
    ReadCallback readCallback;
    EofCallback eofCallback;
    DisconnectCallback disCallback;
    ClosedCallback closedCallback;
    BuffersEmptyCallback emptyCallback;
    IdleCallback idleCallback;
    const Socket& socket;
    Poller::shared_ptr poller;

    std::deque<BufferBase*> bufferQueue;
    std::deque<BufferBase*> writeQueue;
    /* The MSVC-supplied deque is not thread-safe; keep locks to serialize
     * access to the buffer queue and write queue.
     */
    Mutex bufferQueueLock;

    // Number of outstanding I/O operations.
    volatile LONG opsInProgress;
    // Is there a write in progress?
    volatile bool writeInProgress;
    // Deletion requested, but there are callbacks in progress.
    volatile bool queuedDelete;
    // Socket close requested, but there are operations in progress.
    volatile bool queuedClose;

private:
    // Dispatch events that have completed.
    void notifyEof(void);
    void notifyDisconnect(void);
    void notifyClosed(void);
    void notifyBuffersEmpty(void);
    void notifyIdle(void);

    /**
     * Initiate a write of the specified buffer. There's no callback for
     * write completion to the AsynchIO object.
     */
    void startWrite(AsynchIO::BufferBase* buff);

    void close(void);

    /**
     * readComplete is called when a read request is complete.
     *
     * @param result Results of the operation.
     */
    void readComplete(AsynchReadResult *result);

    /**
     * writeComplete is called when a write request is complete.
     *
     * @param result Results of the operation.
     */
    void writeComplete(AsynchWriteResult *result);

    /**
     * Queue of completions to run. This queue enforces the requirement
     * from upper layers that only one thread at a time is allowed to act
     * on any given connection. Once a thread is busy processing a completion
     * on this object, other threads that dispatch completions queue the
     * completions here for the in-progress thread to handle when done.
     * Thus, any threads can dispatch a completion from the IocpPoller, but
     * this class ensures that actual processing at the connection level is
     * only on one thread at a time.
     */
    std::queue<AsynchIoResult *> completionQueue;
    volatile bool working;
    Mutex completionLock;

    /**
     * Called when there's a completion to process.
     */
    void completion(AsynchIoResult *result);
};

// This is used to encapsulate pure callbacks into a handle
class CallbackHandle : public IOHandle {
public:
    CallbackHandle(AsynchIoResult::Completer completeCb,
                   AsynchIO::RequestCallback reqCb = 0) :
    IOHandle(new IOHandlePrivate (INVALID_SOCKET, completeCb, reqCb))
    {}
};

AsynchIO::AsynchIO(const Socket& s,
                   ReadCallback rCb,
                   EofCallback eofCb,
                   DisconnectCallback disCb,
                   ClosedCallback cCb,
                   BuffersEmptyCallback eCb,
                   IdleCallback iCb) :

    readCallback(rCb),
    eofCallback(eofCb),
    disCallback(disCb),
    closedCallback(cCb),
    emptyCallback(eCb),
    idleCallback(iCb),
    socket(s),
    opsInProgress(0),
    writeInProgress(false),
    queuedDelete(false),
    queuedClose(false),
    working(false) {
}

struct deleter
{
    template <typename T>
    void operator()(T *ptr){ delete ptr;}
};

AsynchIO::~AsynchIO() {
    std::for_each( bufferQueue.begin(), bufferQueue.end(), deleter());
    std::for_each( writeQueue.begin(), writeQueue.end(), deleter());
}

void AsynchIO::queueForDeletion() {
    queuedDelete = true;
    if (opsInProgress > 0) {
        QPID_LOG(info, "Delete AsynchIO queued; ops in progress");
        // AsynchIOHandler calls this then deletes itself; don't do any more
        // callbacks.
        readCallback = 0;
        eofCallback = 0;
        disCallback = 0;
        closedCallback = 0;
        emptyCallback = 0;
        idleCallback = 0;
    }
    else {
        delete this;
    }
}

void AsynchIO::start(Poller::shared_ptr poller0) {
    poller = poller0;
    poller->monitorHandle(PollerHandle(socket), Poller::INPUT);
    if (writeQueue.size() > 0)  // Already have data queued for write
        notifyPendingWrite();
    startReading();
}

void AsynchIO::queueReadBuffer(AsynchIO::BufferBase* buff) {
    assert(buff);
    buff->dataStart = 0;
    buff->dataCount = 0;
    QLock l(bufferQueueLock);
    bufferQueue.push_back(buff);
}

void AsynchIO::unread(AsynchIO::BufferBase* buff) {
    assert(buff);
    if (buff->dataStart != 0) {
        memmove(buff->bytes, buff->bytes+buff->dataStart, buff->dataCount);
        buff->dataStart = 0;
    }
    QLock l(bufferQueueLock);
    bufferQueue.push_front(buff);
}

void AsynchIO::queueWrite(AsynchIO::BufferBase* buff) {
    assert(buff);
    QLock l(bufferQueueLock);
    writeQueue.push_back(buff);
    if (!writeInProgress)
        notifyPendingWrite();
}

void AsynchIO::notifyPendingWrite() {
    // This method is generally called from a processing thread; transfer
    // work on this to an I/O thread. Much of the upper layer code assumes
    // that all I/O-related things happen in an I/O thread.
    if (poller == 0)    // Not really going yet...
        return;

    InterlockedIncrement(&opsInProgress);
    PollerHandle ph(CallbackHandle(boost::bind(&AsynchIO::completion, this, _1)));
    poller->monitorHandle(ph, Poller::OUTPUT);
}

void AsynchIO::queueWriteClose() {
    queuedClose = true;
    if (!writeInProgress)
        notifyPendingWrite();
}

bool AsynchIO::writeQueueEmpty() {
    QLock l(bufferQueueLock);
    return writeQueue.size() == 0;
}

/*
 * Initiate a read operation. AsynchIO::readComplete() will be
 * called when the read is complete and data is available.
 */
void AsynchIO::startReading() {
    if (queuedDelete)
        return;

    // (Try to) get a buffer; look on the front since there may be an
    // "unread" one there with data remaining from last time.
    AsynchIO::BufferBase *buff = 0;
    {
        QLock l(bufferQueueLock);

        if (!bufferQueue.empty()) {
            buff = bufferQueue.front();
            assert(buff);
            bufferQueue.pop_front();
        }
    }
    if (buff != 0) {
        int readCount = buff->byteCount - buff->dataCount;
        AsynchReadResult *result =
            new AsynchReadResult(boost::bind(&AsynchIO::completion, this, _1),
                                 buff,
                                 readCount);
        DWORD bytesReceived = 0, flags = 0;
        InterlockedIncrement(&opsInProgress);
        int status = WSARecv(toSocketHandle(socket),
                             const_cast<LPWSABUF>(result->getWSABUF()), 1,
                             &bytesReceived,
                             &flags,
                             result->overlapped(),
                             0);
        if (status != 0) {
            int error = WSAGetLastError();
            if (error != WSA_IO_PENDING) {
                result->failure(error);
                result = 0;   // result is invalid here
                return;
            }
        }
        // On status 0 or WSA_IO_PENDING, completion will handle the rest.
    }
    else {
        notifyBuffersEmpty();
    }
    return;
}

// stopReading was added to prevent a race condition with read-credit on Linux.
// It may or may not be required on windows.
// 
// AsynchIOHandler::readbuff() calls stopReading() inside the same
// critical section that protects startReading() in
// AsynchIOHandler::giveReadCredit().
// 
void AsynchIO::stopReading() {}

// Queue the specified callback for invocation from an I/O thread.
void AsynchIO::requestCallback(RequestCallback callback) {
    // This method is generally called from a processing thread; transfer
    // work on this to an I/O thread. Much of the upper layer code assumes
    // that all I/O-related things happen in an I/O thread.
    if (poller == 0)    // Not really going yet...
        return;

    InterlockedIncrement(&opsInProgress);
    PollerHandle ph(CallbackHandle(
        boost::bind(&AsynchIO::completion, this, _1),
        callback));
    poller->monitorHandle(ph, Poller::INPUT);
}

/**
 * Return a queued buffer if there are enough to spare.
 */
AsynchIO::BufferBase* AsynchIO::getQueuedBuffer() {
    QLock l(bufferQueueLock);
    // Always keep at least one buffer (it might have data that was
    // "unread" in it).
    if (bufferQueue.size() <= 1)
        return 0;
    BufferBase* buff = bufferQueue.back();
    assert(buff);
    bufferQueue.pop_back();
    return buff;
}

void AsynchIO::notifyEof(void) {
    if (eofCallback)
        eofCallback(*this);
}

void AsynchIO::notifyDisconnect(void) {
    if (disCallback)
        disCallback(*this);
}

void AsynchIO::notifyClosed(void) {
    if (closedCallback)
        closedCallback(*this, socket);
}

void AsynchIO::notifyBuffersEmpty(void) {
    if (emptyCallback)
        emptyCallback(*this);
}

void AsynchIO::notifyIdle(void) {
    if (idleCallback)
        idleCallback(*this);
}

/*
 * Asynch reader/writer using overlapped I/O
 */

void AsynchIO::startWrite(AsynchIO::BufferBase* buff) {
    writeInProgress = true;
    InterlockedIncrement(&opsInProgress);
    int writeCount = buff->byteCount-buff->dataCount;
    AsynchWriteResult *result =
        new AsynchWriteResult(boost::bind(&AsynchIO::completion, this, _1),
                              buff,
                              buff->dataCount);
    DWORD bytesSent = 0;
    int status = WSASend(toSocketHandle(socket),
                         const_cast<LPWSABUF>(result->getWSABUF()), 1,
                         &bytesSent,
                         0,
                         result->overlapped(),
                         0);
    if (status != 0) {
        int error = WSAGetLastError();
        if (error != WSA_IO_PENDING) {
            result->failure(error);   // Also decrements in-progress count
            result = 0;   // result is invalid here
            return;
        }
    }
    // On status 0 or WSA_IO_PENDING, completion will handle the rest.
    return;
}

/*
 * Close the socket and callback to say we've done it
 */
void AsynchIO::close(void) {
    socket.close();
    notifyClosed();
}

void AsynchIO::readComplete(AsynchReadResult *result) {
    int status = result->getStatus();
    size_t bytes = result->getTransferred();
    if (status == 0 && bytes > 0) {
        bool restartRead = true;     // May not if receiver doesn't want more
        if (readCallback)
            readCallback(*this, result->getBuff());
        if (restartRead)
            startReading();
    }
    else {
        // No data read, so put the buffer back. It may be partially filled,
        // so "unread" it back to the front of the queue.
        unread(result->getBuff());
        if (status == 0)
            notifyEof();
        else
            notifyDisconnect();
    }
}

/*
 * NOTE - this completion is called for completed writes and also when 
 * a write is desired. The difference is in the buff - if a write is desired
 * the buff is 0.
 */
void AsynchIO::writeComplete(AsynchWriteResult *result) {
    int status = result->getStatus();
    size_t bytes = result->getTransferred();
    AsynchIO::BufferBase *buff = result->getBuff();
    if (buff != 0) {
        writeInProgress = false;
        if (status == 0 && bytes > 0) {
            if (bytes < result->getRequested()) // Still more to go; resubmit
                startWrite(buff);
            else
                queueReadBuffer(buff);     // All done; back to the pool
        }
        else {
            // An error... if it's a connection close, ignore it - it will be
            // noticed and handled on a read completion any moment now.
            // What to do with real error??? Save the Buffer?
        }
    }

    // If there are no writes outstanding, check for more writes to initiate
    // (either queued or via idle). The opsInProgress count is handled in
    // completion()
    if (!writeInProgress) {
        bool writing = false;
        {
            QLock l(bufferQueueLock);
            if (writeQueue.size() > 0) {
                buff = writeQueue.front();
                assert(buff);
                writeQueue.pop_front();
                startWrite(buff);
                writing = true;
            }
        }
        if (!writing && !queuedClose) {
            notifyIdle();
        }
    }
    return;
}

void AsynchIO::completion(AsynchIoResult *result) {
    {
        ScopedLock<Mutex> l(completionLock);
        if (working) {
            completionQueue.push(result);
            return;
        }

        // First thread in with something to do; note we're working then keep
        // handling completions.
        working = true;
        while (result != 0) {
            // New scope to unlock temporarily.
            {
                ScopedUnlock<Mutex> ul(completionLock);
                AsynchReadResult *r = dynamic_cast<AsynchReadResult*>(result);
                if (r != 0)
                    readComplete(r);
                else {
                    AsynchWriteResult *w =
                        dynamic_cast<AsynchWriteResult*>(result);
                    if (w != 0)
                        writeComplete(w);
                    else {
                        AsynchCallbackRequest *req =
                          dynamic_cast<AsynchCallbackRequest*>(result);
                        req->reqCallback(*this);
                    }
                }
                delete result;
                result = 0;
                InterlockedDecrement(&opsInProgress);
            }
            // Lock is held again.
            if (completionQueue.empty())
                continue;
            result = completionQueue.front();
            completionQueue.pop();
        }
        working = false;
    }
    // Lock released; ok to close if ops are done and close requested.
    // Layer above will call back to queueForDeletion() if it hasn't
    // already been done. If it already has, go ahead and delete.
    if (opsInProgress == 0) {
        if (queuedClose)
            // close() may cause a delete; don't trust 'this' on return
            close();
        else if (queuedDelete)
            delete this;
    }
}

} // namespace windows

AsynchIO* qpid::sys::AsynchIO::create(const Socket& s,
                                      AsynchIO::ReadCallback rCb,
                                      AsynchIO::EofCallback eofCb,
                                      AsynchIO::DisconnectCallback disCb,
                                      AsynchIO::ClosedCallback cCb,
                                      AsynchIO::BuffersEmptyCallback eCb,
                                      AsynchIO::IdleCallback iCb)
{
    return new qpid::sys::windows::AsynchIO(s, rCb, eofCb, disCb, cCb, eCb, iCb);
}

}}  // namespace qpid::sys
