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
#include "qpid/sys/SocketAddress.h"
#include "qpid/sys/Poller.h"
#include "qpid/sys/DispatchHandle.h"
#include "qpid/sys/Time.h"
#include "qpid/log/Statement.h"

#include "qpid/sys/posix/check.h"

// TODO The basic algorithm here is not really POSIX specific and with a
// bit more abstraction could (should) be promoted to be platform portable
#include <unistd.h>
#include <sys/socket.h>
#include <signal.h>
#include <errno.h>
#include <string.h>

#include <boost/bind.hpp>
#include <boost/lexical_cast.hpp>

using namespace qpid::sys;

namespace {

struct StaticInit {
    StaticInit() {
        /**
         * Make *process* not generate SIGPIPE when writing to closed
         * pipe/socket (necessary as default action is to terminate process)
         */
        ::signal(SIGPIPE, SIG_IGN);
    };
} init;

/*
 * We keep per thread state to avoid locking overhead. The assumption is that
 * on average all the connections are serviced by all the threads so the state
 * recorded in each thread is about the same. If this turns out not to be the
 * case we could rebalance the info occasionally.  
 */
__thread int threadReadTotal = 0;
__thread int threadMaxRead = 0;
__thread int threadReadCount = 0;
__thread int threadWriteTotal = 0;
__thread int threadWriteCount = 0;
__thread int64_t threadMaxReadTimeNs = 2 * 1000000; // start at 2ms
}

/*
 * Asynch Acceptor
 */
namespace qpid {
namespace sys {
namespace posix {

class AsynchAcceptor : public qpid::sys::AsynchAcceptor {
public:
    AsynchAcceptor(const Socket& s, AsynchAcceptor::Callback callback);
    ~AsynchAcceptor();
    void start(Poller::shared_ptr poller);

private:
    void readable(DispatchHandle& handle);

private:
    AsynchAcceptor::Callback acceptedCallback;
    DispatchHandle handle;
    const Socket& socket;

};

AsynchAcceptor::AsynchAcceptor(const Socket& s,
                               AsynchAcceptor::Callback callback) :
    acceptedCallback(callback),
    handle(s, boost::bind(&AsynchAcceptor::readable, this, _1), 0, 0),
    socket(s) {

    s.setNonblocking();
}

AsynchAcceptor::~AsynchAcceptor() {
    handle.stopWatch();
}

void AsynchAcceptor::start(Poller::shared_ptr poller) {
    handle.startWatch(poller);
}

/*
 * We keep on accepting as long as there is something to accept
 */
void AsynchAcceptor::readable(DispatchHandle& h) {
    Socket* s;
    do {
        errno = 0;
        // TODO: Currently we ignore the peers address, perhaps we should
        // log it or use it for connection acceptance.
        try {
            s = socket.accept();
            if (s) {
                acceptedCallback(*s);
            } else {
                break;
            }
        } catch (const std::exception& e) {
            QPID_LOG(error, "Could not accept socket: " << e.what());
            break;
        }
    } while (true);

    h.rewatch();
}

/*
 * POSIX version of AsynchIO TCP socket connector.
 *
 * The class is implemented in terms of DispatchHandle to allow it to be
 * deleted by deleting the contained DispatchHandle.
 */
class AsynchConnector : public qpid::sys::AsynchConnector,
                        private DispatchHandle {

private:
    void connComplete(DispatchHandle& handle);
    void failure(int, const std::string&);

private:
    ConnectedCallback connCallback;
    FailedCallback failCallback;
    std::string errMsg;
    const Socket& socket;

public:
    AsynchConnector(const Socket& socket,
                    Poller::shared_ptr poller,
                    std::string hostname,
                    uint16_t port,
                    ConnectedCallback connCb,
                    FailedCallback failCb);
};

AsynchConnector::AsynchConnector(const Socket& s,
                                 Poller::shared_ptr poller,
                                 std::string hostname,
                                 uint16_t port,
                                 ConnectedCallback connCb,
                                 FailedCallback failCb) :
    DispatchHandle(s,
                   0,
                   boost::bind(&AsynchConnector::connComplete, this, _1),
                   boost::bind(&AsynchConnector::connComplete, this, _1)),
    connCallback(connCb),
    failCallback(failCb),
    socket(s)
{
    socket.setNonblocking();
    SocketAddress sa(hostname, boost::lexical_cast<std::string>(port));
    try {
        socket.connect(sa);
    } catch(std::exception& e) {
        // Defer reporting failure
        startWatch(poller);
        errMsg = e.what();
        DispatchHandle::call(boost::bind(&AsynchConnector::failure, this, -1, errMsg));
        return;
    }
    startWatch(poller);
}

void AsynchConnector::connComplete(DispatchHandle& h)
{
    int errCode = socket.getError();

    h.stopWatch();
    if (errCode == 0) {
        connCallback(socket);
        DispatchHandle::doDelete();
    } else {
        failure(errCode, strError(errCode));
    }
}

void AsynchConnector::failure(int errCode, const std::string& message)
{
    failCallback(socket, errCode, message);

    DispatchHandle::doDelete();
}

/*
 * POSIX version of AsynchIO reader/writer
 *
 * The class is implemented in terms of DispatchHandle to allow it to be
 * deleted by deleting the contained DispatchHandle.
 */
class AsynchIO : public qpid::sys::AsynchIO, private DispatchHandle {

public:
    AsynchIO(const Socket& s,
             ReadCallback rCb,
             EofCallback eofCb,
             DisconnectCallback disCb,
             ClosedCallback cCb = 0,
             BuffersEmptyCallback eCb = 0,
             IdleCallback iCb = 0);

    // Methods inherited from qpid::sys::AsynchIO

    virtual void queueForDeletion();

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
    virtual BufferBase* getQueuedBuffer();

private:
    ~AsynchIO();

    // Methods that are callback targets from Dispatcher.
    void readable(DispatchHandle& handle);
    void writeable(DispatchHandle& handle);
    void disconnected(DispatchHandle& handle);
    void requestedCall(RequestCallback);
    void close(DispatchHandle& handle);

private:
    ReadCallback readCallback;
    EofCallback eofCallback;
    DisconnectCallback disCallback;
    ClosedCallback closedCallback;
    BuffersEmptyCallback emptyCallback;
    IdleCallback idleCallback;
    const Socket& socket;
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
    /**
     * This records whether we've been reading is flow controlled:
     * it's safe as a simple boolean as the only way to be stopped
     * is in calls only allowed in the callback context, the only calls
     * checking it are also in calls only allowed in callback context.
     */
    volatile bool readingStopped;
};

AsynchIO::AsynchIO(const Socket& s,
                   ReadCallback rCb, EofCallback eofCb, DisconnectCallback disCb,
                   ClosedCallback cCb, BuffersEmptyCallback eCb, IdleCallback iCb) :

    DispatchHandle(s, 
                   boost::bind(&AsynchIO::readable, this, _1),
                   boost::bind(&AsynchIO::writeable, this, _1),
                   boost::bind(&AsynchIO::disconnected, this, _1)),
    readCallback(rCb),
    eofCallback(eofCb),
    disCallback(disCb),
    closedCallback(cCb),
    emptyCallback(eCb),
    idleCallback(iCb),
    socket(s),
    queuedClose(false),
    writePending(false),
    readingStopped(false) {

    s.setNonblocking();
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
    DispatchHandle::doDelete();
}

void AsynchIO::start(Poller::shared_ptr poller) {
    DispatchHandle::startWatch(poller);
}

void AsynchIO::queueReadBuffer(BufferBase* buff) {
    assert(buff);
    buff->dataStart = 0;
    buff->dataCount = 0;

    bool queueWasEmpty = bufferQueue.empty();
    bufferQueue.push_back(buff);
    if (queueWasEmpty && !readingStopped)
        DispatchHandle::rewatchRead();
}

void AsynchIO::unread(BufferBase* buff) {
    assert(buff);
    if (buff->dataStart != 0) {
        memmove(buff->bytes, buff->bytes+buff->dataStart, buff->dataCount);
        buff->dataStart = 0;
    }

    bool queueWasEmpty = bufferQueue.empty();
    bufferQueue.push_front(buff);
    if (queueWasEmpty && !readingStopped)
        DispatchHandle::rewatchRead();
}

void AsynchIO::queueWrite(BufferBase* buff) {
    assert(buff);
    // If we've already closed the socket then throw the write away
    if (queuedClose) {
        queueReadBuffer(buff);
        return;
    } else {
        writeQueue.push_front(buff);
    }
    writePending = false;
    DispatchHandle::rewatchWrite();
}

// This can happen outside the callback context
void AsynchIO::notifyPendingWrite() {
    writePending = true;
    DispatchHandle::rewatchWrite();
}

void AsynchIO::queueWriteClose() {
    queuedClose = true;
    DispatchHandle::rewatchWrite();
}

bool AsynchIO::writeQueueEmpty() {
    return writeQueue.empty();
}

// This can happen outside the callback context
void AsynchIO::startReading() {
    readingStopped = false;
    DispatchHandle::rewatchRead();
}

void AsynchIO::stopReading() {
    readingStopped = true;
    DispatchHandle::unwatchRead();
}

void AsynchIO::requestCallback(RequestCallback callback) {
    // TODO creating a function object every time isn't all that
    // efficient - if this becomes heavily used do something better (what?)
    assert(callback);
    DispatchHandle::call(boost::bind(&AsynchIO::requestedCall, this, callback));
}

void AsynchIO::requestedCall(RequestCallback callback) {
    assert(callback);
    callback(*this);
}

/** Return a queued buffer if there are enough
 * to spare
 */
AsynchIO::BufferBase* AsynchIO::getQueuedBuffer() {
    // Always keep at least one buffer (it might have data that was "unread" in it)
    if (bufferQueue.size()<=1)
        return 0;
    BufferBase* buff = bufferQueue.back();
    assert(buff);
    buff->dataStart = 0;
    buff->dataCount = 0;
    bufferQueue.pop_back();
    return buff;
}

/*
 * We keep on reading as long as we have something to read and a buffer to put
 * it in
 */
void AsynchIO::readable(DispatchHandle& h) {
    int readTotal = 0;
    AbsTime readStartTime = AbsTime::now();
    do {
        // (Try to) get a buffer
        if (!bufferQueue.empty()) {
            // Read into buffer
            BufferBase* buff = bufferQueue.front();
            assert(buff);
            bufferQueue.pop_front();
            errno = 0;
            int readCount = buff->byteCount-buff->dataCount;
            int rc = socket.read(buff->bytes + buff->dataCount, readCount);
            if (rc > 0) {
                buff->dataCount += rc;
                threadReadTotal += rc;
                readTotal += rc;

                readCallback(*this, buff);
                if (readingStopped) {
                    // We have been flow controlled.
                    break;
                }

                if (rc != readCount) {
                    // If we didn't fill the read buffer then time to stop reading
                    break;
                }
                
                // Stop reading if we've overrun our timeslot
                if (Duration(readStartTime, AbsTime::now()) > threadMaxReadTimeNs) {
                    break;
                }
                
            } else {
                // Put buffer back (at front so it doesn't interfere with unread buffers)
                bufferQueue.push_front(buff);
                assert(buff);
                
                // Eof or other side has gone away
                if (rc == 0 || errno == ECONNRESET) {
                    eofCallback(*this);
                    h.unwatchRead();
                    break;
                } else if (errno == EAGAIN) {
                    // We have just put a buffer back so we know
                    // we can carry on watching for reads
                    break;
                } else {
                    // Report error then just treat as a socket disconnect
                    QPID_LOG(error, "Error reading socket: " << qpid::sys::strError(errno) << "(" << errno << ")" );
                    eofCallback(*this);
                    h.unwatchRead();
                    break;
                }
            }
        } else {
            // Something to read but no buffer
            if (emptyCallback) {
                emptyCallback(*this);
            }
            // If we still have no buffers we can't do anything more
            if (bufferQueue.empty()) {
                h.unwatchRead();
                break;
            }
            
        }
    } while (true);

    ++threadReadCount;
    threadMaxRead = std::max(threadMaxRead, readTotal);
    return;
}

/*
 * We carry on writing whilst we have data to write and we can write
 */
void AsynchIO::writeable(DispatchHandle& h) {
    int writeTotal = 0;
    do {
        // See if we've got something to write
        if (!writeQueue.empty()) {
            // Write buffer
            BufferBase* buff = writeQueue.back();
            writeQueue.pop_back();
            errno = 0;
            assert(buff->dataStart+buff->dataCount <= buff->byteCount);
            int rc = socket.write(buff->bytes+buff->dataStart, buff->dataCount);
            if (rc >= 0) {
                threadWriteTotal += rc;
                writeTotal += rc;

                // If we didn't write full buffer put rest back
                if (rc != buff->dataCount) {
                    buff->dataStart += rc;
                    buff->dataCount -= rc;
                    writeQueue.push_back(buff);
                    break;
                }
                
                // Recycle the buffer
                queueReadBuffer(buff);
                
                // If we've already written more than the max for reading then stop
                // (this is to stop writes dominating reads) 
                if (writeTotal > threadMaxRead)
                    break;
            } else {
                // Put buffer back
                writeQueue.push_back(buff);
                if (errno == ECONNRESET || errno == EPIPE) {
                    // Just stop watching for write here - we'll get a
                    // disconnect callback soon enough
                    h.unwatchWrite();
                    break;
                } else if (errno == EAGAIN) {
                    // We have just put a buffer back so we know
                    // we can carry on watching for writes
                    break;
                } else {
                    QPID_POSIX_CHECK(rc);
                }
            }
        } else {
            // If we're waiting to close the socket then can do it now as there is nothing to write
            if (queuedClose) {
                close(h);
                break;
            }
            // Fd is writable, but nothing to write
            if (idleCallback) {
                writePending = false;
                idleCallback(*this);
            }
            // If we still have no buffers to write we can't do anything more
            if (writeQueue.empty() && !writePending && !queuedClose) {
                h.unwatchWrite();
                // The following handles the case where writePending is
                // set to true after the test above; in this case its
                // possible that the unwatchWrite overwrites the
                // desired rewatchWrite so we correct that here
                if (writePending)
                    h.rewatchWrite();
                break;
            }
        }
    } while (true);

    ++threadWriteCount;
    return;
}
        
void AsynchIO::disconnected(DispatchHandle& h) {
    // If we've already queued close do it instead of disconnected callback
    if (queuedClose) {
        close(h);
    } else if (disCallback) {
        disCallback(*this);
        h.unwatch();
    }
}

/*
 * Close the socket and callback to say we've done it
 */
void AsynchIO::close(DispatchHandle& h) {
    h.stopWatch();
    socket.close();
    if (closedCallback) {
        closedCallback(*this, socket);
    }
}

} // namespace posix

AsynchAcceptor* AsynchAcceptor::create(const Socket& s, 
                                       Callback callback)
{
    return new posix::AsynchAcceptor(s, callback);
}

AsynchConnector* AsynchConnector::create(const Socket& s,
                                         Poller::shared_ptr poller,
                                         std::string hostname,
                                         uint16_t port,
                                         ConnectedCallback connCb,
                                         FailedCallback failCb)
{
    return new posix::AsynchConnector(s, poller, hostname, port, connCb, failCb);
}

AsynchIO* AsynchIO::create(const Socket& s,
                           AsynchIO::ReadCallback rCb,
                           AsynchIO::EofCallback eofCb,
                           AsynchIO::DisconnectCallback disCb,
                           AsynchIO::ClosedCallback cCb,
                           AsynchIO::BuffersEmptyCallback eCb,
                           AsynchIO::IdleCallback iCb)
{
    return new posix::AsynchIO(s, rCb, eofCb, disCb, cCb, eCb, iCb);
}

}} // namespace qpid::sys
