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

#include "qpid/sys/ssl/SslIo.h"
#include "qpid/sys/ssl/SslSocket.h"

#include "qpid/sys/Time.h"
#include "qpid/sys/posix/check.h"
#include "qpid/log/Statement.h"

// TODO The basic algorithm here is not really POSIX specific and with a bit more abstraction
// could (should) be promoted to be platform portable
#include <unistd.h>
#include <sys/socket.h>
#include <signal.h>
#include <errno.h>
#include <string.h>

#include <boost/bind.hpp>

using namespace qpid::sys;
using namespace qpid::sys::ssl;

namespace {

/*
 * Make *process* not generate SIGPIPE when writing to closed
 * pipe/socket (necessary as default action is to terminate process)
 */
void ignoreSigpipe() {
    ::signal(SIGPIPE, SIG_IGN);
}

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

SslAcceptor::SslAcceptor(const SslSocket& s, Callback callback) :
    acceptedCallback(callback),
    handle(s, boost::bind(&SslAcceptor::readable, this, _1), 0, 0),
    socket(s) {

    s.setNonblocking();
    ignoreSigpipe();
}

SslAcceptor::~SslAcceptor() 
{
    handle.stopWatch();
}

void SslAcceptor::start(Poller::shared_ptr poller) {
    handle.startWatch(poller);
}

/*
 * We keep on accepting as long as there is something to accept
 */
void SslAcceptor::readable(DispatchHandle& h) {
    SslSocket* s;
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
        }
    } while (true);

    h.rewatch();
}

/*
 * Asynch Connector
 */

SslConnector::SslConnector(const SslSocket& s,
                                 Poller::shared_ptr poller,
                                 std::string hostname,
                                 uint16_t port,
                                 ConnectedCallback connCb,
                                 FailedCallback failCb) :
    DispatchHandle(s,
                   0,
                   boost::bind(&SslConnector::connComplete, this, _1),
                   boost::bind(&SslConnector::connComplete, this, _1)),
    connCallback(connCb),
    failCallback(failCb),
    socket(s)
{
    //TODO: would be better for connect to be performed on a
    //non-blocking socket, but that doesn't work at present so connect
    //blocks until complete
    try {
        socket.connect(hostname, port);
        socket.setNonblocking();
        startWatch(poller);
    } catch(std::exception& e) {
        failure(-1, std::string(e.what()));
    }
}

void SslConnector::connComplete(DispatchHandle& h)
{
    int errCode = socket.getError();

    h.stopWatch();
    if (errCode == 0) {
        connCallback(socket);
        DispatchHandle::doDelete();
    } else {
        // TODO: This need to be fixed as strerror isn't thread safe
        failure(errCode, std::string(::strerror(errCode)));
    }
}

void SslConnector::failure(int errCode, std::string message)
{
    if (failCallback)
        failCallback(errCode, message);

    socket.close();
    delete &socket;

    DispatchHandle::doDelete();
}

/*
 * Asynch reader/writer
 */
SslIO::SslIO(const SslSocket& s,
                   ReadCallback rCb, EofCallback eofCb, DisconnectCallback disCb,
                   ClosedCallback cCb, BuffersEmptyCallback eCb, IdleCallback iCb) :

    DispatchHandle(s, 
                   boost::bind(&SslIO::readable, this, _1),
                   boost::bind(&SslIO::writeable, this, _1),
                   boost::bind(&SslIO::disconnected, this, _1)),
    readCallback(rCb),
    eofCallback(eofCb),
    disCallback(disCb),
    closedCallback(cCb),
    emptyCallback(eCb),
    idleCallback(iCb),
    socket(s),
    queuedClose(false),
    writePending(false) {

    s.setNonblocking();
}

struct deleter
{
    template <typename T>
    void operator()(T *ptr){ delete ptr;}
};

SslIO::~SslIO() {
    std::for_each( bufferQueue.begin(), bufferQueue.end(), deleter());
    std::for_each( writeQueue.begin(), writeQueue.end(), deleter());
}

void SslIO::queueForDeletion() {
    DispatchHandle::doDelete();
}

void SslIO::start(Poller::shared_ptr poller) {
    DispatchHandle::startWatch(poller);
}

void SslIO::queueReadBuffer(BufferBase* buff) {
    assert(buff);
    buff->dataStart = 0;
    buff->dataCount = 0;
    bufferQueue.push_back(buff);
    DispatchHandle::rewatchRead();
}

void SslIO::unread(BufferBase* buff) {
    assert(buff);
    if (buff->dataStart != 0) {
        memmove(buff->bytes, buff->bytes+buff->dataStart, buff->dataCount);
        buff->dataStart = 0;
    }
    bufferQueue.push_front(buff);
    DispatchHandle::rewatchRead();
}

void SslIO::queueWrite(BufferBase* buff) {
    assert(buff);
    // If we've already closed the socket then throw the write away
    if (queuedClose) {
        bufferQueue.push_front(buff);
        return;
    } else {
        writeQueue.push_front(buff);
    }
    writePending = false;
    DispatchHandle::rewatchWrite();
}

void SslIO::notifyPendingWrite() {
    writePending = true;
    DispatchHandle::rewatchWrite();
}

void SslIO::queueWriteClose() {
    queuedClose = true;
    DispatchHandle::rewatchWrite();
}

/** Return a queued buffer if there are enough
 * to spare
 */
SslIO::BufferBase* SslIO::getQueuedBuffer() {
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
void SslIO::readable(DispatchHandle& h) {
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
                    QPID_LOG(error, "Error reading socket: " << qpid::sys::strError(rc) << "(" << rc << ")" );
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
void SslIO::writeable(DispatchHandle& h) {
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
        
void SslIO::disconnected(DispatchHandle& h) {
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
void SslIO::close(DispatchHandle& h) {
    h.stopWatch();
    socket.close();
    if (closedCallback) {
        closedCallback(*this, socket);
    }
}

int SslIO::getKeyLen() {return socket.getKeyLen();}
