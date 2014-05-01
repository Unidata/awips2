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
#include "qpid/sys/rdma/RdmaIO.h"

#include "qpid/log/Statement.h"


#include <iostream>
#include <boost/bind.hpp>

using qpid::sys::SocketAddress;
using qpid::sys::DispatchHandle;
using qpid::sys::Poller;

namespace Rdma {
    AsynchIO::AsynchIO(
            QueuePair::intrusive_ptr q,
            int size,
            int xCredit,
            int rCount,
            ReadCallback rc,
            IdleCallback ic,
            FullCallback fc,
            ErrorCallback ec
    ) :
        qp(q),
        dataHandle(*qp, boost::bind(&AsynchIO::dataEvent, this, _1), 0, 0),
        bufferSize(size),
        recvCredit(0),
        xmitCredit(xCredit),
        recvBufferCount(rCount),
        xmitBufferCount(xCredit),
        outstandingWrites(0),
        closed(false),
        deleting(false),
        state(IDLE),
        readCallback(rc),
        idleCallback(ic),
        fullCallback(fc),
        errorCallback(ec)
    {
        qp->nonblocking();
        qp->notifyRecv();
        qp->notifySend();

        // Prepost some recv buffers before we go any further
        for (int i = 0; i<recvBufferCount; ++i) {
            // Allocate recv buffer
            Buffer* b = qp->createBuffer(bufferSize);
            buffers.push_front(b);
            b->dataCount = b->byteCount;
            qp->postRecv(b);
        }

        for (int i = 0; i<xmitBufferCount; ++i) {
            // Allocate xmit buffer
            Buffer* b = qp->createBuffer(bufferSize);
            buffers.push_front(b);
            bufferQueue.push_front(b);
            b->dataCount = 0;
            b->dataStart = 0;
        }
    }

    AsynchIO::~AsynchIO() {
        // Warn if we are deleting whilst there are still unreclaimed write buffers
        if ( outstandingWrites>0 )
            QPID_LOG(error, "RDMA: qp=" << qp << ": Deleting queue before all write buffers finished");

        // Turn off callbacks (before doing the deletes)
        dataHandle.stopWatch();

        // The buffers ptr_deque automatically deletes all the buffers we've allocated
        // TODO: It might turn out to be more efficient in high connection loads to reuse the
        // buffers rather than having to reregister them all the time (this would be straightforward if all 
        // connections haver the same buffer size and harder otherwise)
    }

    void AsynchIO::start(Poller::shared_ptr poller) {
        dataHandle.startWatch(poller);
    }

    // Mark for deletion/Delete this object when we have no outstanding writes
    void AsynchIO::deferDelete() {
        State oldState;
        State newState;
        bool doReturn;
        //qpid::sys::ScopedLock<qpid::sys::Mutex> l(stateLock);
        // It is safe to assign to deleting here as we either delete ourselves
        // before leaving this function or deleting is set on exit
        do {
            newState = oldState = state.get();
            doReturn = false;
            if (outstandingWrites > 0 || oldState != IDLE) {
                deleting = true;
                doReturn = true;
            } else{
                newState = DELETED; // Stop any read callback before the dataHandle.stopWatch() in the destructor
            }
        } while (!state.boolCompareAndSwap(oldState, newState));
        if (doReturn) {
            return;
        }
        delete this;
    }

    void AsynchIO::queueWrite(Buffer* buff) {
        // Make sure we don't overrun our available buffers
        // either at our end or the known available at the peers end
        if (writable()) {
            // TODO: We might want to batch up sending credit
            if (recvCredit > 0) {
                int creditSent = recvCredit & ~FlagsMask;
                qp->postSend(creditSent, buff);
                recvCredit -= creditSent;
            } else {
                qp->postSend(buff);
            }
            ++outstandingWrites;
            --xmitCredit;
        } else {
            if (fullCallback) {
                fullCallback(*this, buff);
            } else {
                QPID_LOG(error, "RDMA: qp=" << qp << ": Write queue full, but no callback, throwing buffer away");
                returnBuffer(buff);
            }
        }
    }

    // Mark now closed (so we don't accept any more writes or make any idle callbacks)
    void AsynchIO::queueWriteClose() {
        // Don't think we actually need to lock here as transition is 1 way only to closed
        closed = true;
    }

    void AsynchIO::notifyPendingWrite() {
        // As notifyPendingWrite can be called on an arbitrary thread it must check whether we are processing or not.
        // If we are then we just return as we know that  we will eventually do the idle callback anyway.
        //
        // qpid::sys::ScopedLock<qpid::sys::Mutex> l(stateLock);
        // We can get here in any state (as the caller could be in any thread)
        State oldState;
        State newState;
        bool doReturn;
        do {
            newState = oldState = state.get();
            doReturn = false;
            switch (oldState) {
            case NOTIFY_WRITE:
            case PENDING_NOTIFY:
                // We only need to note a pending notify if we're already doing a notify as data processing
                // is always followed by write notification processing
                newState = PENDING_NOTIFY;
                doReturn = true;
                break;
            case PENDING_DATA:
                doReturn = true;
                break;
            case DATA:
                // Only need to return here as data processing will do the idleCallback itself anyway
                doReturn = true;
                break;
            case IDLE:
                newState = NOTIFY_WRITE;
                break;
            case DELETED:
                assert(oldState!=DELETED);
                doReturn = true;
            };
        } while (!state.boolCompareAndSwap(oldState, newState));
        if (doReturn) {
            return;
        }

        doWriteCallback();

        // Keep track of what we need to do so that we can release the lock
        enum {COMPLETION, NOTIFY, RETURN, EXIT} action;
        // If there was pending data whilst we were doing this, process it now
        //
        // Using NOTIFY_WRITE for both NOTIFY & COMPLETION is a bit strange, but we're making sure we get the
        // correct result if we reenter notifyPendingWrite(), in which case we want to
        // end up in PENDING_NOTIFY (entering dataEvent doesn't matter as it only checks
        // not IDLE)
        do {
            //qpid::sys::ScopedLock<qpid::sys::Mutex> l(stateLock);
            do {
                newState = oldState = state.get();
                action = RETURN; // Anything but COMPLETION
                switch (oldState) {
                case NOTIFY_WRITE:
                    newState = IDLE;
                    action = (action == COMPLETION) ? EXIT : RETURN;
                    break;
                case PENDING_DATA:
                    newState = NOTIFY_WRITE;
                    action = COMPLETION;
                    break;
                case PENDING_NOTIFY:
                    newState = NOTIFY_WRITE;
                    action = NOTIFY;
                    break;
                default:
                    assert(oldState!=IDLE && oldState!=DATA && oldState!=DELETED);
                    action = RETURN;
                }
            } while (!state.boolCompareAndSwap(oldState, newState));

            // Note we only get here if we were in the PENDING_DATA or PENDING_NOTIFY state
            // so that we do need to process completions or notifications now
            switch (action) {
            case COMPLETION:
                processCompletions();
                // Fall through
            case NOTIFY:
                doWriteCallback();
                break;
            case RETURN:
                return;
            case EXIT:
                // If we just processed completions we might need to delete ourselves
                if (deleting && outstandingWrites == 0) {
                    delete this;
                }
                return;
            }
        } while (true);
    }

    void AsynchIO::dataEvent(qpid::sys::DispatchHandle&) {
        // Keep track of writable notifications
        // qpid::sys::ScopedLock<qpid::sys::Mutex> l(stateLock);
        State oldState;
        State newState;
        bool doReturn;
        do {
            newState = oldState = state.get();
            doReturn = false;
            // We're already processing a notification
            switch (oldState) {
            case IDLE:
                newState  = DATA;
                break;
            default:
                // Can't get here in DATA state as that would violate the serialisation rules
                assert( oldState!=DATA );
                newState = PENDING_DATA;
                doReturn = true;
            }
        } while (!state.boolCompareAndSwap(oldState, newState));
        if (doReturn) {
            return;
        }

        processCompletions();

        //qpid::sys::ScopedLock<qpid::sys::Mutex> l(stateLock);
        do {
            newState = oldState = state.get();
            assert( oldState==DATA );
            newState = NOTIFY_WRITE;
        } while (!state.boolCompareAndSwap(oldState, newState));

        do {
            doWriteCallback();

            // qpid::sys::ScopedLock<qpid::sys::Mutex> l(stateLock);
            bool doBreak;
            do {
                newState = oldState = state.get();
                doBreak = false;
                if ( oldState==NOTIFY_WRITE ) {
                    newState = IDLE;
                    doBreak = true;
                } else {
                    // Can't get DATA/PENDING_DATA here as dataEvent cannot be reentered
                    assert( oldState==PENDING_NOTIFY );
                    newState = NOTIFY_WRITE;
                }
            } while (!state.boolCompareAndSwap(oldState, newState));
            if (doBreak) {
                break;
            }
        } while (true);

        // We might need to delete ourselves
        if (deleting && outstandingWrites == 0) {
            delete this;
        }
    }

    void AsynchIO::processCompletions() {
        QueuePair::intrusive_ptr q = qp->getNextChannelEvent();

        // Re-enable notification for queue:
        // This needs to happen before we could do anything that could generate more work completion
        // events (ie the callbacks etc. in the following).
        // This can't make us reenter this code as the handle attached to the completion queue will still be
        // disabled by the poller until we leave this code
        qp->notifyRecv();
        qp->notifySend();

        int recvEvents = 0;
        int sendEvents = 0;

        // If no event do nothing
        if (!q)
            return;

        assert(q == qp);

        // Repeat until no more events
        do {
            QueuePairEvent e(qp->getNextEvent());
            if (!e)
                break;

            ::ibv_wc_status status = e.getEventStatus();
            if (status != IBV_WC_SUCCESS) {
                errorCallback(*this);
                // TODO: Probably need to flush queues at this point
                return;
            }

            // Test if recv (or recv with imm)
            //::ibv_wc_opcode eventType = e.getEventType();
            Buffer* b = e.getBuffer();
            QueueDirection dir = e.getDirection();
            if (dir == RECV) {
                ++recvEvents;

                // Get our xmitCredit if it was sent
                bool dataPresent = true;
                if (e.immPresent() ) {
                    xmitCredit += (e.getImm() & ~FlagsMask);
                    dataPresent = ((e.getImm() & IgnoreData) == 0);
                }

                // if there was no data sent then the message was only to update our credit
                if ( dataPresent ) {
                    readCallback(*this, b);
                }

                // At this point the buffer has been consumed so put it back on the recv queue
                b->dataStart = 0;
                b->dataCount = 0;
                qp->postRecv(b);

                // Received another message
                ++recvCredit;

                // Send recvCredit if it is large enough (it will have got this large because we've not sent anything recently)
                if (recvCredit > recvBufferCount/2) {
                    // TODO: This should use RDMA write with imm as there might not ever be a buffer to receive this message
                    // but this is a little unlikely, as to get in this state we have to have received messages without sending any
                    // for a while so its likely we've received an credit update from the far side.
                    if (writable()) {
                        Buffer* ob = getBuffer();
                        // Have to send something as adapters hate it when you try to transfer 0 bytes
                        *reinterpret_cast< uint32_t* >(ob->bytes) = htonl(recvCredit);
                        ob->dataCount = sizeof(uint32_t);

                        int creditSent = recvCredit & ~FlagsMask;
                        qp->postSend(creditSent | IgnoreData, ob);
                        recvCredit -= creditSent;
                        ++outstandingWrites;
                        --xmitCredit;
                    } else {
                        QPID_LOG(warning, "RDMA: qp=" << qp << ":  Unable to send unsolicited credit");
                    }
                }
            } else {
                ++sendEvents;
                {
                qpid::sys::ScopedLock<qpid::sys::Mutex> l(bufferQueueLock);
                bufferQueue.push_front(b);
                }
                --outstandingWrites;
            }
        } while (true);

        // Not sure if this is expected or not 
        if (recvEvents == 0 && sendEvents == 0) {
            QPID_LOG(debug, "RDMA: qp=" << qp << ":  Got channel event with no recv/send completions");
        }
    }

    void AsynchIO::doWriteCallback() {
        // TODO: maybe don't call idle unless we're low on write buffers
        // Keep on calling the idle routine as long as we are writable and we got something to write last call
        while (writable()) {
            int xc = xmitCredit;
            idleCallback(*this);
            // Check whether we actually wrote anything
            if (xmitCredit == xc) {
                QPID_LOG(debug, "RDMA: qp=" << qp << ": Called for data, but got none: xmitCredit=" << xmitCredit);
                return;
            }
        }
    }

    Buffer* AsynchIO::getBuffer() {
        qpid::sys::ScopedLock<qpid::sys::Mutex> l(bufferQueueLock);
        assert(!bufferQueue.empty());
        Buffer* b = bufferQueue.front();
        bufferQueue.pop_front();
        b->dataCount = 0;
        b->dataStart = 0;
        return b;
    }

    void AsynchIO::returnBuffer(Buffer* b) {
        qpid::sys::ScopedLock<qpid::sys::Mutex> l(bufferQueueLock);
        bufferQueue.push_front(b);
        b->dataCount = 0;
        b->dataStart = 0;
    }

    ConnectionManager::ConnectionManager(
        ErrorCallback errc,
        DisconnectedCallback dc
    ) :
        ci(Connection::make()),
        handle(*ci, boost::bind(&ConnectionManager::event, this, _1), 0, 0),
        errorCallback(errc),
        disconnectedCallback(dc)
    {
        ci->nonblocking();
    }

    ConnectionManager::~ConnectionManager()
    {
        handle.stopWatch();
    }

    void ConnectionManager::start(Poller::shared_ptr poller) {
        startConnection(ci);
        handle.startWatch(poller);
    }

    void ConnectionManager::event(DispatchHandle&) {
        connectionEvent(ci);
    }

    Listener::Listener(
        const SocketAddress& src,
        const ConnectionParams& cp,
        EstablishedCallback ec,
        ErrorCallback errc,
        DisconnectedCallback dc,
        ConnectionRequestCallback crc
    ) :
        ConnectionManager(errc, dc),
        src_addr(src),
        checkConnectionParams(cp),
        connectionRequestCallback(crc),
        establishedCallback(ec)
    {
    }

    void Listener::startConnection(Connection::intrusive_ptr ci) {
        ci->bind(src_addr);
        ci->listen();
    }

    void Listener::connectionEvent(Connection::intrusive_ptr ci) {
        ConnectionEvent e(ci->getNextEvent());

        // If (for whatever reason) there was no event do nothing
        if (!e)
            return;

        // Important documentation ommision the new rdma_cm_id
        // you get from CONNECT_REQUEST has the same context info
        // as its parent listening rdma_cm_id
        ::rdma_cm_event_type eventType = e.getEventType();
        ::rdma_conn_param conn_param = e.getConnectionParam();
        Rdma::Connection::intrusive_ptr id = e.getConnection();

        switch (eventType) {
        case RDMA_CM_EVENT_CONNECT_REQUEST: {
            // Make sure peer has sent params we can use
            if (!conn_param.private_data || conn_param.private_data_len < sizeof(ConnectionParams)) {
                id->reject();
                break;
            } 
            ConnectionParams cp = *static_cast<const ConnectionParams*>(conn_param.private_data);

            // Reject if requested msg size is bigger than we allow
            if (cp.maxRecvBufferSize > checkConnectionParams.maxRecvBufferSize) {
                id->reject(&checkConnectionParams);
                break;
            }

            bool accept = true;
            if (connectionRequestCallback)
                accept = connectionRequestCallback(id, cp);

            if (accept) {
                // Accept connection
                cp.initialXmitCredit = checkConnectionParams.initialXmitCredit;
                id->accept(conn_param, &cp);
            } else {
                // Reject connection
                id->reject();
            }
            break;
        }
        case RDMA_CM_EVENT_ESTABLISHED:
            establishedCallback(id);
            break;
        case RDMA_CM_EVENT_DISCONNECTED:
            disconnectedCallback(id);
            break;
        case RDMA_CM_EVENT_CONNECT_ERROR:
            errorCallback(id, CONNECT_ERROR);
            break;
        default:
            // Unexpected response
            errorCallback(id, UNKNOWN);
            //std::cerr << "Warning: unexpected response to listen - " << eventType << "\n";
        }
    }

    Connector::Connector(
        const SocketAddress& dst,
        const ConnectionParams& cp,
        ConnectedCallback cc,
        ErrorCallback errc,
        DisconnectedCallback dc,
        RejectedCallback rc
    ) :
        ConnectionManager(errc, dc),
        dst_addr(dst),
        connectionParams(cp),
        rejectedCallback(rc),
        connectedCallback(cc)
    {
    }

    void Connector::startConnection(Connection::intrusive_ptr ci) {
        ci->resolve_addr(dst_addr);
    }

    void Connector::connectionEvent(Connection::intrusive_ptr ci) {
        ConnectionEvent e(ci->getNextEvent());

        // If (for whatever reason) there was no event do nothing
        if (!e)
            return;

        ::rdma_cm_event_type eventType = e.getEventType();
        ::rdma_conn_param conn_param = e.getConnectionParam();
        Rdma::Connection::intrusive_ptr id = e.getConnection();
        switch (eventType) {
        case RDMA_CM_EVENT_ADDR_RESOLVED:
            // RESOLVE_ADDR
            ci->resolve_route();
            break;
        case RDMA_CM_EVENT_ADDR_ERROR:
            // RESOLVE_ADDR
            errorCallback(ci, ADDR_ERROR);
            break;
        case RDMA_CM_EVENT_ROUTE_RESOLVED:
            // RESOLVE_ROUTE:
            ci->connect(&connectionParams);
            break;
        case RDMA_CM_EVENT_ROUTE_ERROR:
            // RESOLVE_ROUTE:
            errorCallback(ci, ROUTE_ERROR);
            break;
        case RDMA_CM_EVENT_CONNECT_ERROR:
            // CONNECTING
            errorCallback(ci, CONNECT_ERROR);
            break;
        case RDMA_CM_EVENT_UNREACHABLE:
            // CONNECTING
            errorCallback(ci, UNREACHABLE);
            break;
        case RDMA_CM_EVENT_REJECTED: {
            // CONNECTING
            // Extract private data from event
            assert(conn_param.private_data && conn_param.private_data_len >= sizeof(ConnectionParams));
            ConnectionParams cp = *static_cast<const ConnectionParams*>(conn_param.private_data);
            rejectedCallback(ci, cp);
            break;
        }
        case RDMA_CM_EVENT_ESTABLISHED: {
            // CONNECTING
            // Extract private data from event
            assert(conn_param.private_data && conn_param.private_data_len >= sizeof(ConnectionParams));
            ConnectionParams cp = *static_cast<const ConnectionParams*>(conn_param.private_data);
            connectedCallback(ci, cp);
            break;
        }
        case RDMA_CM_EVENT_DISCONNECTED:
            // ESTABLISHED
            disconnectedCallback(ci);
            break;
        default:
            QPID_LOG(warning, "RDMA: Unexpected event in connect: " << eventType);
        }
    }
}
