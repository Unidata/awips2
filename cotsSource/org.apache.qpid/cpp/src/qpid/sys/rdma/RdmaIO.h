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
#ifndef Rdma_Acceptor_h
#define Rdma_Acceptor_h

#include "qpid/sys/rdma/rdma_wrap.h"

#include "qpid/sys/AtomicValue.h"
#include "qpid/sys/Dispatcher.h"
#include "qpid/sys/DispatchHandle.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/SocketAddress.h"

#include <netinet/in.h>

#include <boost/function.hpp>
#include <boost/ptr_container/ptr_deque.hpp>
#include <deque>

namespace Rdma {

    class Connection;

    class AsynchIO
    {
        typedef boost::function1<void, AsynchIO&> ErrorCallback;
        typedef boost::function2<void, AsynchIO&, Buffer*> ReadCallback;
        typedef boost::function1<void, AsynchIO&>  IdleCallback;
        typedef boost::function2<void, AsynchIO&, Buffer*>  FullCallback;

        QueuePair::intrusive_ptr qp;
        qpid::sys::DispatchHandleRef dataHandle;
        int bufferSize;
        int recvCredit;
        int xmitCredit;
        int recvBufferCount;
        int xmitBufferCount;
        int outstandingWrites;
        bool closed; // TODO: Perhaps (probably) this state can be merged with the following...
        bool deleting; // TODO: Perhaps (probably) this state can be merged with the following...
        enum State { IDLE, DATA, PENDING_DATA, NOTIFY_WRITE, PENDING_NOTIFY, DELETED };
        qpid::sys::AtomicValue<State> state;
        //qpid::sys::Mutex stateLock;
        std::deque<Buffer*> bufferQueue;
        qpid::sys::Mutex bufferQueueLock;
        boost::ptr_deque<Buffer> buffers;

        ReadCallback readCallback;
        IdleCallback idleCallback;
        FullCallback fullCallback;
        ErrorCallback errorCallback;

    public:
        // TODO: Instead of specifying a buffer size specify the amount of memory the AsynchIO class can use
        // for buffers both read and write (allocate half to each up front) and fail if we cannot allocate that much
        // locked memory
        AsynchIO(
            QueuePair::intrusive_ptr q,
            int size,
            int xCredit,
            int rCount,
            ReadCallback rc,
            IdleCallback ic,
            FullCallback fc,
            ErrorCallback ec
        );

        void start(qpid::sys::Poller::shared_ptr poller);
        bool writable() const;
        bool bufferAvailable() const;
        void queueWrite(Buffer* buff);
        void notifyPendingWrite();
        void queueWriteClose();
        void deferDelete();
        int incompletedWrites() const;
        Buffer* getBuffer();
        void returnBuffer(Buffer*);

    private:
        // Don't let anyone else delete us to make sure there can't be outstanding callbacks
        ~AsynchIO();

        // Constants for the peer-peer command messages
        // These are sent in the high bits if the imm data of an rdma message
        // The low bits are used to send the credit
        const static int FlagsMask = 0x10000000; // Mask for all flag bits - be sure to update this if you add more command bits
        const static int IgnoreData = 0x10000000; // Message contains no application data

        void dataEvent(qpid::sys::DispatchHandle& handle);
        void processCompletions();
        void doWriteCallback();
    };

    inline bool AsynchIO::writable() const {
        return (!closed && outstandingWrites < xmitBufferCount && xmitCredit > 0);
    }

    inline int AsynchIO::incompletedWrites() const {
        return outstandingWrites;
    }

    inline bool AsynchIO::bufferAvailable() const {
        return !bufferQueue.empty();
    }
    // These are the parameters necessary to start the conversation
    // * Each peer HAS to allocate buffers of the size of the maximum receive from its peer
    // * Each peer HAS to know the initial "credit" it has for transmitting to its peer 
    struct ConnectionParams {
        int maxRecvBufferSize;
        int initialXmitCredit ;

        ConnectionParams(int s, int c) :
            maxRecvBufferSize(s),
            initialXmitCredit(c)
        {}
    };

    enum ErrorType {
        ADDR_ERROR,
        ROUTE_ERROR,
        CONNECT_ERROR,
        UNREACHABLE,
        UNKNOWN
    };

    typedef boost::function2<void, Rdma::Connection::intrusive_ptr&, ErrorType> ErrorCallback;
    typedef boost::function1<void, Rdma::Connection::intrusive_ptr&> DisconnectedCallback;

    class ConnectionManager {
        Connection::intrusive_ptr ci;
        qpid::sys::DispatchHandle handle;

    protected:
        ErrorCallback errorCallback;
        DisconnectedCallback disconnectedCallback;

   public:
        ConnectionManager(
            ErrorCallback errc,
            DisconnectedCallback dc
        );

        virtual ~ConnectionManager();

        void start(qpid::sys::Poller::shared_ptr poller);

    private:
        void event(qpid::sys::DispatchHandle& handle);

        virtual void startConnection(Connection::intrusive_ptr ci) = 0;
        virtual void connectionEvent(Connection::intrusive_ptr ci) = 0;
    };

    typedef boost::function2<bool, Rdma::Connection::intrusive_ptr&, const ConnectionParams&> ConnectionRequestCallback;
    typedef boost::function1<void, Rdma::Connection::intrusive_ptr&> EstablishedCallback;

    class Listener : public ConnectionManager
    {
        qpid::sys::SocketAddress src_addr;
        ConnectionParams checkConnectionParams;
        ConnectionRequestCallback connectionRequestCallback;
        EstablishedCallback establishedCallback;

    public:
        Listener(
            const qpid::sys::SocketAddress& src,
            const ConnectionParams& cp,
            EstablishedCallback ec,
            ErrorCallback errc,
            DisconnectedCallback dc,
            ConnectionRequestCallback crc = 0
        );

    private:
        void startConnection(Connection::intrusive_ptr ci);
        void connectionEvent(Connection::intrusive_ptr ci);
    };

    typedef boost::function2<void, Rdma::Connection::intrusive_ptr&, const ConnectionParams&> RejectedCallback;
    typedef boost::function2<void, Rdma::Connection::intrusive_ptr&, const ConnectionParams&> ConnectedCallback;

    class Connector : public ConnectionManager
    {
        qpid::sys::SocketAddress dst_addr;
        ConnectionParams connectionParams;
        RejectedCallback rejectedCallback;
        ConnectedCallback connectedCallback;

    public:
        Connector(
            const qpid::sys::SocketAddress& dst,
            const ConnectionParams& cp,
            ConnectedCallback cc,
            ErrorCallback errc,
            DisconnectedCallback dc,
            RejectedCallback rc = 0
        );

    private:
        void startConnection(Connection::intrusive_ptr ci);
        void connectionEvent(Connection::intrusive_ptr ci);
    };
}

#endif // Rdma_Acceptor_h
