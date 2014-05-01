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
#ifndef RDMA_WRAP_H
#define RDMA_WRAP_H

#include "qpid/sys/rdma/rdma_factories.h"

#include <rdma/rdma_cma.h>

#include "qpid/RefCounted.h"
#include "qpid/sys/IOHandle.h"
#include "qpid/sys/posix/PrivatePosix.h"

#include <fcntl.h>

#include <netdb.h>

#include <vector>
#include <algorithm>
#include <iostream>
#include <stdexcept>
#include <boost/shared_ptr.hpp>
#include <boost/intrusive_ptr.hpp>

namespace Rdma {
    const int DEFAULT_TIMEOUT = 2000; // 2 secs
    const int DEFAULT_BACKLOG = 100;
    const int DEFAULT_CQ_ENTRIES = 256;
    const int DEFAULT_WR_ENTRIES = 64;
    extern const ::rdma_conn_param DEFAULT_CONNECT_PARAM;

    int deviceCount();

    struct Buffer {
        friend class QueuePair;

        char* const bytes;
        const int32_t byteCount;
        int32_t dataStart;
        int32_t dataCount;

        Buffer(::ibv_pd* pd, char* const b, const int32_t s) :
            bytes(b),
            byteCount(s),
            dataStart(0),
            dataCount(0),
            mr(CHECK_NULL(::ibv_reg_mr(
                pd, bytes, byteCount,
                ::IBV_ACCESS_LOCAL_WRITE)))
        {}

        ~Buffer() {
            (void) ::ibv_dereg_mr(mr);
            delete [] bytes;
        }

    private:
        ::ibv_mr* mr;
    };

    class Connection;

    enum QueueDirection {
        NONE,
        SEND,
        RECV
    };

    class QueuePairEvent {
        boost::shared_ptr< ::ibv_cq > cq;
        ::ibv_wc wc;
        QueueDirection dir;

        friend class QueuePair;

        QueuePairEvent() :
            dir(NONE)
        {}

        QueuePairEvent(
            const ::ibv_wc& w,
            boost::shared_ptr< ::ibv_cq > c,
            QueueDirection d) :
            cq(c),
            wc(w),
            dir(d)
        {
            assert(dir != NONE);
        }

    public:
        operator bool() const {
            return dir != NONE;
        }

        bool immPresent() const {
            return wc.wc_flags & IBV_WC_WITH_IMM;
        }
        
        uint32_t getImm() const {
            return ntohl(wc.imm_data);
        }

        QueueDirection getDirection() const {
            return dir;
        }

        ::ibv_wc_opcode getEventType() const {
            return wc.opcode;
        }

        ::ibv_wc_status getEventStatus() const {
            return wc.status;
        }

        Buffer* getBuffer() const {
            Buffer* b = reinterpret_cast<Buffer*>(wc.wr_id);
            b->dataCount = wc.byte_len;
            return b;
        }
    };

    // Wrapper for a queue pair - this has the functionality for
    // putting buffers on the receive queue and for sending buffers
    // to the other end of the connection.
    class QueuePair : public qpid::sys::IOHandle, public qpid::RefCounted {
        boost::shared_ptr< ::ibv_pd > pd;
        boost::shared_ptr< ::ibv_comp_channel > cchannel;
        boost::shared_ptr< ::ibv_cq > scq;
        boost::shared_ptr< ::ibv_cq > rcq;
        boost::shared_ptr< ::ibv_qp > qp;
        int outstandingSendEvents;
        int outstandingRecvEvents;

        friend class Connection;

        QueuePair(boost::shared_ptr< ::rdma_cm_id > id);
        ~QueuePair();

    public:
        typedef boost::intrusive_ptr<QueuePair> intrusive_ptr;

        // Create a buffer to use for writing
        Buffer* createBuffer(int s) {
            return new Buffer(pd.get(), new char[s], s);
        }

        // Make channel non-blocking by making
        // associated fd nonblocking
        void nonblocking() {
            ::fcntl(cchannel->fd, F_SETFL, O_NONBLOCK);
        }

        // If we get EAGAIN because the channel has been set non blocking
        // and we'd have to wait then return an empty event
        QueuePair::intrusive_ptr getNextChannelEvent() {
            // First find out which cq has the event
            ::ibv_cq* cq;
            void* ctx;
            int rc = ::ibv_get_cq_event(cchannel.get(), &cq, &ctx);
            if (rc == -1 && errno == EAGAIN)
                return 0;
            CHECK(rc);

            // Batch acknowledge the event
            if (cq == scq.get()) {
                if (++outstandingSendEvents > DEFAULT_CQ_ENTRIES / 2) {
                    ::ibv_ack_cq_events(cq, outstandingSendEvents);
                    outstandingSendEvents = 0;
                }
            } else if (cq == rcq.get()) {
                if (++outstandingRecvEvents > DEFAULT_CQ_ENTRIES / 2) {
                    ::ibv_ack_cq_events(cq, outstandingRecvEvents);
                    outstandingRecvEvents = 0;
                }
            }

            return static_cast<QueuePair*>(ctx);
        }

        QueuePairEvent getNextEvent() {
            ::ibv_wc w;
            if (::ibv_poll_cq(scq.get(), 1, &w) == 1)
                return QueuePairEvent(w, scq, SEND);
            else if (::ibv_poll_cq(rcq.get(), 1, &w) == 1)
                return QueuePairEvent(w, rcq, RECV);
            else
                return QueuePairEvent();
        }

        void postRecv(Buffer* buf);
        void postSend(Buffer* buf);
        void postSend(uint32_t imm, Buffer* buf);
        void notifyRecv();
        void notifySend();
    };

    class ConnectionEvent {
        friend class Connection;

        // The order of the members is important as we have to acknowledge
        // the event before destroying the ids on destruction
        boost::intrusive_ptr<Connection> id;
        boost::intrusive_ptr<Connection> listen_id;
        boost::shared_ptr< ::rdma_cm_event > event;

        ConnectionEvent() {}
        ConnectionEvent(::rdma_cm_event* e);

        // Default copy, assignment and destructor ok
    public:
        operator bool() const {
            return event;
        }

        ::rdma_cm_event_type getEventType() const {
            return event->event;
        }

        ::rdma_conn_param getConnectionParam() const;

        boost::intrusive_ptr<Connection> getConnection () const {
            return id;
        }

        boost::intrusive_ptr<Connection> getListenId() const {
            return listen_id;
        }
    };

    // For the moment this is a fairly simple wrapper for rdma_cm_id.
    //
    // NB: It allocates a protection domain (pd) per connection which means that
    // registered buffers can't be shared between different connections
    // (this can only happen between connections on the same controller in any case,
    // so needs careful management if used)
    class Connection : public qpid::sys::IOHandle, public qpid::RefCounted {
        boost::shared_ptr< ::rdma_event_channel > channel;
        boost::shared_ptr< ::rdma_cm_id > id;
        QueuePair::intrusive_ptr qp;

        void* context;

        friend class ConnectionEvent;
        friend class QueuePair;

        // Wrap the passed in rdma_cm_id with a Connection
        // this basically happens only on connection request
        Connection(::rdma_cm_id* i) :
            qpid::sys::IOHandle(new qpid::sys::IOHandlePrivate),
            id(i, destroyId),
            context(0)
        {
            impl->fd = id->channel->fd;

            // Just overwrite the previous context as it will
            // have come from the listening connection
            if (i)
                i->context = this;
        }

        Connection() :
            qpid::sys::IOHandle(new qpid::sys::IOHandlePrivate),
            channel(mkEChannel()),
            id(mkId(channel.get(), this, RDMA_PS_TCP)),
            context(0)
        {
            impl->fd = channel->fd;
	}

        ~Connection() {
            // Reset the id context in case someone else has it
            id->context = 0;
        }

        // Default destructor fine

        void ensureQueuePair() {
            assert(id.get());

            // Only allocate a queue pair if there isn't one already
            if (qp)
                return;

            qp = new QueuePair(id);
        }

    public:
        typedef boost::intrusive_ptr<Connection> intrusive_ptr;

        static intrusive_ptr make() {
            return new Connection();
        }

        static intrusive_ptr find(::rdma_cm_id* i) {
            if (!i)
                return 0;
            Connection* id = static_cast< Connection* >(i->context);
            if (!id)
                throw std::logic_error("Couldn't find existing Connection");
            return id;
        }

        template <typename T>
        void addContext(T* c) {
            // Don't allow replacing context
            if (!context)
                context = c;
        }

        template <typename T>
        T* getContext() {
            return static_cast<T*>(context);
        }

        // Make channel non-blocking by making
        // associated fd nonblocking
        void nonblocking() {
            assert(id.get());
            ::fcntl(id->channel->fd, F_SETFL, O_NONBLOCK);
        }

        // If we get EAGAIN because the channel has been set non blocking
        // and we'd have to wait then return an empty event
        ConnectionEvent getNextEvent() {
            assert(id.get());
            ::rdma_cm_event* e;
            int rc = ::rdma_get_cm_event(id->channel, &e);
            if (rc == -1 && errno == EAGAIN)
                return ConnectionEvent();
            CHECK(rc);
            return ConnectionEvent(e);
        }

        void bind(qpid::sys::SocketAddress& src_addr) const {
            assert(id.get());
            CHECK(::rdma_bind_addr(id.get(), getAddrInfo(src_addr).ai_addr));
        }

        void listen(int backlog = DEFAULT_BACKLOG) const {
            assert(id.get());
            CHECK(::rdma_listen(id.get(), backlog));
        }

        void resolve_addr(
            qpid::sys::SocketAddress& dst_addr,
            int timeout_ms = DEFAULT_TIMEOUT) const
        {
            assert(id.get());
            CHECK(::rdma_resolve_addr(id.get(), 0, getAddrInfo(dst_addr).ai_addr, timeout_ms));
        }

        void resolve_route(int timeout_ms = DEFAULT_TIMEOUT) const {
            assert(id.get());
            CHECK(::rdma_resolve_route(id.get(), timeout_ms));
        }

        void disconnect() const {
            assert(id.get());
            CHECK(::rdma_disconnect(id.get()));
        }

        // TODO: Currently you can only connect with the default connection parameters
        void connect() {
            assert(id.get());

            // Need to have a queue pair before we can connect
            ensureQueuePair();

            ::rdma_conn_param p = DEFAULT_CONNECT_PARAM;
            CHECK(::rdma_connect(id.get(), &p));
        }

        template <typename T>
        void connect(const T* data) {
            assert(id.get());
            // Need to have a queue pair before we can connect
            ensureQueuePair();

            ::rdma_conn_param p = DEFAULT_CONNECT_PARAM;
            p.private_data = data;
            p.private_data_len = sizeof(T);
            CHECK(::rdma_connect(id.get(), &p));
        }

        // TODO: Not sure how to default accept params - they come from the connection request
        // event
        template <typename T>
        void accept(const ::rdma_conn_param& param, const T* data) {
            assert(id.get());
            // Need to have a queue pair before we can accept
            ensureQueuePair();

            ::rdma_conn_param p = param;
            p.private_data = data;
            p.private_data_len = sizeof(T);
            CHECK(::rdma_accept(id.get(), &p));
        }

        void accept(const ::rdma_conn_param& param) {
            assert(id.get());
            // Need to have a queue pair before we can accept
            ensureQueuePair();

            ::rdma_conn_param p = param;
            p.private_data = 0;
            p.private_data_len = 0;
            CHECK(::rdma_accept(id.get(), &p));
        }

        template <typename T>
        void reject(const T* data) const {
            assert(id.get());
            CHECK(::rdma_reject(id.get(), data, sizeof(T)));
        }

        void reject() const {
            assert(id.get());
            CHECK(::rdma_reject(id.get(), 0, 0));
        }

        QueuePair::intrusive_ptr getQueuePair() {
            assert(id.get());

            ensureQueuePair();

            return qp;
        }
        
        std::string getLocalName() const {
            ::sockaddr* addr = ::rdma_get_local_addr(id.get());
            char hostName[NI_MAXHOST];
            char portName[NI_MAXSERV];
            CHECK_IBV(::getnameinfo(
                addr, sizeof(::sockaddr_storage),
                hostName, sizeof(hostName),
                portName, sizeof(portName),
                NI_NUMERICHOST | NI_NUMERICSERV));
            std::string r(hostName);
            r += ":";
            r += portName;
            return r;
        }
        
        std::string getPeerName() const {
            ::sockaddr* addr = ::rdma_get_peer_addr(id.get());
            char hostName[NI_MAXHOST];
            char portName[NI_MAXSERV];
            CHECK_IBV(::getnameinfo(
                addr, sizeof(::sockaddr_storage),
                hostName, sizeof(hostName),
                portName, sizeof(portName),
                NI_NUMERICHOST | NI_NUMERICSERV));
            std::string r(hostName);
            r += ":";
            r += portName;
            return r;
        }
    };

    inline void QueuePair::notifyRecv() {
        CHECK_IBV(ibv_req_notify_cq(rcq.get(), 0));
    }

    inline void QueuePair::notifySend() {
        CHECK_IBV(ibv_req_notify_cq(scq.get(), 0));
    }

    inline ConnectionEvent::ConnectionEvent(::rdma_cm_event* e) :
        id((e->event != RDMA_CM_EVENT_CONNECT_REQUEST) ?
                Connection::find(e->id) : new Connection(e->id)),
        listen_id(Connection::find(e->listen_id)),
        event(e, acker)
    {}
}

std::ostream& operator<<(std::ostream& o, ::rdma_cm_event_type t);

#endif // RDMA_WRAP_H
