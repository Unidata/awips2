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

#include "qpid/sys/rdma/rdma_wrap.h"

namespace Rdma {
    const ::rdma_conn_param DEFAULT_CONNECT_PARAM = {
        0,    // .private_data
        0,    // .private_data_len
        4,    // .responder_resources
        4,    // .initiator_depth
        0,    // .flow_control
        5,    // .retry_count
        7     // .rnr_retry_count
    };
    
    // This is moderately inefficient so don't use in a critical path
    int deviceCount() {
        int count;
        ::ibv_free_device_list(::ibv_get_device_list(&count));
        return count;
    }

    ::rdma_conn_param ConnectionEvent::getConnectionParam() const {
        // It's badly documented, but it seems from the librdma source code that all the following
        // event types have a valid param.conn
        switch (event->event) {
        case RDMA_CM_EVENT_CONNECT_REQUEST:
        case RDMA_CM_EVENT_ESTABLISHED:
        case RDMA_CM_EVENT_REJECTED:
        case RDMA_CM_EVENT_DISCONNECTED:
        case RDMA_CM_EVENT_CONNECT_ERROR:
            return event->param.conn;
        default:
            ::rdma_conn_param p = {};
            return p;
        }
    }

    QueuePair::QueuePair(boost::shared_ptr< ::rdma_cm_id > i) :
        qpid::sys::IOHandle(new qpid::sys::IOHandlePrivate),
        pd(allocPd(i->verbs)),
        cchannel(mkCChannel(i->verbs)),
        scq(mkCq(i->verbs, DEFAULT_CQ_ENTRIES, 0, cchannel.get())),
        rcq(mkCq(i->verbs, DEFAULT_CQ_ENTRIES, 0, cchannel.get())),
        outstandingSendEvents(0),
        outstandingRecvEvents(0)
    {
        impl->fd = cchannel->fd;

        // Set cq context to this QueuePair object so we can find
        // ourselves again
        scq->cq_context = this;
        rcq->cq_context = this;

        ::ibv_qp_init_attr qp_attr = {};

        // TODO: make a default struct for this
        qp_attr.cap.max_send_wr  = DEFAULT_WR_ENTRIES;
        qp_attr.cap.max_send_sge = 4;
        qp_attr.cap.max_recv_wr  = DEFAULT_WR_ENTRIES;
        qp_attr.cap.max_recv_sge = 4;

        qp_attr.send_cq      = scq.get();
        qp_attr.recv_cq      = rcq.get();
        qp_attr.qp_type      = IBV_QPT_RC;

        CHECK(::rdma_create_qp(i.get(), pd.get(), &qp_attr));
        qp = boost::shared_ptr< ::ibv_qp >(i->qp, destroyQp);

        // Set the qp context to this so we can find ourselves again
        qp->qp_context = this;
    }

    QueuePair::~QueuePair() {
        if (outstandingSendEvents > 0)
            ::ibv_ack_cq_events(scq.get(), outstandingSendEvents);
        if (outstandingRecvEvents > 0)
            ::ibv_ack_cq_events(rcq.get(), outstandingRecvEvents);

        // Reset back pointer in case someone else has the qp
        qp->qp_context = 0;
    }

    void QueuePair::postRecv(Buffer* buf) {
        ::ibv_recv_wr rwr = {};
        ::ibv_sge sge;

        sge.addr = (uintptr_t) buf->bytes+buf->dataStart;
        sge.length = buf->dataCount;
        sge.lkey = buf->mr->lkey;

        rwr.wr_id = reinterpret_cast<uint64_t>(buf);
        rwr.sg_list = &sge;
        rwr.num_sge = 1;

        ::ibv_recv_wr* badrwr = 0;
        CHECK_IBV(::ibv_post_recv(qp.get(), &rwr, &badrwr));
        if (badrwr)
            throw std::logic_error("ibv_post_recv(): Bad rwr");
    }

    void QueuePair::postSend(Buffer* buf) {
        ::ibv_send_wr swr = {};
        ::ibv_sge sge;

        sge.addr = (uintptr_t) buf->bytes+buf->dataStart;
        sge.length = buf->dataCount;
        sge.lkey = buf->mr->lkey;

        swr.wr_id = reinterpret_cast<uint64_t>(buf);
        swr.opcode = IBV_WR_SEND;
        swr.send_flags = IBV_SEND_SIGNALED;
        swr.sg_list = &sge;
        swr.num_sge = 1;

        ::ibv_send_wr* badswr = 0;
        CHECK_IBV(::ibv_post_send(qp.get(), &swr, &badswr));
        if (badswr)
            throw std::logic_error("ibv_post_send(): Bad swr");
    }

    void QueuePair::postSend(uint32_t imm, Buffer* buf) {
        ::ibv_send_wr swr = {};
        ::ibv_sge sge;

        sge.addr = (uintptr_t) buf->bytes+buf->dataStart;
        sge.length = buf->dataCount;
        sge.lkey = buf->mr->lkey;
        swr.send_flags = IBV_SEND_SIGNALED;        

        swr.wr_id = reinterpret_cast<uint64_t>(buf);
        swr.imm_data = htonl(imm);
        swr.opcode = IBV_WR_SEND_WITH_IMM;
        swr.sg_list = &sge;
        swr.num_sge = 1;

        ::ibv_send_wr* badswr = 0;
        CHECK_IBV(::ibv_post_send(qp.get(), &swr, &badswr));
        if (badswr)
            throw std::logic_error("ibv_post_send(): Bad swr");
    }
}

std::ostream& operator<<(std::ostream& o, ::rdma_cm_event_type t) {
#   define CHECK_TYPE(t) case t: o << #t; break;
    switch(t) {
        CHECK_TYPE(RDMA_CM_EVENT_ADDR_RESOLVED)
        CHECK_TYPE(RDMA_CM_EVENT_ADDR_ERROR)
        CHECK_TYPE(RDMA_CM_EVENT_ROUTE_RESOLVED)
        CHECK_TYPE(RDMA_CM_EVENT_ROUTE_ERROR)
        CHECK_TYPE(RDMA_CM_EVENT_CONNECT_REQUEST)
        CHECK_TYPE(RDMA_CM_EVENT_CONNECT_RESPONSE)
        CHECK_TYPE(RDMA_CM_EVENT_CONNECT_ERROR)
        CHECK_TYPE(RDMA_CM_EVENT_UNREACHABLE)
        CHECK_TYPE(RDMA_CM_EVENT_REJECTED)
        CHECK_TYPE(RDMA_CM_EVENT_ESTABLISHED)
        CHECK_TYPE(RDMA_CM_EVENT_DISCONNECTED)
        CHECK_TYPE(RDMA_CM_EVENT_DEVICE_REMOVAL)
        CHECK_TYPE(RDMA_CM_EVENT_MULTICAST_JOIN)
        CHECK_TYPE(RDMA_CM_EVENT_MULTICAST_ERROR)
    default:
         o << "UNKNOWN_EVENT";
    }
#   undef CHECK_TYPE
    return o;
}
