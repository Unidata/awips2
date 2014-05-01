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
#ifndef RDMA_FACTORIES_H
#define RDMA_FACTORIES_H

#include "qpid/sys/rdma/rdma_exception.h"

#include <rdma/rdma_cma.h>

#include <boost/shared_ptr.hpp>

namespace Rdma {
    // These allow us to use simple shared_ptrs to do ref counting
    void acker(::rdma_cm_event* e) throw ();
    void destroyEChannel(::rdma_event_channel* c) throw ();
    void destroyId(::rdma_cm_id* i) throw ();
    void deallocPd(::ibv_pd* p) throw ();
    void destroyCChannel(::ibv_comp_channel* c) throw ();
    void destroyCq(::ibv_cq* cq) throw ();
    void destroyQp(::ibv_qp* qp) throw ();

    inline boost::shared_ptr< ::rdma_event_channel > mkEChannel() {
        ::rdma_event_channel* c = CHECK_NULL(::rdma_create_event_channel());
        return boost::shared_ptr< ::rdma_event_channel >(c, destroyEChannel);
    }

    inline boost::shared_ptr< ::rdma_cm_id >
    mkId(::rdma_event_channel* ec, void* context, ::rdma_port_space ps) {
        ::rdma_cm_id* i;
        CHECK(::rdma_create_id(ec, &i, context, ps));
        return boost::shared_ptr< ::rdma_cm_id >(i, destroyId);
    }

    inline boost::shared_ptr< ::ibv_pd > allocPd(::ibv_context* c) {
        ::ibv_pd* pd = CHECK_NULL(ibv_alloc_pd(c));
        return boost::shared_ptr< ::ibv_pd >(pd, deallocPd);
    }

    inline boost::shared_ptr< ::ibv_comp_channel > mkCChannel(::ibv_context* c) {
        ::ibv_comp_channel* cc = CHECK_NULL(::ibv_create_comp_channel(c));
        return boost::shared_ptr< ::ibv_comp_channel >(cc, destroyCChannel);
    }

    inline boost::shared_ptr< ::ibv_cq >
    mkCq(::ibv_context* c, int cqe, void* context, ::ibv_comp_channel* cc) {
        ::ibv_cq* cq = CHECK_NULL(ibv_create_cq(c, cqe, context, cc, 0));
        return boost::shared_ptr< ::ibv_cq >(cq, destroyCq);
    }
}

#endif // RDMA_FACTORIES_H
