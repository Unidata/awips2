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
#include "qpid/sys/rdma/rdma_factories.h"

namespace Rdma {
    void acker(::rdma_cm_event* e) throw () {
        if (e)
            // Intentionally ignore return value - we can't do anything about it here
            (void) ::rdma_ack_cm_event(e);
    }

    void destroyEChannel(::rdma_event_channel* c) throw () {
        if (c)
            // Intentionally ignore return value - we can't do anything about it here
            (void) ::rdma_destroy_event_channel(c);
    }

    void destroyId(::rdma_cm_id* i) throw () {
        if (i)
            // Intentionally ignore return value - we can't do anything about it here
            (void) ::rdma_destroy_id(i);
    }

    void deallocPd(::ibv_pd* p) throw () {
        if (p)
            // Intentionally ignore return value - we can't do anything about it here
            (void) ::ibv_dealloc_pd(p);
    }

    void destroyCChannel(::ibv_comp_channel* c) throw () {
        if (c)
            // Intentionally ignore return value - we can't do anything about it here
            (void) ::ibv_destroy_comp_channel(c);
    }

    void destroyCq(::ibv_cq* cq) throw () {
        if (cq)
            (void) ::ibv_destroy_cq(cq);
    }

    void destroyQp(::ibv_qp* qp) throw () {
        if (qp)
            (void) ::ibv_destroy_qp(qp);
    }

}
