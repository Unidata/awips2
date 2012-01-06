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
#include "qpid/cluster/PollerDispatch.h"

#include "qpid/log/Statement.h"
#include <boost/bind.hpp>

namespace qpid {
namespace cluster {

PollerDispatch::PollerDispatch(Cpg& c, boost::shared_ptr<sys::Poller> p,
                               boost::function<void()> e)
    : cpg(c), poller(p), onError(e),
      dispatchHandle(cpg,
                     boost::bind(&PollerDispatch::dispatch, this, _1), // read
                     0,         // write
                     boost::bind(&PollerDispatch::disconnect, this, _1) // disconnect
      ),
      started(false)
{}
    
PollerDispatch::~PollerDispatch() {
    if (started)
        dispatchHandle.stopWatch();
}

void PollerDispatch::start() {
    dispatchHandle.startWatch(poller);
    started = true;
}

// Entry point: called by IO to dispatch CPG events.
void PollerDispatch::dispatch(sys::DispatchHandle& h) {
    try {
        cpg.dispatchAll();
        h.rewatch();
    } catch (const std::exception& e) {
        QPID_LOG(critical, "Error in cluster dispatch: " << e.what());
        onError();
    }
}

// Entry point: called if disconnected from  CPG.
void PollerDispatch::disconnect(sys::DispatchHandle& ) {
    QPID_LOG(critical, "Disconnected from cluster");
    onError();
}

}} // namespace qpid::cluster
