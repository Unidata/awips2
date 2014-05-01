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

#include "qpid/cluster/Quorum_cman.h"
#include "qpid/cluster/Cluster.h"
#include "qpid/log/Statement.h"
#include "qpid/Options.h"
#include "qpid/sys/Time.h"
#include "qpid/sys/posix/PrivatePosix.h"

namespace qpid {
namespace cluster {

namespace {

boost::function<void()> errorFn;

void cmanCallbackFn(cman_handle_t handle, void */*privdata*/, int reason, int /*arg*/) {
    if (reason == CMAN_REASON_STATECHANGE && !cman_is_quorate(handle)) {
        QPID_LOG(critical, "Lost contact with cluster quorum.");
        if (errorFn) errorFn();
        cman_stop_notification(handle);
    }
}
}

Quorum::Quorum(boost::function<void()> err) : enable(false), cman(0), cmanFd(0) {
    errorFn = err;
}

Quorum::~Quorum() {
    dispatchHandle.reset();
    if (cman) cman_finish(cman);
}

void Quorum::start(boost::shared_ptr<sys::Poller> p) {
    poller = p;
    enable = true;
    QPID_LOG(debug, "Connecting to quorum service.");
    cman = cman_init(0);
    if (cman == 0) throw ErrnoException("Can't connect to cman service");
    if (!cman_is_quorate(cman)) {
        QPID_LOG(notice, "Waiting for cluster quorum.");
        while(!cman_is_quorate(cman)) sys::sleep(5);
    }
    int err = cman_start_notification(cman, cmanCallbackFn);
    if (err != 0) throw ErrnoException("Can't register for cman notifications");
    watch(getFd());
}

void Quorum::watch(int fd) {
    cmanFd = fd;
    dispatchHandle.reset(
        new sys::DispatchHandleRef(
            sys::PosixIOHandle(cmanFd),
            boost::bind(&Quorum::dispatch, this, _1), // read
            0, // write
            boost::bind(&Quorum::disconnect, this, _1)  // disconnect
        ));
    dispatchHandle->startWatch(poller);
}

int Quorum::getFd() {
    int fd = cman_get_fd(cman);
    if (fd == 0) throw ErrnoException("Can't get cman file descriptor");
    return fd;
}

void Quorum::dispatch(sys::DispatchHandle&) {
    try {
        cman_dispatch(cman, CMAN_DISPATCH_ALL);
        int fd = getFd();
        if (fd != cmanFd) watch(fd);
    } catch (const std::exception& e) {
        QPID_LOG(critical, "Error in quorum dispatch: " << e.what());
        errorFn();
    }
}

void Quorum::disconnect(sys::DispatchHandle&) {
    QPID_LOG(critical, "Disconnected from quorum service");
    errorFn();
}

}} // namespace qpid::cluster
