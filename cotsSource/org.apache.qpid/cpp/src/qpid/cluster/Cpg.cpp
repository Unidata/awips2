/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/cluster/Cpg.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Time.h"
#include "qpid/sys/posix/PrivatePosix.h"
#include "qpid/log/Statement.h"

#include <vector>
#include <limits>
#include <iterator>
#include <sstream>

#include <unistd.h>

// This is a macro instead of a function because we don't want to
// evaluate the MSG argument unless there is an error.
#define CPG_CHECK(RESULT, MSG) \
    if ((RESULT) != CPG_OK) throw Exception(errorStr((RESULT), (MSG)))

namespace qpid {
namespace cluster {

using namespace std;



Cpg* Cpg::cpgFromHandle(cpg_handle_t handle) {
    void* cpg=0;
    CPG_CHECK(cpg_context_get(handle, &cpg), "Cannot get CPG instance.");
    if (!cpg) throw Exception("Cannot get CPG instance.");
    return reinterpret_cast<Cpg*>(cpg);
}

// Applies the same retry-logic to all cpg calls that need it.
void Cpg::callCpg ( CpgOp & c ) {
    cpg_error_t result;
    unsigned int snooze = 10;
    for ( unsigned int nth_try = 0; nth_try < cpgRetries; ++ nth_try ) {
        if ( CPG_OK == (result = c.op(handle, & group))) {
            QPID_LOG(info, c.opName << " successful.");
            break;
        }
        else if ( result == CPG_ERR_TRY_AGAIN ) {
            QPID_LOG(info, "Retrying " << c.opName );
            sys::usleep ( snooze );
            snooze *= 10;
            snooze = (snooze <= maxCpgRetrySleep) ? snooze : maxCpgRetrySleep;
        }
        else break;  // Don't retry unless CPG tells us to.
    }

    if ( result != CPG_OK )
        CPG_CHECK(result, c.msg(group));
}

// Global callback functions.
void Cpg::globalDeliver (
    cpg_handle_t handle,
    const struct cpg_name *group,
    uint32_t nodeid,
    uint32_t pid,
    void* msg,
    size_t msg_len)
{
    cpgFromHandle(handle)->handler.deliver(handle, group, nodeid, pid, msg, msg_len);
}

void Cpg::globalConfigChange(
    cpg_handle_t handle,
    const struct cpg_name *group,
    const struct cpg_address *members, size_t nMembers,
    const struct cpg_address *left, size_t nLeft,
    const struct cpg_address *joined, size_t nJoined
)
{
    cpgFromHandle(handle)->handler.configChange(handle, group, members, nMembers, left, nLeft, joined, nJoined);
}

void Cpg::globalDeliver (
    cpg_handle_t handle,
    struct cpg_name *group,
    uint32_t nodeid,
    uint32_t pid,
    void* msg,
    int msg_len)
{
    cpgFromHandle(handle)->handler.deliver(handle, group, nodeid, pid, msg, msg_len);
}

void Cpg::globalConfigChange(
    cpg_handle_t handle,
    struct cpg_name *group,
    struct cpg_address *members, int nMembers,
    struct cpg_address *left, int nLeft,
    struct cpg_address *joined, int nJoined
)
{
    cpgFromHandle(handle)->handler.configChange(handle, group, members, nMembers, left, nLeft, joined, nJoined);
}

int Cpg::getFd() {
    int fd;
    CPG_CHECK(cpg_fd_get(handle, &fd), "Cannot get CPG file descriptor");
    return fd;
}

Cpg::Cpg(Handler& h) : IOHandle(new sys::IOHandlePrivate), handler(h), isShutdown(false) {
    cpg_callbacks_t callbacks;
    ::memset(&callbacks, 0, sizeof(callbacks));
    callbacks.cpg_deliver_fn = &globalDeliver;
    callbacks.cpg_confchg_fn = &globalConfigChange;

    QPID_LOG(notice, "Initializing CPG");
    cpg_error_t err = cpg_initialize(&handle, &callbacks);
    int retries = 6; // FIXME aconway 2009-08-06: make this configurable.
    while (err == CPG_ERR_TRY_AGAIN && --retries) {
        QPID_LOG(notice, "Re-trying CPG initialization.");
        sys::sleep(5);
        err = cpg_initialize(&handle, &callbacks);
    }
    CPG_CHECK(err, "Failed to initialize CPG.");
    CPG_CHECK(cpg_context_set(handle, this), "Cannot set CPG context");
    // Note: CPG is currently unix-specific. If CPG is ported to
    // windows then this needs to be refactored into
    // qpid::sys::<platform>
    IOHandle::impl->fd = getFd();
}

Cpg::~Cpg() {
    try {
        shutdown();
    } catch (const std::exception& e) {
        QPID_LOG(error, "Error during CPG shutdown: " << e.what());
    }
}

void Cpg::join(const std::string& name) {
    group = name;
    callCpg ( cpgJoinOp );
}
    
void Cpg::leave() {
    callCpg ( cpgLeaveOp );
}




bool Cpg::mcast(const iovec* iov, int iovLen) {
    // Check for flow control
    cpg_flow_control_state_t flowState;
    CPG_CHECK(cpg_flow_control_state_get(handle, &flowState), "Cannot get CPG flow control status.");
    if (flowState == CPG_FLOW_CONTROL_ENABLED)
        return false;

    cpg_error_t result;
    do {
        result = cpg_mcast_joined(handle, CPG_TYPE_AGREED, const_cast<iovec*>(iov), iovLen);
        if (result != CPG_ERR_TRY_AGAIN) CPG_CHECK(result, cantMcastMsg(group));
    } while(result == CPG_ERR_TRY_AGAIN);
    return true;
}

void Cpg::shutdown() {
    if (!isShutdown) {
        QPID_LOG(debug,"Shutting down CPG");
        isShutdown=true;

        callCpg ( cpgFinalizeOp );
    }
}

void Cpg::dispatchOne() {
    CPG_CHECK(cpg_dispatch(handle,CPG_DISPATCH_ONE), "Error in CPG dispatch");
}

void Cpg::dispatchAll() {
    CPG_CHECK(cpg_dispatch(handle,CPG_DISPATCH_ALL), "Error in CPG dispatch");
}

void Cpg::dispatchBlocking() {
    CPG_CHECK(cpg_dispatch(handle,CPG_DISPATCH_BLOCKING), "Error in CPG dispatch");
}

string Cpg::errorStr(cpg_error_t err, const std::string& msg) {
    std::ostringstream  os;
    os << msg << ": ";
    switch (err) {
      case CPG_OK: os << "ok"; break;
      case CPG_ERR_LIBRARY: os << "library"; break;
      case CPG_ERR_TIMEOUT: os << "timeout"; break;
      case CPG_ERR_TRY_AGAIN: os << "try again"; break;
      case CPG_ERR_INVALID_PARAM: os << "invalid param"; break;
      case CPG_ERR_NO_MEMORY: os << "no memory"; break;
      case CPG_ERR_BAD_HANDLE: os << "bad handle"; break;
      case CPG_ERR_ACCESS: os << "access denied. You may need to set your group ID to 'ais'"; break;
      case CPG_ERR_NOT_EXIST: os << "not exist"; break;
      case CPG_ERR_EXIST: os << "exist"; break;
      case CPG_ERR_NOT_SUPPORTED: os << "not supported"; break;
      case CPG_ERR_SECURITY: os << "security"; break;
      case CPG_ERR_TOO_MANY_GROUPS: os << "too many groups"; break;
      default: os << ": unknown cpg error " << err;
    };
    os << " (" << err << ")";
    return os.str();
}

std::string Cpg::cantJoinMsg(const Name& group) {
    return "Cannot join CPG group "+group.str();
}

std::string Cpg::cantFinalizeMsg(const Name& group) {
    return "Cannot finalize CPG group "+group.str();
}

std::string Cpg::cantLeaveMsg(const Name& group) {
    return "Cannot leave CPG group "+group.str();
}

std::string Cpg::cantMcastMsg(const Name& group) {
    return "Cannot mcast to CPG group "+group.str();
}

MemberId Cpg::self() const {
    unsigned int nodeid;
    CPG_CHECK(cpg_local_get(handle, &nodeid), "Cannot get local CPG identity");
    return MemberId(nodeid, getpid());
}

namespace { int byte(uint32_t value, int i) { return (value >> (i*8)) & 0xff; } }

ostream& operator<<(ostream& out, const MemberId& id) {
    if (id.first) {
        out << byte(id.first, 0) << "."
            << byte(id.first, 1) << "."
            << byte(id.first, 2) << "."
            << byte(id.first, 3)
            << ":";
    }
    return out << id.second;
}

ostream& operator<<(ostream& o, const ConnectionId& c) {
    return o << c.first << "-" << c.second;
}

std::string MemberId::str() const  {
    char s[8];
    uint32_t x;
    x = htonl(first);
    ::memcpy(s, &x, 4);
    x = htonl(second);
    ::memcpy(s+4, &x, 4);
    return std::string(s,8);
}

MemberId::MemberId(const std::string& s) {
    uint32_t x;
    memcpy(&x, &s[0], 4);
    first = ntohl(x);
    memcpy(&x, &s[4], 4);
    second = ntohl(x);
}
}} // namespace qpid::cluster
