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

#include "qpid/broker/SessionManager.h"
#include "qpid/broker/SessionState.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/log/Statement.h"
#include "qpid/log/Helpers.h"
#include "qpid/memory.h"

#include <boost/bind.hpp>
#include <boost/range.hpp>

#include <algorithm>
#include <functional>
#include <ostream>

namespace qpid {
namespace broker {

using boost::intrusive_ptr;
using namespace sys;
using namespace framing;

SessionManager::SessionManager(const SessionState::Configuration& c, Broker& b)
    : config(c), broker(b) {}

SessionManager::~SessionManager() {
    detached.clear();           // Must clear before destructor as session dtor will call forget()
}

std::auto_ptr<SessionState>  SessionManager::attach(SessionHandler& h, const SessionId& id, bool/*force*/) {
    Mutex::ScopedLock l(lock);
    eraseExpired();             // Clean up expired table
    std::pair<Attached::iterator, bool> insert = attached.insert(id);
    if (!insert.second)
        throw SessionBusyException(QPID_MSG("Session already attached: " << id));
    Detached::iterator i = std::find(detached.begin(), detached.end(), id);
    std::auto_ptr<SessionState> state;
    if (i == detached.end())  
        state.reset(new SessionState(broker, h, id, config));
    else {
        state.reset(detached.release(i).release());
        state->attach(h);
    }
    return state;
    // FIXME aconway 2008-04-29: implement force 
}

void  SessionManager::detach(std::auto_ptr<SessionState> session) {
    Mutex::ScopedLock l(lock);
    attached.erase(session->getId());
    session->detach();
    if (session->getTimeout() > 0) {
    session->expiry = AbsTime(now(),session->getTimeout()*TIME_SEC);
    if (session->mgmtObject != 0)
        session->mgmtObject->set_expireTime ((uint64_t) Duration (session->expiry));
        detached.push_back(session.release()); // In expiry order
    eraseExpired();
}
}

void SessionManager::forget(const SessionId& id) {
    Mutex::ScopedLock l(lock);
    attached.erase(id);
}

void SessionManager::eraseExpired() {
    // Called with lock held.
    if (!detached.empty()) {
        // This used to use a more elegant invocation of std::lower_bound
        // but violated the strict weak ordering rule which Visual Studio
        // enforced. See QPID-1424 for more info should you be tempted to
        // replace the loop with something more elegant.
        AbsTime now = AbsTime::now();
        Detached::iterator keep = detached.begin();
        while ((keep != detached.end()) && ((*keep).expiry < now))
            keep++;
        if (detached.begin() != keep) {
            QPID_LOG(debug, "Expiring sessions: " << log::formatList(detached.begin(), keep));
            detached.erase(detached.begin(), keep);
        }
    }
}

}} // namespace qpid::broker
