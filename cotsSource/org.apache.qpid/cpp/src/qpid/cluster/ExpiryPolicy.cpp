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

#include "qpid/broker/Message.h"
#include "qpid/cluster/ExpiryPolicy.h"
#include "qpid/cluster/Multicaster.h"
#include "qpid/framing/ClusterMessageExpiredBody.h"
#include "qpid/sys/Time.h"
#include "qpid/sys/Timer.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace cluster {

ExpiryPolicy::ExpiryPolicy(Multicaster& m, const MemberId& id, sys::Timer& t)
    : expiryId(0), expiredPolicy(new Expired), mcast(m), memberId(id), timer(t) {}

struct ExpiryTask : public sys::TimerTask {
    ExpiryTask(const boost::intrusive_ptr<ExpiryPolicy>& policy, uint64_t id, sys::AbsTime when)
        : TimerTask(when), expiryPolicy(policy), expiryId(id) {}
    void fire() { expiryPolicy->sendExpire(expiryId); }
    boost::intrusive_ptr<ExpiryPolicy> expiryPolicy;
    const uint64_t expiryId;
};

void ExpiryPolicy::willExpire(broker::Message& m) {
    uint64_t id = expiryId++;
    assert(unexpiredById.find(id) == unexpiredById.end());
    assert(unexpiredByMessage.find(&m) == unexpiredByMessage.end());
    unexpiredById[id] = &m;
    unexpiredByMessage[&m] = id;
    timer.add(new ExpiryTask(this, id, m.getExpiration()));
}

void ExpiryPolicy::forget(broker::Message& m) {
    MessageIdMap::iterator i = unexpiredByMessage.find(&m);
    assert(i != unexpiredByMessage.end());
    unexpiredById.erase(i->second);
    unexpiredByMessage.erase(i);
}

bool ExpiryPolicy::hasExpired(broker::Message& m) {
    return unexpiredByMessage.find(&m) == unexpiredByMessage.end();
}

void ExpiryPolicy::sendExpire(uint64_t id) {
    mcast.mcastControl(framing::ClusterMessageExpiredBody(framing::ProtocolVersion(), id), memberId);
}

void ExpiryPolicy::deliverExpire(uint64_t id) {
    IdMessageMap::iterator i = unexpiredById.find(id);
    if (i != unexpiredById.end()) {
        i->second->setExpiryPolicy(expiredPolicy); // hasExpired() == true; 
        unexpiredByMessage.erase(i->second);
        unexpiredById.erase(i);
    }
}

boost::optional<uint64_t> ExpiryPolicy::getId(broker::Message& m) {
    MessageIdMap::iterator i = unexpiredByMessage.find(&m);
    return i == unexpiredByMessage.end() ? boost::optional<uint64_t>() : i->second;
}

bool ExpiryPolicy::Expired::hasExpired(broker::Message&) { return true; }
void ExpiryPolicy::Expired::willExpire(broker::Message&) { }

}} // namespace qpid::cluster
