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
#include "qpid/client/FailoverListener.h"
#include "qpid/client/Session.h"
#include "qpid/framing/Uuid.h"
#include "qpid/log/Statement.h"
#include "qpid/log/Helpers.h"

namespace qpid {
namespace client {

const std::string FailoverListener::AMQ_FAILOVER("amq.failover");

FailoverListener::FailoverListener(Connection c) :
    connection(c),
    session(c.newSession(AMQ_FAILOVER+"."+framing::Uuid(true).str())),
    subscriptions(session)
{
    knownBrokers = c.getInitialBrokers();
    if (session.exchangeQuery(arg::name=AMQ_FAILOVER).getNotFound()) {
        session.close();
        return;
    }
    std::string qname=session.getId().getName();
    session.queueDeclare(arg::queue=qname, arg::exclusive=true, arg::autoDelete=true);
    session.exchangeBind(arg::queue=qname, arg::exchange=AMQ_FAILOVER);
    subscriptions.subscribe(*this, qname, SubscriptionSettings(FlowControl::unlimited(),
                                                                ACCEPT_MODE_NONE));
    thread = sys::Thread(*this);
}

void FailoverListener::run() {
    try {
        subscriptions.run();
    } catch(...) {}
}

FailoverListener::~FailoverListener() {
    try {
        subscriptions.stop();
        thread.join();
        if (connection.isOpen()) {
            session.sync();
            session.close();
        }
    } catch (...) {}
}

void FailoverListener::received(Message& msg) {
    sys::Mutex::ScopedLock l(lock);
    knownBrokers = getKnownBrokers(msg);
}

std::vector<Url> FailoverListener::getKnownBrokers() const {
    sys::Mutex::ScopedLock l(lock);
    return knownBrokers;
}

std::vector<Url> FailoverListener::getKnownBrokers(const Message& msg) {
    std::vector<Url> knownBrokers;
    framing::Array urlArray;
    msg.getHeaders().getArray("amq.failover", urlArray);
    for (framing::Array::ValueVector::const_iterator i = urlArray.begin();
         i != urlArray.end();
         ++i ) 
        knownBrokers.push_back(Url((*i)->get<std::string>()));
    return knownBrokers;
}


}} // namespace qpid::client
