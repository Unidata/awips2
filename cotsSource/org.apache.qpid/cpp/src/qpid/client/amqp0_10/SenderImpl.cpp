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
#include "SenderImpl.h"
#include "MessageSink.h"
#include "SessionImpl.h"
#include "AddressResolution.h"
#include "OutgoingMessage.h"
#include "qpid/messaging/Session.h"

namespace qpid {
namespace client {
namespace amqp0_10 {

SenderImpl::SenderImpl(SessionImpl& _parent, const std::string& _name, 
                       const qpid::messaging::Address& _address) : 
    parent(_parent), name(_name), address(_address), state(UNRESOLVED),
    capacity(50), window(0), flushed(false) {}

void SenderImpl::send(const qpid::messaging::Message& message) 
{
    Send f(*this, &message);
    while (f.repeat) parent.execute(f);
}

void SenderImpl::cancel()
{
    execute<Cancel>();
}

void SenderImpl::setCapacity(uint32_t c)
{
    bool flush = c < capacity;
    capacity = c;
    execute1<CheckPendingSends>(flush);
}
uint32_t SenderImpl::getCapacity() { return capacity; }
uint32_t SenderImpl::pending()
{
    CheckPendingSends f(*this, false);
    parent.execute(f);
    return f.pending;
} 

void SenderImpl::init(qpid::client::AsyncSession s, AddressResolution& resolver)
{
    session = s;
    if (state == UNRESOLVED) {
        sink = resolver.resolveSink(session, address);
        state = ACTIVE;
    }
    if (state == CANCELLED) {
        sink->cancel(session, name);
        parent.senderCancelled(name);
    } else {
        sink->declare(session, name);
        replay();
    }
}

void SenderImpl::waitForCapacity() 
{
    //TODO: add option to throw exception rather than blocking?
    if (capacity <= (flushed ? checkPendingSends(false) : outgoing.size())) {
        //Initial implementation is very basic. As outgoing is
        //currently only reduced on receiving completions and we are
        //blocking anyway we may as well sync(). If successful that
        //should clear all outstanding sends.
        session.sync();
        checkPendingSends(false);
    }
    //flush periodically and check for conmpleted sends
    if (++window > (capacity / 4)) {//TODO: make this configurable?
        checkPendingSends(true);
        window = 0;
    }
}

void SenderImpl::sendImpl(const qpid::messaging::Message& m) 
{
    //TODO: make recording for replay optional (would still want to track completion however)
    std::auto_ptr<OutgoingMessage> msg(new OutgoingMessage());
    msg->convert(m);
    msg->setSubject(m.getSubject().empty() ? address.getSubject() : m.getSubject());
    outgoing.push_back(msg.release());
    sink->send(session, name, outgoing.back());
}

void SenderImpl::replay()
{
    for (OutgoingMessages::iterator i = outgoing.begin(); i != outgoing.end(); ++i) {
        sink->send(session, name, *i);
    }
}

uint32_t SenderImpl::checkPendingSends(bool flush)
{
    if (flush) {
        session.flush(); 
        flushed = true;
    } else {
        flushed = false;
    }
    while (!outgoing.empty() && outgoing.front().status.isComplete()) {
        outgoing.pop_front();
    }
    return outgoing.size();
}

void SenderImpl::cancelImpl()
{
    state = CANCELLED;
    sink->cancel(session, name);
    parent.senderCancelled(name);
}

const std::string& SenderImpl::getName() const
{
    return name;
}

qpid::messaging::Session SenderImpl::getSession() const
{
    return qpid::messaging::Session(&parent);
}

}}} // namespace qpid::client::amqp0_10
