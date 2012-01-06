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
#include "ReceiverImpl.h"
#include "AddressResolution.h"
#include "MessageSource.h"
#include "SessionImpl.h"
#include "qpid/messaging/Receiver.h"
#include "qpid/messaging/Session.h"

namespace qpid {
namespace client {
namespace amqp0_10 {

using qpid::messaging::Receiver;

void ReceiverImpl::received(qpid::messaging::Message&)
{
    //TODO: should this be configurable
    if (capacity && --window <= capacity/2) {
        session.sendCompletion();
        window = capacity;
    }
}
    
qpid::messaging::Message ReceiverImpl::get(qpid::sys::Duration timeout) 
{
    qpid::messaging::Message result;
    if (!get(result, timeout)) throw Receiver::NoMessageAvailable();
    return result;
}
    
qpid::messaging::Message ReceiverImpl::fetch(qpid::sys::Duration timeout) 
{
    qpid::messaging::Message result;
    if (!fetch(result, timeout)) throw Receiver::NoMessageAvailable();
    return result;
}

bool ReceiverImpl::get(qpid::messaging::Message& message, qpid::sys::Duration timeout)
{
    Get f(*this, message, timeout);
    while (!parent.execute(f)) {}
    return f.result;
}

bool ReceiverImpl::fetch(qpid::messaging::Message& message, qpid::sys::Duration timeout)
{
    Fetch f(*this, message, timeout);
    while (!parent.execute(f)) {}
    return f.result;
}

void ReceiverImpl::cancel() 
{ 
    execute<Cancel>();
}

void ReceiverImpl::start()
{
    if (state == STOPPED) {
        state = STARTED;
        startFlow();
    }
}

void ReceiverImpl::stop()
{
    state = STOPPED;
    session.messageStop(destination);
}

void ReceiverImpl::setCapacity(uint32_t c)
{
    execute1<SetCapacity>(c);
}

void ReceiverImpl::startFlow()
{
    if (capacity > 0) {
        session.messageSetFlowMode(destination, FLOW_MODE_WINDOW);
        session.messageFlow(destination, CREDIT_UNIT_MESSAGE, capacity);
        session.messageFlow(destination, CREDIT_UNIT_BYTE, byteCredit);
        window = capacity;
    }
}

void ReceiverImpl::init(qpid::client::AsyncSession s, AddressResolution& resolver)
{

    session = s;
    if (state == UNRESOLVED) {
        source = resolver.resolveSource(session, address);
        state = STARTED;
    }
    if (state == CANCELLED) {
        source->cancel(session, destination);
        parent.receiverCancelled(destination);        
    } else {
        source->subscribe(session, destination);
        start();
    }
}


const std::string& ReceiverImpl::getName() const { return destination; }

uint32_t ReceiverImpl::getCapacity()
{
    return capacity;
}

uint32_t ReceiverImpl::available()
{
    return parent.available(destination);
}

uint32_t ReceiverImpl::pendingAck()
{
    return parent.pendingAck(destination);
}

ReceiverImpl::ReceiverImpl(SessionImpl& p, const std::string& name, 
                           const qpid::messaging::Address& a) : 

    parent(p), destination(name), address(a), byteCredit(0xFFFFFFFF), 
    state(UNRESOLVED), capacity(0), window(0) {}

bool ReceiverImpl::getImpl(qpid::messaging::Message& message, qpid::sys::Duration timeout)
{
    return parent.get(*this, message, timeout);
}

bool ReceiverImpl::fetchImpl(qpid::messaging::Message& message, qpid::sys::Duration timeout)
{
    if (state == CANCELLED) return false;//TODO: or should this be an error?

    if (capacity == 0 || state != STARTED) {
        session.messageSetFlowMode(destination, FLOW_MODE_CREDIT);
        session.messageFlow(destination, CREDIT_UNIT_MESSAGE, 1);
        session.messageFlow(destination, CREDIT_UNIT_BYTE, 0xFFFFFFFF);
    }
    
    if (getImpl(message, timeout)) {
        return true;
    } else {
        sync(session).messageFlush(destination);
        startFlow();//reallocate credit
        return getImpl(message, 0);
    }
}

void ReceiverImpl::cancelImpl() 
{ 
    if (state != CANCELLED) {
        state = CANCELLED;
        source->cancel(session, destination);
        parent.receiverCancelled(destination);
    }
}

void ReceiverImpl::setCapacityImpl(uint32_t c)
{
    if (c != capacity) {
        capacity = c;
        if (state == STARTED) {
            session.messageStop(destination);
            startFlow();
        }
    }
}
qpid::messaging::Session ReceiverImpl::getSession() const
{
    return qpid::messaging::Session(&parent);
}

}}} // namespace qpid::client::amqp0_10
