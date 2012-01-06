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
#include "qpid/client/amqp0_10/SessionImpl.h"
#include "qpid/client/amqp0_10/ConnectionImpl.h"
#include "qpid/client/amqp0_10/ReceiverImpl.h"
#include "qpid/client/amqp0_10/SenderImpl.h"
#include "qpid/client/amqp0_10/MessageSource.h"
#include "qpid/client/amqp0_10/MessageSink.h"
#include "qpid/client/PrivateImplRef.h"
#include "qpid/Exception.h"
#include "qpid/log/Statement.h"
#include "qpid/messaging/Address.h"
#include "qpid/messaging/Connection.h"
#include "qpid/messaging/Message.h"
#include "qpid/messaging/MessageImpl.h"
#include "qpid/messaging/Sender.h"
#include "qpid/messaging/Receiver.h"
#include "qpid/messaging/Session.h"
#include "qpid/framing/reply_exceptions.h"
#include <boost/format.hpp>
#include <boost/function.hpp>
#include <boost/intrusive_ptr.hpp>

using qpid::messaging::KeyError;
using qpid::messaging::MessageImplAccess;
using qpid::messaging::Sender;
using qpid::messaging::Receiver;
using qpid::messaging::VariantMap;

namespace qpid {
namespace client {
namespace amqp0_10 {

SessionImpl::SessionImpl(ConnectionImpl& c, bool t) : connection(c), transactional(t) {}


void SessionImpl::sync()
{
    retry<Sync>();
}

void SessionImpl::flush()
{
    retry<Flush>();
}

void SessionImpl::commit()
{
    if (!execute<Commit>()) {
        throw Exception();//TODO: what type?
    }
}

void SessionImpl::rollback()
{
    //If the session fails during this operation, the transaction will
    //be rolled back anyway.
    execute<Rollback>();
}

void SessionImpl::acknowledge()
{
    //Should probably throw an exception on failure here, or indicate
    //it through a return type at least. Failure means that the
    //message may be redelivered; i.e. the application cannot delete
    //any state necessary for preventing reprocessing of the message
    execute<Acknowledge>();
}

void SessionImpl::reject(qpid::messaging::Message& m)
{
    //Possibly want to somehow indicate failure here as well. Less
    //clear need as compared to acknowledge however.
    execute1<Reject>(m);
}

void SessionImpl::close()
{
    //cancel all the senders and receivers (get copy of names and then
    //make the calls to avoid modifying maps while iterating over
    //them):
    std::vector<std::string> s;
    std::vector<std::string> r;
    {
        qpid::sys::Mutex::ScopedLock l(lock);        
        for (Senders::const_iterator i = senders.begin(); i != senders.end(); ++i) s.push_back(i->first);
        for (Receivers::const_iterator i = receivers.begin(); i != receivers.end(); ++i) r.push_back(i->first);
    }
    for (std::vector<std::string>::const_iterator i = s.begin(); i != s.end(); ++i) getSender(*i).cancel();
    for (std::vector<std::string>::const_iterator i = r.begin(); i != r.end(); ++i) getReceiver(*i).cancel();
    

    connection.closed(*this);
    session.close();
}

template <class T, class S> boost::intrusive_ptr<S> getImplPtr(T& t)
{
    return boost::dynamic_pointer_cast<S>(qpid::client::PrivateImplRef<T>::get(t));
}

template <class T> void getFreeKey(std::string& key, T& map)
{
    std::string name = key;
    int count = 1;
    for (typename T::const_iterator i = map.find(name); i != map.end(); i = map.find(name)) {
        name = (boost::format("%1%_%2%") % key % ++count).str();
    }
    key = name;
}


void SessionImpl::setSession(qpid::client::Session s)
{
    qpid::sys::Mutex::ScopedLock l(lock);
    session = s;
    incoming.setSession(session);
    if (transactional) session.txSelect();
    for (Receivers::iterator i = receivers.begin(); i != receivers.end(); ++i) {
        getImplPtr<Receiver, ReceiverImpl>(i->second)->init(session, resolver);
    }
    for (Senders::iterator i = senders.begin(); i != senders.end(); ++i) {
        getImplPtr<Sender, SenderImpl>(i->second)->init(session, resolver);
    }
}

struct SessionImpl::CreateReceiver : Command
{
    qpid::messaging::Receiver result;
    const qpid::messaging::Address& address;
    
    CreateReceiver(SessionImpl& i, const qpid::messaging::Address& a) :
        Command(i), address(a) {}
    void operator()() { result = impl.createReceiverImpl(address); }
};

Receiver SessionImpl::createReceiver(const qpid::messaging::Address& address)
{
    return get1<CreateReceiver, Receiver>(address);
}

Receiver SessionImpl::createReceiverImpl(const qpid::messaging::Address& address)
{
    std::string name = address.getName();
    getFreeKey(name, receivers);
    Receiver receiver(new ReceiverImpl(*this, name, address));
    getImplPtr<Receiver, ReceiverImpl>(receiver)->init(session, resolver);
    receivers[name] = receiver;
    return receiver;
}

struct SessionImpl::CreateSender : Command
{
    qpid::messaging::Sender result;
    const qpid::messaging::Address& address;
    
    CreateSender(SessionImpl& i, const qpid::messaging::Address& a) :
        Command(i), address(a) {}
    void operator()() { result = impl.createSenderImpl(address); }
};

Sender SessionImpl::createSender(const qpid::messaging::Address& address)
{
    return get1<CreateSender, Sender>(address);
}

Sender SessionImpl::createSenderImpl(const qpid::messaging::Address& address)
{ 
    std::string name = address.getName();
    getFreeKey(name, senders);
    Sender sender(new SenderImpl(*this, name, address));
    getImplPtr<Sender, SenderImpl>(sender)->init(session, resolver);
    senders[name] = sender;
    return sender;
}

Sender SessionImpl::getSender(const std::string& name) const
{
    qpid::sys::Mutex::ScopedLock l(lock);
    Senders::const_iterator i = senders.find(name);
    if (i == senders.end()) {
        throw KeyError(name);
    } else {
        return i->second;
    }    
}

Receiver SessionImpl::getReceiver(const std::string& name) const
{
    qpid::sys::Mutex::ScopedLock l(lock);
    Receivers::const_iterator i = receivers.find(name);
    if (i == receivers.end()) {
        throw KeyError(name);
    } else {
        return i->second;
    }
}

SessionImpl& SessionImpl::convert(qpid::messaging::Session& s)
{
    boost::intrusive_ptr<SessionImpl> impl = getImplPtr<qpid::messaging::Session, SessionImpl>(s);
    if (!impl) {
        throw qpid::Exception(QPID_MSG("Configuration error; require qpid::client::amqp0_10::SessionImpl"));
    }
    return *impl;
}

namespace {

struct IncomingMessageHandler : IncomingMessages::Handler
{
    typedef boost::function1<bool, IncomingMessages::MessageTransfer&> Callback;
    Callback callback;

    IncomingMessageHandler(Callback c) : callback(c) {}

    bool accept(IncomingMessages::MessageTransfer& transfer)
    {
        return callback(transfer);
    }
};

}


bool SessionImpl::getNextReceiver(Receiver* receiver, IncomingMessages::MessageTransfer& transfer)
{
    Receivers::const_iterator i = receivers.find(transfer.getDestination());
    if (i == receivers.end()) {
        QPID_LOG(error, "Received message for unknown destination " << transfer.getDestination());
        return false;
    } else {
        *receiver = i->second;
        return true;
    }
}

bool SessionImpl::accept(ReceiverImpl* receiver, 
                         qpid::messaging::Message* message, 
                         IncomingMessages::MessageTransfer& transfer)
{
    if (receiver->getName() == transfer.getDestination()) {
        transfer.retrieve(message);
        receiver->received(*message);
        return true;
    } else {
        return false;
    }
}

bool SessionImpl::getIncoming(IncomingMessages::Handler& handler, qpid::sys::Duration timeout)
{
    return incoming.get(handler, timeout);
}

bool SessionImpl::get(ReceiverImpl& receiver, qpid::messaging::Message& message, qpid::sys::Duration timeout)
{
    IncomingMessageHandler handler(boost::bind(&SessionImpl::accept, this, &receiver, &message, _1));
    return getIncoming(handler, timeout);
}

bool SessionImpl::nextReceiver(qpid::messaging::Receiver& receiver, qpid::sys::Duration timeout)
{
    qpid::sys::Mutex::ScopedLock l(lock);
    while (true) {
        try {
            std::string destination;
            if (incoming.getNextDestination(destination, timeout)) {
                Receivers::const_iterator i = receivers.find(destination);
                if (i == receivers.end()) {
                    throw qpid::Exception(QPID_MSG("Received message for unknown destination " << destination));
                } else {
                    receiver = i->second;
                }
                return true;
            } else {
                return false;
            }
        } catch (TransportFailure&) {
            reconnect();
        }
    }
}

qpid::messaging::Receiver SessionImpl::nextReceiver(qpid::sys::Duration timeout)
{
    qpid::messaging::Receiver receiver;
    if (!nextReceiver(receiver, timeout)) throw Receiver::NoMessageAvailable();
    if (!receiver) throw qpid::Exception("Bad receiver returned!");
    return receiver;
}

uint32_t SessionImpl::available()
{
    return get1<Available, uint32_t>((const std::string*) 0);
}
uint32_t SessionImpl::available(const std::string& destination)
{
    return get1<Available, uint32_t>(&destination);
}

struct SessionImpl::Available : Command
{
    const std::string* destination;
    uint32_t result;
    
    Available(SessionImpl& i, const std::string* d) : Command(i), destination(d), result(0) {}
    void operator()() { result = impl.availableImpl(destination); }
};

uint32_t SessionImpl::availableImpl(const std::string* destination)
{
    if (destination) {
        return incoming.available(*destination);
    } else {
        return incoming.available();
    }
}

uint32_t SessionImpl::pendingAck()
{
    return get1<PendingAck, uint32_t>((const std::string*) 0);
}

uint32_t SessionImpl::pendingAck(const std::string& destination)
{
    return get1<PendingAck, uint32_t>(&destination);
}

struct SessionImpl::PendingAck : Command
{
    const std::string* destination;
    uint32_t result;
    
    PendingAck(SessionImpl& i, const std::string* d) : Command(i), destination(d), result(0) {}
    void operator()() { result = impl.pendingAckImpl(destination); }
};

uint32_t SessionImpl::pendingAckImpl(const std::string* destination)
{
    if (destination) {
        return incoming.pendingAccept(*destination);
    } else {
        return incoming.pendingAccept();
    }
}

void SessionImpl::syncImpl()
{
    session.sync();
}

void SessionImpl::flushImpl()
{
    session.flush();
}


void SessionImpl::commitImpl()
{
    incoming.accept();
    session.txCommit();
}

void SessionImpl::rollbackImpl()
{
    for (Receivers::iterator i = receivers.begin(); i != receivers.end(); ++i) {
        getImplPtr<Receiver, ReceiverImpl>(i->second)->stop();
    }
    //ensure that stop has been processed and all previously sent
    //messages are available for release:                   
    session.sync();
    incoming.releaseAll();
    session.txRollback();    

    for (Receivers::iterator i = receivers.begin(); i != receivers.end(); ++i) {
        getImplPtr<Receiver, ReceiverImpl>(i->second)->start();
    }
}

void SessionImpl::acknowledgeImpl()
{
    incoming.accept();
}

void SessionImpl::rejectImpl(qpid::messaging::Message& m)
{
    SequenceSet set;
    set.add(MessageImplAccess::get(m).getInternalId());
    session.messageReject(set);
}

void SessionImpl::receiverCancelled(const std::string& name)
{
    receivers.erase(name);
    session.sync();
    incoming.releasePending(name);
}

void SessionImpl::senderCancelled(const std::string& name)
{
    senders.erase(name);
}

void SessionImpl::reconnect()
{
    connection.reconnect();    
}

qpid::messaging::Connection SessionImpl::getConnection() const
{
    return qpid::messaging::Connection(&connection);
}

}}} // namespace qpid::client::amqp0_10
