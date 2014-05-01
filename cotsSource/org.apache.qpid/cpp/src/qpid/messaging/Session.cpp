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
#include "qpid/messaging/Session.h"
#include "qpid/messaging/Address.h"
#include "qpid/messaging/Connection.h"
#include "qpid/messaging/Message.h"
#include "qpid/messaging/Sender.h"
#include "qpid/messaging/Receiver.h"
#include "qpid/messaging/SessionImpl.h"
#include "qpid/client/PrivateImplRef.h"

namespace qpid {
namespace client {

typedef PrivateImplRef<qpid::messaging::Session> PI;

}

namespace messaging {

using qpid::client::PI;

Session::Session(SessionImpl* impl) { PI::ctor(*this, impl); }
Session::Session(const Session& s) : qpid::client::Handle<SessionImpl>() { PI::copy(*this, s); }
Session::~Session() { PI::dtor(*this); }
Session& Session::operator=(const Session& s) { return PI::assign(*this, s); }
void Session::commit() { impl->commit(); }
void Session::rollback() { impl->rollback(); }
void Session::acknowledge() { impl->acknowledge(); }
void Session::reject(Message& m) { impl->reject(m); }
void Session::close() { impl->close(); }

Sender Session::createSender(const Address& address)
{
    return impl->createSender(address);
}
Receiver Session::createReceiver(const Address& address)
{
    return impl->createReceiver(address);
}

Sender Session::createSender(const std::string& address)
{ 
    return impl->createSender(Address(address)); 
}
Receiver Session::createReceiver(const std::string& address)
{ 
    return impl->createReceiver(Address(address)); 
}

void Session::sync()
{
    impl->sync();
}

void Session::flush()
{
    impl->flush();
}

bool Session::nextReceiver(Receiver& receiver, qpid::sys::Duration timeout)
{
    return impl->nextReceiver(receiver, timeout);
}


Receiver Session::nextReceiver(qpid::sys::Duration timeout)
{
    return impl->nextReceiver(timeout);
}

uint32_t Session::available() { return impl->available(); }
uint32_t Session::pendingAck() { return impl->pendingAck(); }

Sender Session::getSender(const std::string& name) const
{ 
    return impl->getSender(name); 
}
Receiver Session::getReceiver(const std::string& name) const
{ 
    return impl->getReceiver(name); 
}

Connection Session::getConnection() const
{ 
    return impl->getConnection(); 
}

KeyError::KeyError(const std::string& msg) : Exception(msg) {}

}} // namespace qpid::messaging
