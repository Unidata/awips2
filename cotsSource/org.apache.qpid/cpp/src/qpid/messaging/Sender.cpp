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
#include "qpid/messaging/Sender.h"
#include "qpid/messaging/Message.h"
#include "qpid/messaging/SenderImpl.h"
#include "qpid/messaging/Session.h"
#include "qpid/client/PrivateImplRef.h"

namespace qpid {
namespace client {

typedef PrivateImplRef<qpid::messaging::Sender> PI;

}

namespace messaging {

using qpid::client::PI;

Sender::Sender(SenderImpl* impl) { PI::ctor(*this, impl); }
Sender::Sender(const Sender& s) : qpid::client::Handle<SenderImpl>() { PI::copy(*this, s); }
Sender::~Sender() { PI::dtor(*this); }
Sender& Sender::operator=(const Sender& s) { return PI::assign(*this, s); }
void Sender::send(const Message& message) { impl->send(message); }
void Sender::cancel() { impl->cancel(); }
void Sender::setCapacity(uint32_t c) { impl->setCapacity(c); }
uint32_t Sender::getCapacity() { return impl->getCapacity(); }
uint32_t Sender::pending() { return impl->pending(); }
const std::string& Sender::getName() const { return impl->getName(); }
Session Sender::getSession() const { return impl->getSession(); }

}} // namespace qpid::messaging
