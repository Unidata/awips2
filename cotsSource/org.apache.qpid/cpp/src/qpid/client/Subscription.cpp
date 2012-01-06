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

#include "qpid/client/Subscription.h"
#include "qpid/client/SubscriptionImpl.h"
#include "qpid/client/CompletionImpl.h"
#include "qpid/client/PrivateImplRef.h"
#include "qpid/framing/enum.h"

namespace qpid {
namespace client {

typedef PrivateImplRef<Subscription> PI;
Subscription::Subscription(SubscriptionImpl* p) { PI::ctor(*this, p); }
Subscription::~Subscription() { PI::dtor(*this); }
Subscription::Subscription(const Subscription& c) : Handle<SubscriptionImpl>() { PI::copy(*this, c); }
Subscription& Subscription::operator=(const Subscription& c) { return PI::assign(*this, c); }


std::string Subscription::getName() const { return impl->getName(); }
std::string Subscription::getQueue() const { return impl->getQueue(); }
const SubscriptionSettings& Subscription::getSettings() const { return impl->getSettings(); }
void Subscription::setFlowControl(const FlowControl& f) { impl->setFlowControl(f); }
void Subscription::setAutoAck(unsigned int n) { impl->setAutoAck(n); }
SequenceSet Subscription::getUnacquired() const { return impl->getUnacquired(); }
SequenceSet Subscription::getUnaccepted() const { return impl->getUnaccepted(); }
void Subscription::acquire(const SequenceSet& messageIds) { impl->acquire(messageIds); }
void Subscription::accept(const SequenceSet& messageIds) { impl->accept(messageIds); }
void Subscription::release(const SequenceSet& messageIds) { impl->release(messageIds); }
Session Subscription::getSession() const { return impl->getSession(); }
SubscriptionManager Subscription::getSubscriptionManager() { return impl->getSubscriptionManager(); }
void Subscription::cancel() { impl->cancel(); }
void Subscription::grantMessageCredit(uint32_t value) { impl->grantCredit(framing::message::CREDIT_UNIT_MESSAGE, value); }
void Subscription::grantByteCredit(uint32_t value) { impl->grantCredit(framing::message::CREDIT_UNIT_BYTE, value); }
}} // namespace qpid::client


