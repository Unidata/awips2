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

#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/SubscriptionManagerImpl.h"
#include "qpid/client/PrivateImplRef.h"


namespace qpid {
namespace client {

typedef PrivateImplRef<SubscriptionManager> PI;

SubscriptionManager::SubscriptionManager(const Session& s) { PI::ctor(*this, new SubscriptionManagerImpl(s)); }
SubscriptionManager::SubscriptionManager(SubscriptionManagerImpl* i) { PI::ctor(*this, i); }
SubscriptionManager::SubscriptionManager(const SubscriptionManager& x) : Runnable(), Handle<SubscriptionManagerImpl>() { PI::copy(*this, x); }
SubscriptionManager::~SubscriptionManager() { PI::dtor(*this); }
SubscriptionManager& SubscriptionManager::operator=(const SubscriptionManager& x) { return PI::assign(*this, x); }

Subscription SubscriptionManager::subscribe(
    MessageListener& listener, const std::string& q, const SubscriptionSettings& ss, const std::string& n)
{ return impl->subscribe(listener, q, ss, n); }

Subscription SubscriptionManager::subscribe(
    LocalQueue& lq, const std::string& q, const SubscriptionSettings& ss, const std::string& n)
{ return impl->subscribe(lq, q, ss, n); }


Subscription SubscriptionManager::subscribe(
    MessageListener& listener, const std::string& q, const std::string& n)
{ return impl->subscribe(listener, q, n); }


Subscription SubscriptionManager::subscribe(
    LocalQueue& lq, const std::string& q, const std::string& n)
{ return impl->subscribe(lq, q, n); }

void SubscriptionManager::cancel(const std::string& dest) { return impl->cancel(dest); }

void SubscriptionManager::setAutoStop(bool set) { impl->setAutoStop(set); }

void SubscriptionManager::run() { impl->run(); }

void SubscriptionManager::start() { impl->start(); }

void SubscriptionManager::wait() { impl->wait(); }

void SubscriptionManager::stop() { impl->stop(); }

bool SubscriptionManager::get(Message& result, const std::string& queue, sys::Duration timeout) {
    return impl->get(result, queue, timeout);
}

Message SubscriptionManager::get(const std::string& queue, sys::Duration timeout) {
    return impl->get(queue, timeout);
}

Session SubscriptionManager::getSession() const { return impl->getSession(); }

Subscription SubscriptionManager::getSubscription(const std::string& name) const {
    return impl->getSubscription(name);
}
void SubscriptionManager::registerFailoverHandler (boost::function<void ()> fh) {
    impl->registerFailoverHandler(fh); 
}

void SubscriptionManager::setFlowControl(const std::string& name, const FlowControl& flow) {
    impl->setFlowControl(name, flow);
}

void SubscriptionManager::setFlowControl(const std::string& name, uint32_t messages,  uint32_t bytes, bool window) {
    impl->setFlowControl(name, FlowControl(messages, bytes, window));
}

void SubscriptionManager::setFlowControl(uint32_t messages,  uint32_t bytes, bool window) {
    impl->setFlowControl(messages, bytes, window);
}

void SubscriptionManager::setAcceptMode(AcceptMode mode) { impl->setAcceptMode(mode); }
void SubscriptionManager::setAcquireMode(AcquireMode mode) { impl->setAcquireMode(mode); }

}} // namespace qpid::client


