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
#include "qpid/client/SubscriptionImpl.h"
#include "qpid/client/LocalQueueImpl.h"
#include "qpid/client/PrivateImplRef.h"
#include <qpid/client/Dispatcher.h>
#include <qpid/client/Session.h>
#include <qpid/client/MessageListener.h>
#include <qpid/framing/Uuid.h>
#include <set>
#include <sstream>


namespace qpid {
namespace client {

SubscriptionManagerImpl::SubscriptionManagerImpl(const Session& s)
  : dispatcher(s), session(s), autoStop(true)
{}

Subscription SubscriptionManagerImpl::subscribe(
    MessageListener& listener, const std::string& q, const SubscriptionSettings& ss, const std::string& n)
{
    sys::Mutex::ScopedLock l(lock);
    std::string name=n.empty() ? q:n;
    boost::intrusive_ptr<SubscriptionImpl> si = new SubscriptionImpl(SubscriptionManager(this), q, ss, name, &listener);
    dispatcher.listen(si);
    //issue subscription request after listener is registered with dispatcher
    si->subscribe();
    return subscriptions[name] = Subscription(si.get());
}

Subscription SubscriptionManagerImpl::subscribe(
    LocalQueue& lq, const std::string& q, const SubscriptionSettings& ss, const std::string& n)
{
    sys::Mutex::ScopedLock l(lock);
    std::string name=n.empty() ? q:n;
    boost::intrusive_ptr<SubscriptionImpl> si = new SubscriptionImpl(SubscriptionManager(this), q, ss, name, 0);
    boost::intrusive_ptr<LocalQueueImpl> lqi = PrivateImplRef<LocalQueue>::get(lq);
    lqi->queue=si->divert();
    si->subscribe();
    lqi->subscription = Subscription(si.get());
    return subscriptions[name] = lqi->subscription;
}

Subscription SubscriptionManagerImpl::subscribe(
    MessageListener& listener, const std::string& q, const std::string& n)
{
    return subscribe(listener, q, defaultSettings, n);
}

Subscription SubscriptionManagerImpl::subscribe(
    LocalQueue& lq, const std::string& q, const std::string& n)
{
    return subscribe(lq, q, defaultSettings, n);
}

void SubscriptionManagerImpl::cancel(const std::string& dest)
{
    sys::Mutex::ScopedLock l(lock);
    std::map<std::string, Subscription>::iterator i = subscriptions.find(dest);
    if (i != subscriptions.end()) {
        sync(session).messageCancel(dest);
        dispatcher.cancel(dest);
        Subscription s = i->second;
        if (s.isValid())
            PrivateImplRef<Subscription>::get(s)->cancelDiversion();
        subscriptions.erase(i);
    }
}

void SubscriptionManagerImpl::setAutoStop(bool set) { autoStop=set; }

void SubscriptionManagerImpl::run()
{
    dispatcher.setAutoStop(autoStop);
    dispatcher.run();
}

void SubscriptionManagerImpl::start()
{
    dispatcher.setAutoStop(autoStop);
    dispatcher.start();
}

void SubscriptionManagerImpl::wait()
{
    dispatcher.wait();
}

void SubscriptionManagerImpl::stop()
{
    dispatcher.stop();
}

bool SubscriptionManagerImpl::get(Message& result, const std::string& queue, sys::Duration timeout) {
    LocalQueue lq;
    std::string unique = framing::Uuid(true).str();
    subscribe(lq, queue, SubscriptionSettings(FlowControl::messageCredit(1)), unique);
    SubscriptionManager sm(this);
    AutoCancel ac(sm, unique);
    //first wait for message to be delivered if a timeout has been specified
    if (timeout && lq.get(result, timeout))
        return true;
    //make sure message is not on queue before final check
    sync(session).messageFlush(unique);
    return lq.get(result, 0);
}

Message SubscriptionManagerImpl::get(const std::string& queue, sys::Duration timeout) {
    Message result;
    if (!get(result, queue, timeout))
        throw Exception("Timed out waiting for a message");
    return result;
}

Session SubscriptionManagerImpl::getSession() const { return session; }

Subscription SubscriptionManagerImpl::getSubscription(const std::string& name) const {
    sys::Mutex::ScopedLock l(lock);
    std::map<std::string, Subscription>::const_iterator i = subscriptions.find(name);
    if (i == subscriptions.end())
        throw Exception(QPID_MSG("Subscription not found: " << name));
    return i->second;
}

void SubscriptionManagerImpl::registerFailoverHandler (boost::function<void ()> fh) {
    dispatcher.registerFailoverHandler(fh);
}

void SubscriptionManagerImpl::setFlowControl(const std::string& name, const FlowControl& flow) {
    getSubscription(name).setFlowControl(flow);
}

void SubscriptionManagerImpl::setFlowControl(const std::string& name, uint32_t messages,  uint32_t bytes, bool window) {
    setFlowControl(name, FlowControl(messages, bytes, window));
}

}} // namespace qpid::client


