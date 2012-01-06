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

#include "qpid/client/AsyncSession.h"
#include "qpid/client/SubscriptionImpl.h"
#include "qpid/client/SessionImpl.h"
#include "qpid/client/SubscriptionManagerImpl.h"
#include "qpid/client/MessageImpl.h"
#include "qpid/client/CompletionImpl.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/SubscriptionSettings.h"
#include "qpid/client/SessionBase_0_10Access.h"
#include "qpid/client/PrivateImplRef.h"

namespace qpid {
namespace client {

using sys::Mutex;
using framing::MessageAcquireResult;

SubscriptionImpl::SubscriptionImpl(SubscriptionManager m, const std::string& q, const SubscriptionSettings& s, const std::string& n, MessageListener* l)
    : manager(*PrivateImplRef<SubscriptionManager>::get(m)), name(n), queue(q), settings(s), listener(l)
{}

void SubscriptionImpl::subscribe()
{
    async(manager.getSession()).messageSubscribe( 
        arg::queue=queue,
        arg::destination=name,
        arg::acceptMode=settings.acceptMode,
        arg::acquireMode=settings.acquireMode,
        arg::exclusive=settings.exclusive);
    setFlowControl(settings.flowControl);
}

std::string SubscriptionImpl::getName() const { return name; }

std::string SubscriptionImpl::getQueue() const { return queue; }

const SubscriptionSettings& SubscriptionImpl::getSettings() const {
    Mutex::ScopedLock l(lock);
    return settings;
}

void SubscriptionImpl::setFlowControl(const FlowControl& f) {
    Mutex::ScopedLock l(lock);
    AsyncSession s=manager.getSession();
    if (&settings.flowControl != &f) settings.flowControl = f;
    s.messageSetFlowMode(name, f.window); 
    s.messageFlow(name, CREDIT_UNIT_MESSAGE, f.messages); 
    s.messageFlow(name, CREDIT_UNIT_BYTE, f.bytes);
    s.sync();
}

void SubscriptionImpl::grantCredit(framing::message::CreditUnit unit, uint32_t value) {
    async(manager.getSession()).messageFlow(name, unit, value);
}

void SubscriptionImpl::setAutoAck(size_t n) {
    Mutex::ScopedLock l(lock);
    settings.autoAck = n;
}

SequenceSet SubscriptionImpl::getUnacquired() const { Mutex::ScopedLock l(lock); return unacquired; }
SequenceSet SubscriptionImpl::getUnaccepted() const { Mutex::ScopedLock l(lock); return unaccepted; }

void SubscriptionImpl::acquire(const SequenceSet& messageIds) {
    Mutex::ScopedLock l(lock);
    MessageAcquireResult result = manager.getSession().messageAcquire(messageIds);
    unacquired.remove(result.getTransfers());
    if (settings.acceptMode == ACCEPT_MODE_EXPLICIT)
        unaccepted.add(result.getTransfers());
}

void SubscriptionImpl::accept(const SequenceSet& messageIds) {
    Mutex::ScopedLock l(lock);
    manager.getSession().messageAccept(messageIds);
    unaccepted.remove(messageIds);
    switch (settings.completionMode) {
      case COMPLETE_ON_ACCEPT:
        manager.getSession().markCompleted(messageIds, true);
        break;
      case COMPLETE_ON_DELIVERY:
        manager.getSession().sendCompletion();
        break;
      default://do nothing
        break;
    }
}

void SubscriptionImpl::release(const SequenceSet& messageIds) {
    Mutex::ScopedLock l(lock);
    manager.getSession().messageRelease(messageIds);
    if (settings.acceptMode == ACCEPT_MODE_EXPLICIT)
        unaccepted.remove(messageIds);
}

Session SubscriptionImpl::getSession() const { return manager.getSession(); }

SubscriptionManager SubscriptionImpl::getSubscriptionManager() { return SubscriptionManager(&manager); }

void SubscriptionImpl::cancel() { manager.cancel(name); }

void SubscriptionImpl::received(Message& m) {
    Mutex::ScopedLock l(lock);
    MessageImpl& mi = *MessageImpl::get(m);
    if (mi.getMethod().getAcquireMode() == ACQUIRE_MODE_NOT_ACQUIRED) 
        unacquired.add(m.getId());
    else if (mi.getMethod().getAcceptMode() == ACCEPT_MODE_EXPLICIT)
        unaccepted.add(m.getId());

    if (listener) {
        Mutex::ScopedUnlock u(lock);
        listener->received(m);
    }

    if (settings.completionMode == COMPLETE_ON_DELIVERY) {
        manager.getSession().markCompleted(m.getId(), false, false);
    }
    if (settings.autoAck) {
        if (unaccepted.size() >= settings.autoAck) {
            async(manager.getSession()).messageAccept(unaccepted);
            switch (settings.completionMode) {
              case COMPLETE_ON_ACCEPT:
                manager.getSession().markCompleted(unaccepted, true);
                break;
              case COMPLETE_ON_DELIVERY:
                manager.getSession().sendCompletion();
                break;
              default://do nothing
                break;
            }
            unaccepted.clear();
        }
    }
}

Demux::QueuePtr SubscriptionImpl::divert()
{
    Session session(manager.getSession());
    Demux& demux = SessionBase_0_10Access(session).get()->getDemux();
    demuxRule = std::auto_ptr<ScopedDivert>(new ScopedDivert(name, demux));
    return demuxRule->getQueue();
}

void SubscriptionImpl::cancelDiversion() { 
    demuxRule.reset();
}

}} // namespace qpid::client

