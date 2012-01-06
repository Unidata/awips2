#ifndef QPID_CLIENT_SUBSCRIPTIONMANAGER_H
#define QPID_CLIENT_SUBSCRIPTIONMANAGER_H

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

#include "qpid/client/Session.h"
#include "qpid/client/Subscription.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/Thread.h"
#include "qpid/client/ClientImportExport.h"
#include "qpid/client/MessageListener.h"
#include "qpid/client/LocalQueue.h"
#include "qpid/client/Handle.h"
#include <string>

namespace qpid {
namespace client {

class SubscriptionManagerImpl;

/**
 * A class to help create and manage subscriptions.
 *
 * Set up your subscriptions, then call run() to have messages
 * delivered.
 *
 * \ingroup clientapi
 *
 * \details
 *
 * <h2>Subscribing and canceling subscriptions</h2>
 *
 * <ul>
 * <li>
 * <p>subscribe()</p>
 * <pre> SubscriptionManager subscriptions(session);
 * Listener listener(subscriptions);
 * subscriptions.subscribe(listener, myQueue);</pre>
 * <pre> SubscriptionManager subscriptions(session);
 * LocalQueue local_queue;
 * subscriptions.subscribe(local_queue, string("message_queue"));</pre></li>
 * <li>
 * <p>cancel()</p>
 * <pre>subscriptions.cancel();</pre></li>
 * </ul>
 *
 * <h2>Waiting for messages (and returning)</h2>
 *
 * <ul>
 * <li>
 * <p>run()</p>
 * <pre> // Give up control to receive messages
 * subscriptions.run();</pre></li>
 * <li>
 * <p>stop()</p>
 * <pre>.// Use this code in a listener to return from run()
 * subscriptions.stop();</pre></li>
 * <li>
 * <p>setAutoStop()</p>
 * <pre>.// Return from subscriptions.run() when last subscription is cancelled
 *.subscriptions.setAutoStop(true);
 *.subscriptons.run();
 * </pre></li>
 * <li>
 * <p>Ending a subscription in a listener</p>
 * <pre>
 * void Listener::received(Message&amp; message) {
 *
 *  if (message.getData() == "That's all, folks!") {
 *       subscriptions.cancel(message.getDestination());
 *   }
 * }
 * </pre>
 * </li>
 * </ul>
 *
 */
class SubscriptionManager : public sys::Runnable, public Handle<SubscriptionManagerImpl>
{
  public:
    /** Create a new SubscriptionManager associated with a session */
    QPID_CLIENT_EXTERN SubscriptionManager(const Session& session);
    QPID_CLIENT_EXTERN SubscriptionManager(const SubscriptionManager&);
    QPID_CLIENT_EXTERN ~SubscriptionManager();
    QPID_CLIENT_EXTERN SubscriptionManager& operator=(const SubscriptionManager&);

    /**
     * Subscribe a MessagesListener to receive messages from queue.
     *
     * Provide your own subclass of MessagesListener to process
     * incoming messages. It will be called for each message received.
     *
     *@param listener Listener object to receive messages.
     *@param queue Name of the queue to subscribe to.
     *@param settings settings for the subscription.
     *@param name unique destination name for the subscription, defaults to queue name.
     */
    QPID_CLIENT_EXTERN Subscription subscribe(MessageListener& listener,
                           const std::string& queue,
                           const SubscriptionSettings& settings,
                           const std::string& name=std::string());

    /**
     * Subscribe a LocalQueue to receive messages from queue.
     *
     * Incoming messages are stored in the queue for you to retrieve.
     *
     *@param queue Name of the queue to subscribe to.
     *@param flow initial FlowControl for the subscription.
     *@param name unique destination name for the subscription, defaults to queue name.
     * If not specified, the queue name is used.
     */
    QPID_CLIENT_EXTERN Subscription subscribe(LocalQueue& localQueue,
                           const std::string& queue,
                           const SubscriptionSettings& settings,
                           const std::string& name=std::string());

    /**
     * Subscribe a MessagesListener to receive messages from queue.
     *
     * Provide your own subclass of MessagesListener to process
     * incoming messages. It will be called for each message received.
     *
     *@param listener Listener object to receive messages.
     *@param queue Name of the queue to subscribe to.
     *@param name unique destination name for the subscription, defaults to queue name.
     * If not specified, the queue name is used.
     */
    QPID_CLIENT_EXTERN Subscription subscribe(MessageListener& listener,
                           const std::string& queue,
                           const std::string& name=std::string());

    /**
     * Subscribe a LocalQueue to receive messages from queue.
     *
     * Incoming messages are stored in the queue for you to retrieve.
     *
     *@param queue Name of the queue to subscribe to.
     *@param name unique destination name for the subscription, defaults to queue name.
     * If not specified, the queue name is used.
     */
    QPID_CLIENT_EXTERN Subscription subscribe(LocalQueue& localQueue,
                           const std::string& queue,
                           const std::string& name=std::string());


    /** Get a single message from a queue.
     * (Note: this currently uses a subscription per invocation and is
     * thus relatively expensive. The subscription is cancelled as
     * part of each call which can trigger auto-deletion).
     *@param result is set to the message from the queue.
     *@param timeout wait up this timeout for a message to appear.
     *@return true if result was set, false if no message available after timeout.
     */
    QPID_CLIENT_EXTERN bool get(Message& result, const std::string& queue, sys::Duration timeout=0);

    /** Get a single message from a queue.
     * (Note: this currently uses a subscription per invocation and is
     * thus relatively expensive. The subscription is cancelled as
     * part of each call which can trigger auto-deletion).
     *@param timeout wait up this timeout for a message to appear.
     *@return message from the queue.
     *@throw Exception if the timeout is exceeded.
     */
    QPID_CLIENT_EXTERN Message get(const std::string& queue, sys::Duration timeout=sys::TIME_INFINITE);

    /** Get a subscription by name.
     *@throw Exception if not found.
     */
    QPID_CLIENT_EXTERN Subscription getSubscription(const std::string& name) const;

    /** Cancel a subscription. See also: Subscription.cancel() */
    QPID_CLIENT_EXTERN void cancel(const std::string& name);

    /** Deliver messages in the current thread until stop() is called.
     * Only one thread may be running in a SubscriptionManager at a time.
     * @see run
     */
    QPID_CLIENT_EXTERN void run();

    /** Start a new thread to deliver messages.
     * Only one thread may be running in a SubscriptionManager at a time.
     * @see start
     */
    QPID_CLIENT_EXTERN void start();

    /**
     * Wait for the thread started by a call to start() to complete.
     */
    QPID_CLIENT_EXTERN void wait();

    /** If set true, run() will stop when all subscriptions
     * are cancelled. If false, run will only stop when stop()
     * is called. True by default.
     */
    QPID_CLIENT_EXTERN void setAutoStop(bool set=true);

    /** Stop delivery. Causes run() to return, or the thread started with start() to exit. */
    QPID_CLIENT_EXTERN void stop();

    static const uint32_t UNLIMITED=0xFFFFFFFF;

    /** Set the flow control for a subscription. */
    QPID_CLIENT_EXTERN void setFlowControl(const std::string& name, const FlowControl& flow);

    /** Set the flow control for a subscription.
     *@param name: name of the subscription.
     *@param messages: message credit.
     *@param bytes: byte credit.
     *@param window: if true use window-based flow control.
     */
    QPID_CLIENT_EXTERN void setFlowControl(const std::string& name, uint32_t messages,  uint32_t bytes, bool window=true);

    /** Set the default settings for subscribe() calls that don't
     * include a SubscriptionSettings parameter.
     */
    QPID_CLIENT_EXTERN void setDefaultSettings(const SubscriptionSettings& s);

    /** Get the default settings for subscribe() calls that don't
     * include a SubscriptionSettings parameter.
     */
    QPID_CLIENT_EXTERN const SubscriptionSettings& getDefaultSettings() const;

    /** Get the default settings for subscribe() calls that don't
     * include a SubscriptionSettings parameter.
     */
    QPID_CLIENT_EXTERN SubscriptionSettings& getDefaultSettings();

    /**
     * Set the default flow control settings for subscribe() calls
     * that don't include a SubscriptionSettings parameter.
     *
     *@param messages: message credit.
     *@param bytes: byte credit.
     *@param window: if true use window-based flow control.
     */
    QPID_CLIENT_EXTERN void setFlowControl(uint32_t messages,  uint32_t bytes, bool window=true);

    /**
     *Set the default accept-mode for subscribe() calls that don't
     *include a SubscriptionSettings parameter.
     */
    QPID_CLIENT_EXTERN void setAcceptMode(AcceptMode mode);

    /**
     * Set the default acquire-mode subscribe()s that don't specify SubscriptionSettings.
     */
    QPID_CLIENT_EXTERN void setAcquireMode(AcquireMode mode);

    QPID_CLIENT_EXTERN void registerFailoverHandler ( boost::function<void ()> fh );

    QPID_CLIENT_EXTERN Session getSession() const;

    SubscriptionManager(SubscriptionManagerImpl*); ///<@internal

  private:
    typedef SubscriptionManagerImpl Impl;
    friend class PrivateImplRef<SubscriptionManager>;
};

/** AutoCancel cancels a subscription in its destructor */
class AutoCancel {
  public:
    AutoCancel(SubscriptionManager& sm_, const std::string& tag_) : sm(sm_), tag(tag_) {}
    ~AutoCancel() { sm.cancel(tag); }
  private:
    SubscriptionManager& sm;
    std::string tag;
};

}} // namespace qpid::client

#endif  /*!QPID_CLIENT_SUBSCRIPTIONMANAGER_H*/
