#ifndef QPID_CLIENT_SUBSCRIPTIONMANAGERIMPL_H
#define QPID_CLIENT_SUBSCRIPTIONMANAGERIMPL_H

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
#include "qpid/sys/Mutex.h"
#include <qpid/client/Dispatcher.h>
#include <qpid/client/Completion.h>
#include <qpid/client/Session.h>
#include <qpid/client/AsyncSession.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/LocalQueue.h>
#include <qpid/client/Subscription.h>
#include <qpid/sys/Runnable.h>
#include <qpid/RefCounted.h>
#include <set>
#include <sstream>

namespace qpid {
namespace client {

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
class SubscriptionManagerImpl : public sys::Runnable, public RefCounted
{
  public:
    /** Create a new SubscriptionManagerImpl associated with a session */
    SubscriptionManagerImpl(const Session& session);
    
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
    Subscription subscribe(MessageListener& listener,
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
    Subscription subscribe(LocalQueue& localQueue,
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
    Subscription subscribe(MessageListener& listener,
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
    Subscription subscribe(LocalQueue& localQueue,
                           const std::string& queue,
                           const std::string& name=std::string());


    /** Get a single message from a queue.
     *@param result is set to the message from the queue.
     *@param timeout wait up this timeout for a message to appear. 
     *@return true if result was set, false if no message available after timeout.
     */
    bool get(Message& result, const std::string& queue, sys::Duration timeout=0);

    /** Get a single message from a queue.
     *@param timeout wait up this timeout for a message to appear. 
     *@return message from the queue.
     *@throw Exception if the timeout is exceeded.
     */
    Message get(const std::string& queue, sys::Duration timeout=sys::TIME_INFINITE);

    /** Get a subscription by name.
     *@throw Exception if not found.
     */
    Subscription getSubscription(const std::string& name) const;
    
    /** Cancel a subscription. See also: Subscription.cancel() */
    void cancel(const std::string& name);

    /** Deliver messages in the current thread until stop() is called.
     * Only one thread may be running in a SubscriptionManager at a time.
     * @see run
     */
    void run();

    /** Start a new thread to deliver messages.
     * Only one thread may be running in a SubscriptionManager at a time.
     * @see start
     */
    void start();

    /**
     * Wait for the thread started by a call to start() to complete.
     */
    void wait();
    
    /** If set true, run() will stop when all subscriptions
     * are cancelled. If false, run will only stop when stop()
     * is called. True by default.
     */
    void setAutoStop(bool set=true);

    /** Stop delivery. Causes run() to return, or the thread started with start() to exit. */
    void stop();

    static const uint32_t UNLIMITED=0xFFFFFFFF;

    /** Set the flow control for a subscription. */
    void setFlowControl(const std::string& name, const FlowControl& flow);

    /** Set the flow control for a subscription.
     *@param name: name of the subscription.
     *@param messages: message credit.
     *@param bytes: byte credit.
     *@param window: if true use window-based flow control.
     */
    void setFlowControl(const std::string& name, uint32_t messages,  uint32_t bytes, bool window=true);

    /** Set the default settings for subscribe() calls that don't
     * include a SubscriptionSettings parameter.
     */
    void setDefaultSettings(const SubscriptionSettings& s) { defaultSettings = s; }

    /** Get the default settings for subscribe() calls that don't
     * include a SubscriptionSettings parameter.
     */
    const SubscriptionSettings& getDefaultSettings() const { return defaultSettings; }

    /** Get the default settings for subscribe() calls that don't
     * include a SubscriptionSettings parameter.
     */
    SubscriptionSettings& getDefaultSettings() { return defaultSettings; }

    /**
     * Set the default flow control settings for subscribe() calls
     * that don't include a SubscriptionSettings parameter.
     *
     *@param messages: message credit.
     *@param bytes: byte credit.
     *@param window: if true use window-based flow control.
     */
    void setFlowControl(uint32_t messages,  uint32_t bytes, bool window=true) {
        defaultSettings.flowControl = FlowControl(messages, bytes, window);
    }

    /**
     *Set the default accept-mode for subscribe() calls that don't
     *include a SubscriptionSettings parameter.
     */
    void setAcceptMode(AcceptMode mode) { defaultSettings.acceptMode = mode; }

    /**
     * Set the default acquire-mode subscribe()s that don't specify SubscriptionSettings.
     */
    void setAcquireMode(AcquireMode mode) { defaultSettings.acquireMode = mode; }

    void registerFailoverHandler ( boost::function<void ()> fh );

    Session getSession() const;

  private:
    mutable sys::Mutex lock;
    qpid::client::Dispatcher dispatcher;
    qpid::client::AsyncSession session;
    bool autoStop;
    SubscriptionSettings defaultSettings;
    std::map<std::string, Subscription> subscriptions;
};


}} // namespace qpid::client

#endif  /*!QPID_CLIENT_SUBSCRIPTIONMANAGERIMPL_H*/
