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
#include <string>
#include "qpid/client/ClientImportExport.h"

#ifndef _MessageListener_
#define _MessageListener_

#include "qpid/client/Message.h"

namespace qpid {
namespace client {

    /**
     * Implement a subclass of MessageListener and subscribe it using
     * the SubscriptionManager to receive messages.
     *
     * Another way to receive messages is by using a LocalQueue.
     *
     * \ingroup clientapi
     * \details
     *
     * <h2>Using a MessageListener</h2>
     *
     * <ul>
     * <li>
     * <p>The received() function is called when a message arrives:</p>
     * <pre>virtual void received(Message&amp; message)=0;</pre>
     * </li>
     * <li>
     * <p>Derive your own listener, implement the received() function:</p>
     * <pre>
     * class Listener : public MessageListener {
     *  private:
     *    SubscriptionManager&amp; subscriptions;
     *  public:
     *    Listener(SubscriptionManager&amp; subscriptions);
     *    virtual void received(Message&amp; message);
     * };
     *
     * Listener::Listener(SubscriptionManager&amp; subs) : subscriptions(subs)
     * {}
     *
     * void Listener::received(Message&amp; message) {
     *   std::cout &lt;&lt; "Message: " &lt;&lt; message.getData() &lt;&lt; std::endl;
     *   if (message.getData() == "That's all, folks!") {
     *       std::cout &lt;&lt; "Shutting down listener for " &lt;&lt; message.getDestination()
     *                &lt;&lt; std::endl;
     *       subscriptions.cancel(message.getDestination());
     *   }
     * }
     *</pre>
     * <pre>
     * SubscriptionManager subscriptions(session);
     *
     * // Create a listener and subscribe it to the queue named "message_queue"
     * Listener listener(subscriptions);
     * subscriptions.subscribe(listener, "message_queue");
     *
     * // Receive messages until the subscription is cancelled
     * // by Listener::received()
     * subscriptions.run();
     * </pre>
     * </li>
     * </ul>
     *
     */

    class MessageListener{
    public:
        QPID_CLIENT_EXTERN virtual ~MessageListener();

        /** Called for each message arriving from the broker. Override
         * in your own subclass to process messages.
         */
	virtual void received(Message& msg) = 0;
    };

}
}


#endif
