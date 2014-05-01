#ifndef QPID_CLIENT_LOCALQUEUEIMPL_H
#define QPID_CLIENT_LOCALQUEUEIMPL_H

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

#include "qpid/client/ClientImportExport.h"
#include "qpid/client/Handle.h"
#include "qpid/client/Message.h"
#include "qpid/client/Subscription.h"
#include "qpid/client/Demux.h"
#include "qpid/sys/Time.h"
#include "qpid/RefCounted.h"

namespace qpid {
namespace client {

/**
 * A local queue to collect messages retrieved from a remote broker
 * queue. Create a queue and subscribe it using the SubscriptionManager.
 * Messages from the remote queue on the broker will be stored in the
 * local queue until you retrieve them.
 *
 * \ingroup clientapi
 *
 * \details Using a Local Queue
 *
 * <pre>
 * LocalQueue local_queue;
 * subscriptions.subscribe(local_queue, string("message_queue"));
 * for (int i=0; i&lt;10; i++) {
 *   Message message = local_queue.get();
 *   std::cout &lt;&lt; message.getData() &lt;&lt; std::endl;
 * }
 * </pre>
 * 
 * <h2>Getting Messages</h2>
 *
 * <ul><li>
 * <p>get()</p>
 * <pre>Message message = local_queue.get();</pre>
 * <pre>// Specifying timeouts (TIME_SEC, TIME_MSEC, TIME_USEC, TIME_NSEC)
 *#include <qpid/sys/Time.h>
 *Message message;
 *local_queue.get(message, 5*sys::TIME_SEC);</pre></li></ul>
 * 
 * <h2>Checking size</h2>
 * <ul><li>
 * <p>empty()</p>
 * <pre>if (local_queue.empty()) { ... }</pre></li>
 * <li><p>size()</p>
 * <pre>std::cout &lt;&lt; local_queue.size();</pre></li>
 * </ul>
 */

class LocalQueueImpl : public RefCounted {
  public:
    /** Wait up to timeout for the next message from the local queue.
     *@param result Set to the message from the queue.
     *@param timeout wait up this timeout for a message to appear. 
     *@return true if result was set, false if queue was empty after timeout.
     */
     bool get(Message& result, sys::Duration timeout=0);

    /** Get the next message off the local queue, or wait up to the timeout
     * for message from the broker queue.
     *@param timeout wait up this timeout for a message to appear.
     *@return message from the queue.
     *@throw ClosedException if subscription is closed or timeout exceeded.
     */
     Message get(sys::Duration timeout=sys::TIME_INFINITE);

    /** Synonym for get() */
     Message pop(sys::Duration timeout=sys::TIME_INFINITE);

    /** Return true if local queue is empty. */
     bool empty() const;

    /** Number of messages on the local queue */
     size_t size() const;

  private:
    Demux::QueuePtr queue;
    Subscription subscription;
  friend class SubscriptionManagerImpl;
};

}} // namespace qpid::client

#endif  /*!QPID_CLIENT_LOCALQUEUEIMPL_H*/
