#ifndef QPID_CLIENT_MESSAGE_H
#define QPID_CLIENT_MESSAGE_H

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
#include "qpid/framing/MessageProperties.h"
#include "qpid/framing/DeliveryProperties.h"
#include <string>

namespace qpid {

namespace framing {
class FieldTable;
class SequenceNumber;           // FIXME aconway 2009-04-17: remove with getID?
}

namespace client {

class MessageImpl;

/**
 * A message sent to or received from the broker.
 *
 * \ingroup clientapi
 * \details
 *
 * <h2>Getting and setting message contents</h2>
 *
 * <ul>
 * <li>
 * <p>getData()</p>
 * <pre>std::cout &lt;&lt; "Response: " &lt;&lt; message.getData() &lt;&lt; std::endl;</pre>
 * </li>
 * <li>
 * <p>setData()</p>
 * <pre>message.setData("That's all, folks!");</pre></li>
 * <li>
 * <p>appendData()</p>
 * <pre>message.appendData(" ... let's add a bit more ...");</pre></li>
 * </ul>
 *
 * <h2>Getting and Setting Delivery Properties</h2>
 *
 * <ul>
 * <li>
 * <p>getDeliveryProperties()</p>
 * <pre>message.getDeliveryProperties().setRoutingKey("control");</pre>
 * <pre>message.getDeliveryProperties().setDeliveryMode(PERSISTENT);</pre>
 * <pre>message.getDeliveryProperties().setPriority(9);</pre>
 * <pre>message.getDeliveryProperties().setTtl(100);</pre></li>
 *
 * <li>
 * <p>hasDeliveryProperties()</p>
 * <pre>if (! message.hasDeliveryProperties()) {
 *  ...
 *}</pre></li>
 * </ul>
 *
 * <h2>Getting and Setting Message Properties</h2>
 *
 * <ul>
 * <li>
 * <p>getMessageProperties()</p>
 * <pre>
 *request.getMessageProperties().setReplyTo(ReplyTo("amq.direct", response_queue.str()));
 * </pre>
 * <pre>
 *routingKey = request.getMessageProperties().getReplyTo().getRoutingKey();
 *exchange = request.getMessageProperties().getReplyTo().getExchange();
 * </pre>
 * <pre>message.getMessageProperties().setContentType("text/plain");</pre>
 * <pre>message.getMessageProperties().setContentEncoding("text/plain");</pre>
 * </li>
 * <li>
 * <p>hasMessageProperties()</p>
 * <pre>request.getMessageProperties().hasReplyTo();</pre>
 * </li>
 * </ul>
 *
 * <h2>Getting and Setting Application Headers</h2>
 *
 * <ul>
 * <li>
 * <p>getHeaders()</p>
 * <pre>
 *message.getHeaders().getString("control");
 * </pre>
 * <pre>
 *message.getHeaders().setString("control","continue");
 * </pre></li>
 * </ul>
 *
 *
 */
class Message
{
public:
    /** Create a Message.
     *@param data Data for the message body.
     *@param routingKey Passed to the exchange that routes the message.
     */
    QPID_CLIENT_EXTERN Message(
        const std::string& data=std::string(),
        const std::string& routingKey=std::string());
    Message(MessageImpl*);    ///< @internal
    QPID_CLIENT_EXTERN Message(const Message&);
    QPID_CLIENT_EXTERN ~Message();
    QPID_CLIENT_EXTERN Message& operator=(const Message&);
    QPID_CLIENT_EXTERN void swap(Message&);

    QPID_CLIENT_EXTERN void setData(const std::string&);
    QPID_CLIENT_EXTERN const std::string& getData() const;
    QPID_CLIENT_EXTERN std::string& getData();

    QPID_CLIENT_EXTERN void appendData(const std::string&);

    QPID_CLIENT_EXTERN bool hasMessageProperties() const;
    QPID_CLIENT_EXTERN framing::MessageProperties& getMessageProperties();
    QPID_CLIENT_EXTERN const framing::MessageProperties& getMessageProperties() const;

    QPID_CLIENT_EXTERN bool hasDeliveryProperties() const;
    QPID_CLIENT_EXTERN framing::DeliveryProperties& getDeliveryProperties();
    QPID_CLIENT_EXTERN const framing::DeliveryProperties& getDeliveryProperties() const;


    /** The destination of messages sent to the broker is the exchange
     * name.  The destination of messages received from the broker is
     * the delivery tag identifyig the local subscription (often this
     * is the name of the subscribed queue.)
     */
    QPID_CLIENT_EXTERN std::string getDestination() const;

    /** Check the redelivered flag. */
    QPID_CLIENT_EXTERN bool isRedelivered() const;
    /** Set the redelivered flag. */
    QPID_CLIENT_EXTERN void setRedelivered(bool redelivered);

    /** Get a modifyable reference to the message headers. */
    QPID_CLIENT_EXTERN framing::FieldTable& getHeaders();

    /** Get a non-modifyable reference to the message headers. */
    QPID_CLIENT_EXTERN const framing::FieldTable& getHeaders() const;

    // FIXME aconway 2009-04-17: does this need to be in public API?
    ///@internal
    QPID_CLIENT_EXTERN const framing::SequenceNumber& getId() const;

  private:
    MessageImpl* impl;
    friend class MessageImpl; // Helper template for implementation
};

}}

#endif  /*!QPID_CLIENT_MESSAGE_H*/
