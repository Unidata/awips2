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

#include "qpid/broker/Message.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/framing/Uuid.h"

using namespace qpid;
using namespace broker;
using namespace framing;

namespace qpid {
namespace tests {

struct MessageUtils
{
    static boost::intrusive_ptr<Message> createMessage(const string& exchange="", const string& routingKey="",
                                                       const bool durable = false, const Uuid& messageId=Uuid(true),
                                                       uint64_t contentSize = 0)
    {
        boost::intrusive_ptr<broker::Message> msg(new broker::Message());

        AMQFrame method(( MessageTransferBody(ProtocolVersion(), exchange, 0, 0)));
        AMQFrame header((AMQHeaderBody()));

        msg->getFrames().append(method);
        msg->getFrames().append(header);
        MessageProperties* props = msg->getFrames().getHeaders()->get<MessageProperties>(true);
        props->setContentLength(contentSize);
        props->setMessageId(messageId);
        msg->getFrames().getHeaders()->get<DeliveryProperties>(true)->setRoutingKey(routingKey);
        if (durable)
            msg->getFrames().getHeaders()->get<DeliveryProperties>(true)->setDeliveryMode(2);
        return msg;
    }

    static void addContent(boost::intrusive_ptr<Message> msg, const string& data)
    {
        AMQFrame content((AMQContentBody(data)));
        msg->getFrames().append(content);
    }
};

}} // namespace qpid::tests
