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
#include "qpid/client/amqp0_10/OutgoingMessage.h"
#include "qpid/client/amqp0_10/AddressResolution.h"
#include "qpid/client/amqp0_10/Codecs.h"
#include "qpid/client/amqp0_10/CodecsInternal.h"
#include "qpid/messaging/Address.h"
#include "qpid/messaging/Message.h"
#include "qpid/messaging/MessageImpl.h"

namespace qpid {
namespace client {
namespace amqp0_10 {

using qpid::messaging::Address;
using qpid::messaging::MessageImplAccess;

void OutgoingMessage::convert(const qpid::messaging::Message& from)
{
    //TODO: need to avoid copying as much as possible
    message.setData(from.getContent());
    message.getMessageProperties().setContentType(from.getContentType());
    const Address& address = from.getReplyTo();
    if (address) {
        message.getMessageProperties().setReplyTo(AddressResolution::convert(address));
    }
    translate(from.getHeaders(), message.getMessageProperties().getApplicationHeaders());
    //TODO: set other message properties
    message.getDeliveryProperties().setRoutingKey(from.getSubject());
    //TODO: set other delivery properties
}

namespace {
const std::string SUBJECT("subject");
}

void OutgoingMessage::setSubject(const std::string& subject)
{
    if (!subject.empty()) {
        message.getMessageProperties().getApplicationHeaders().setString(SUBJECT, subject);
    }
}

std::string OutgoingMessage::getSubject() const
{
    return message.getMessageProperties().getApplicationHeaders().getAsString(SUBJECT);
}

}}} // namespace qpid::client::amqp0_10
