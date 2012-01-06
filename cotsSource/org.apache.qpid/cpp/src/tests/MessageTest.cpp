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
#include "qpid/framing/AMQP_HighestVersion.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/framing/Uuid.h"
#include "qpid/sys/alloca.h"

#include "unit_test.h"

#include <iostream>

using namespace qpid::broker;
using namespace qpid::framing;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(MessageTestSuite)

QPID_AUTO_TEST_CASE(testEncodeDecode)
{
    string exchange = "MyExchange";
    string routingKey = "MyRoutingKey";
    Uuid messageId(true);
    string data1("abcdefg");
    string data2("hijklmn");

    boost::intrusive_ptr<Message> msg(new Message());

    AMQFrame method((MessageTransferBody(ProtocolVersion(), exchange, 0, 0)));
    AMQFrame header((AMQHeaderBody()));
    AMQFrame content1((AMQContentBody(data1)));
    AMQFrame content2((AMQContentBody(data2)));

    msg->getFrames().append(method);
    msg->getFrames().append(header);
    msg->getFrames().append(content1);
    msg->getFrames().append(content2);

    MessageProperties* mProps = msg->getFrames().getHeaders()->get<MessageProperties>(true);
    mProps->setContentLength(data1.size() + data2.size());
    mProps->setMessageId(messageId);
    FieldTable applicationHeaders;
    applicationHeaders.setString("abc", "xyz");
    mProps->setApplicationHeaders(applicationHeaders);
    DeliveryProperties* dProps = msg->getFrames().getHeaders()->get<DeliveryProperties>(true);
    dProps->setRoutingKey(routingKey);
    dProps->setDeliveryMode(PERSISTENT);
    BOOST_CHECK(msg->isPersistent());

    char* buff = static_cast<char*>(::alloca(msg->encodedSize()));
    Buffer wbuffer(buff, msg->encodedSize());
    msg->encode(wbuffer);

    Buffer rbuffer(buff, msg->encodedSize());
    msg = new Message();
    msg->decodeHeader(rbuffer);
    msg->decodeContent(rbuffer);
    BOOST_CHECK_EQUAL(exchange, msg->getExchangeName());
    BOOST_CHECK_EQUAL(routingKey, msg->getRoutingKey());
    BOOST_CHECK_EQUAL((uint64_t) data1.size() + data2.size(), msg->contentSize());
    BOOST_CHECK_EQUAL((uint64_t) data1.size() + data2.size(), msg->getProperties<MessageProperties>()->getContentLength());
    BOOST_CHECK_EQUAL(messageId, msg->getProperties<MessageProperties>()->getMessageId());
    BOOST_CHECK_EQUAL(string("xyz"), msg->getProperties<MessageProperties>()->getApplicationHeaders().getAsString("abc"));
    BOOST_CHECK_EQUAL((uint8_t) PERSISTENT, msg->getProperties<DeliveryProperties>()->getDeliveryMode());
    BOOST_CHECK(msg->isPersistent());
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
