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

#include "qpid/Exception.h"
#include "qpid/broker/Exchange.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/DeliverableMessage.h"
#include "qpid/broker/DirectExchange.h"
#include "qpid/broker/ExchangeRegistry.h"
#include "qpid/broker/FanOutExchange.h"
#include "qpid/broker/HeadersExchange.h"
#include "qpid/broker/TopicExchange.h"
#include "qpid/framing/reply_exceptions.h"
#include "unit_test.h"
#include <iostream>
#include "MessageUtils.h"

using boost::intrusive_ptr;
using namespace qpid::broker;
using namespace qpid::framing;
using namespace qpid::sys;
using namespace qpid;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ExchangeTestSuite)

QPID_AUTO_TEST_CASE(testMe)
{
    Queue::shared_ptr queue(new Queue("queue", true));
    Queue::shared_ptr queue2(new Queue("queue2", true));

    TopicExchange topic("topic");
    topic.bind(queue, "abc", 0);
    topic.bind(queue2, "abc", 0);

    DirectExchange direct("direct");
    direct.bind(queue, "abc", 0);
    direct.bind(queue2, "abc", 0);

    queue.reset();
    queue2.reset();

    intrusive_ptr<Message> msgPtr(MessageUtils::createMessage("exchange", "key", false, "id"));
    DeliverableMessage msg(msgPtr);
    topic.route(msg, "abc", 0);
    direct.route(msg, "abc", 0);

}

QPID_AUTO_TEST_CASE(testIsBound)
{
    Queue::shared_ptr a(new Queue("a", true));
    Queue::shared_ptr b(new Queue("b", true));
    Queue::shared_ptr c(new Queue("c", true));
    Queue::shared_ptr d(new Queue("d", true));

    string k1("abc");
    string k2("def");
    string k3("xyz");

    FanOutExchange fanout("fanout");
    BOOST_CHECK(fanout.bind(a, "", 0));
    BOOST_CHECK(fanout.bind(b, "", 0));
    BOOST_CHECK(fanout.bind(c, "", 0));

    BOOST_CHECK(fanout.isBound(a, 0, 0));
    BOOST_CHECK(fanout.isBound(b, 0, 0));
    BOOST_CHECK(fanout.isBound(c, 0, 0));
    BOOST_CHECK(!fanout.isBound(d, 0, 0));

    DirectExchange direct("direct");
    BOOST_CHECK(direct.bind(a, k1, 0));
    BOOST_CHECK(direct.bind(a, k3, 0));
    BOOST_CHECK(direct.bind(b, k2, 0));
    BOOST_CHECK(direct.bind(c, k1, 0));

    BOOST_CHECK(direct.isBound(a, 0, 0));
    BOOST_CHECK(direct.isBound(a, &k1, 0));
    BOOST_CHECK(direct.isBound(a, &k3, 0));
    BOOST_CHECK(!direct.isBound(a, &k2, 0));
    BOOST_CHECK(direct.isBound(b, 0, 0));
    BOOST_CHECK(direct.isBound(b, &k2, 0));
    BOOST_CHECK(direct.isBound(c, &k1, 0));
    BOOST_CHECK(!direct.isBound(d, 0, 0));
    BOOST_CHECK(!direct.isBound(d, &k1, 0));
    BOOST_CHECK(!direct.isBound(d, &k2, 0));
    BOOST_CHECK(!direct.isBound(d, &k3, 0));

    TopicExchange topic("topic");
    BOOST_CHECK(topic.bind(a, k1, 0));
    BOOST_CHECK(topic.bind(a, k3, 0));
    BOOST_CHECK(topic.bind(b, k2, 0));
    BOOST_CHECK(topic.bind(c, k1, 0));

    BOOST_CHECK(topic.isBound(a, 0, 0));
    BOOST_CHECK(topic.isBound(a, &k1, 0));
    BOOST_CHECK(topic.isBound(a, &k3, 0));
    BOOST_CHECK(!topic.isBound(a, &k2, 0));
    BOOST_CHECK(topic.isBound(b, 0, 0));
    BOOST_CHECK(topic.isBound(b, &k2, 0));
    BOOST_CHECK(topic.isBound(c, &k1, 0));
    BOOST_CHECK(!topic.isBound(d, 0, 0));
    BOOST_CHECK(!topic.isBound(d, &k1, 0));
    BOOST_CHECK(!topic.isBound(d, &k2, 0));
    BOOST_CHECK(!topic.isBound(d, &k3, 0));

    HeadersExchange headers("headers");
    FieldTable args1;
    args1.setString("x-match", "all");
    args1.setString("a", "A");
    args1.setInt("b", 1);
    FieldTable args2;
    args2.setString("x-match", "any");
    args2.setString("a", "A");
    args2.setInt("b", 1);
    FieldTable args3;
    args3.setString("x-match", "any");
    args3.setString("c", "C");
    args3.setInt("b", 6);

    headers.bind(a, "", &args1);
    headers.bind(a, "", &args3);
    headers.bind(b, "", &args2);
    headers.bind(c, "", &args1);

    BOOST_CHECK(headers.isBound(a, 0, 0));
    BOOST_CHECK(headers.isBound(a, 0, &args1));
    BOOST_CHECK(headers.isBound(a, 0, &args3));
    BOOST_CHECK(!headers.isBound(a, 0, &args2));
    BOOST_CHECK(headers.isBound(b, 0, 0));
    BOOST_CHECK(headers.isBound(b, 0, &args2));
    BOOST_CHECK(headers.isBound(c, 0, &args1));
    BOOST_CHECK(!headers.isBound(d, 0, 0));
    BOOST_CHECK(!headers.isBound(d, 0, &args1));
    BOOST_CHECK(!headers.isBound(d, 0, &args2));
    BOOST_CHECK(!headers.isBound(d, 0, &args3));
}

QPID_AUTO_TEST_CASE(testDeleteGetAndRedeclare)
{
    ExchangeRegistry exchanges;
    exchanges.declare("my-exchange", "direct", false, FieldTable());
    exchanges.destroy("my-exchange");
    try {
        exchanges.get("my-exchange");
    } catch (const NotFoundException&) {}
    std::pair<Exchange::shared_ptr, bool> response = exchanges.declare("my-exchange", "direct", false, FieldTable());
    BOOST_CHECK_EQUAL(string("direct"), response.first->getType());
}

intrusive_ptr<Message> cmessage(std::string exchange, std::string routingKey) {
    intrusive_ptr<Message> msg(new Message());
    AMQFrame method((MessageTransferBody(ProtocolVersion(), exchange, 0, 0)));
    AMQFrame header((AMQHeaderBody()));
    msg->getFrames().append(method);
    msg->getFrames().append(header);
    msg->getFrames().getHeaders()->get<DeliveryProperties>(true)->setRoutingKey(routingKey);
    return msg;
}

QPID_AUTO_TEST_CASE(testSequenceOptions)
{
    FieldTable args;
    args.setInt("qpid.msg_sequence",1);
    char* buff = new char[10000];
    framing::Buffer buffer(buff,10000);
    {
        DirectExchange direct("direct1", false, args);

        intrusive_ptr<Message> msg1 = cmessage("e", "A");
        intrusive_ptr<Message> msg2 = cmessage("e", "B");
        intrusive_ptr<Message> msg3 = cmessage("e", "C");

        DeliverableMessage dmsg1(msg1);
        DeliverableMessage dmsg2(msg2);
        DeliverableMessage dmsg3(msg3);

        direct.route(dmsg1, "abc", 0);
        direct.route(dmsg2, "abc", 0);
        direct.route(dmsg3, "abc", 0);

        BOOST_CHECK_EQUAL(1, msg1->getApplicationHeaders()->getAsInt64("qpid.msg_sequence"));
        BOOST_CHECK_EQUAL(2, msg2->getApplicationHeaders()->getAsInt64("qpid.msg_sequence"));
        BOOST_CHECK_EQUAL(3, msg3->getApplicationHeaders()->getAsInt64("qpid.msg_sequence"));

        FanOutExchange fanout("fanout1", false, args);
        HeadersExchange header("headers1", false, args);
        TopicExchange topic ("topic1", false, args);

        // check other exchanges, that they preroute
        intrusive_ptr<Message> msg4 = cmessage("e", "A");
        intrusive_ptr<Message> msg5 = cmessage("e", "B");
        intrusive_ptr<Message> msg6 = cmessage("e", "C");

        DeliverableMessage dmsg4(msg4);
        DeliverableMessage dmsg5(msg5);
        DeliverableMessage dmsg6(msg6);

        fanout.route(dmsg4, "abc", 0);
        BOOST_CHECK_EQUAL(1, msg4->getApplicationHeaders()->getAsInt64("qpid.msg_sequence"));

        FieldTable headers;
        header.route(dmsg5, "abc", &headers);
        BOOST_CHECK_EQUAL(1, msg5->getApplicationHeaders()->getAsInt64("qpid.msg_sequence"));

        topic.route(dmsg6, "abc", 0);
        BOOST_CHECK_EQUAL(1, msg6->getApplicationHeaders()->getAsInt64("qpid.msg_sequence"));
        direct.encode(buffer);
    }
    {

        ExchangeRegistry exchanges;
        buffer.reset();
        DirectExchange::shared_ptr exch_dec = Exchange::decode(exchanges, buffer);

        intrusive_ptr<Message> msg1 = cmessage("e", "A");
        DeliverableMessage dmsg1(msg1);
        exch_dec->route(dmsg1, "abc", 0);

        BOOST_CHECK_EQUAL(4, msg1->getApplicationHeaders()->getAsInt64("qpid.msg_sequence"));

    }
    delete [] buff;
}

QPID_AUTO_TEST_CASE(testIVEOption)
{
    FieldTable args;
    args.setInt("qpid.ive",1);
    DirectExchange direct("direct1", false, args);
    FanOutExchange fanout("fanout1", false, args);
    HeadersExchange header("headers1", false, args);
    TopicExchange topic ("topic1", false, args);

    intrusive_ptr<Message> msg1 = cmessage("direct1", "abc");
    msg1->getProperties<MessageProperties>()->getApplicationHeaders().setString("a", "abc");
    DeliverableMessage dmsg1(msg1);

    FieldTable args2;
    args2.setString("x-match", "any");
    args2.setString("a", "abc");

    direct.route(dmsg1, "abc", 0);
    fanout.route(dmsg1, "abc", 0);
    header.route(dmsg1, "abc", &args2);
    topic.route(dmsg1, "abc", 0);
    Queue::shared_ptr queue(new Queue("queue", true));
    Queue::shared_ptr queue1(new Queue("queue1", true));
    Queue::shared_ptr queue2(new Queue("queue2", true));
    Queue::shared_ptr queue3(new Queue("queue3", true));

    BOOST_CHECK(HeadersExchange::match(args2, msg1->getProperties<MessageProperties>()->getApplicationHeaders()));

    BOOST_CHECK(direct.bind(queue, "abc", 0));
    BOOST_CHECK(fanout.bind(queue1, "abc", 0));
    BOOST_CHECK(header.bind(queue2, "", &args2));
    BOOST_CHECK(topic.bind(queue3, "abc", 0));

    BOOST_CHECK_EQUAL(1u,queue->getMessageCount());
    BOOST_CHECK_EQUAL(1u,queue1->getMessageCount());
    BOOST_CHECK_EQUAL(1u,queue2->getMessageCount());
    BOOST_CHECK_EQUAL(1u,queue3->getMessageCount());

}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
