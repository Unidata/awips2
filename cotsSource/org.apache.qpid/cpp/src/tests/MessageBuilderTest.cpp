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
#include "qpid/broker/MessageBuilder.h"
#include "qpid/broker/NullMessageStore.h"
#include "qpid/framing/frame_functors.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/framing/TypeFilter.h"
#include "unit_test.h"
#include <list>

using namespace qpid::broker;
using namespace qpid::framing;
using namespace qpid::sys;

namespace qpid {
namespace tests {

class MockMessageStore : public NullMessageStore
{
    enum Op {STAGE=1, APPEND=2};

    uint64_t id;
    boost::intrusive_ptr<PersistableMessage> expectedMsg;
    string expectedData;
    std::list<Op> ops;

    void checkExpectation(Op actual)
    {
        BOOST_CHECK_EQUAL(ops.front(), actual);
        ops.pop_front();
    }

  public:
    MockMessageStore() : id(0), expectedMsg(0) {}

    void expectStage(PersistableMessage& msg)
    {
        expectedMsg = &msg;
        ops.push_back(STAGE);
    }

    void expectAppendContent(PersistableMessage& msg, const string& data)
    {
        expectedMsg = &msg;
        expectedData = data;
        ops.push_back(APPEND);
    }

    void stage(const boost::intrusive_ptr<PersistableMessage>& msg)
    {
        checkExpectation(STAGE);
        BOOST_CHECK_EQUAL(expectedMsg, msg);
        msg->setPersistenceId(++id);
    }

    void appendContent(const boost::intrusive_ptr<const PersistableMessage>& msg,
                       const string& data)
    {
        checkExpectation(APPEND);
        BOOST_CHECK_EQUAL(boost::static_pointer_cast<const PersistableMessage>(expectedMsg), msg);
        BOOST_CHECK_EQUAL(expectedData, data);
    }

    bool expectationsMet()
    {
        return ops.empty();
    }

    //don't treat this store as a null impl
    bool isNull() const
    {
        return false;
    }

};

QPID_AUTO_TEST_SUITE(MessageBuilderTestSuite)

QPID_AUTO_TEST_CASE(testHeaderOnly)
{
    MessageBuilder builder(0, 0);
    builder.start(SequenceNumber());

    std::string exchange("builder-exchange");
    std::string key("builder-exchange");

    AMQFrame method((MessageTransferBody(ProtocolVersion(), exchange, 0, 0)));
    AMQFrame header((AMQHeaderBody()));

    header.castBody<AMQHeaderBody>()->get<MessageProperties>(true)->setContentLength(0);
    header.castBody<AMQHeaderBody>()->get<DeliveryProperties>(true)->setRoutingKey(key);

    builder.handle(method);
    builder.handle(header);

    BOOST_CHECK(builder.getMessage());
    BOOST_CHECK_EQUAL(exchange, builder.getMessage()->getExchangeName());
    BOOST_CHECK_EQUAL(key, builder.getMessage()->getRoutingKey());
    BOOST_CHECK(builder.getMessage()->getFrames().isComplete());
}

QPID_AUTO_TEST_CASE(test1ContentFrame)
{
    MessageBuilder builder(0, 0);
    builder.start(SequenceNumber());

    std::string data("abcdefg");
    std::string exchange("builder-exchange");
    std::string key("builder-exchange");

    AMQFrame method((MessageTransferBody(ProtocolVersion(), exchange, 0, 0)));
    AMQFrame header((AMQHeaderBody()));
    AMQFrame content((AMQContentBody(data)));
    method.setEof(false);
    header.setBof(false);
    header.setEof(false);
    content.setBof(false);

    header.castBody<AMQHeaderBody>()->get<MessageProperties>(true)->setContentLength(data.size());
    header.castBody<AMQHeaderBody>()->get<DeliveryProperties>(true)->setRoutingKey(key);

    builder.handle(method);
    BOOST_CHECK(builder.getMessage());
    BOOST_CHECK(!builder.getMessage()->getFrames().isComplete());

    builder.handle(header);
    BOOST_CHECK(builder.getMessage());
    BOOST_CHECK(!builder.getMessage()->getFrames().isComplete());

    builder.handle(content);
    BOOST_CHECK(builder.getMessage());
    BOOST_CHECK(builder.getMessage()->getFrames().isComplete());
}

QPID_AUTO_TEST_CASE(test2ContentFrames)
{
    MessageBuilder builder(0, 0);
    builder.start(SequenceNumber());

    std::string data1("abcdefg");
    std::string data2("hijklmn");
    std::string exchange("builder-exchange");
    std::string key("builder-exchange");

    AMQFrame method((MessageTransferBody(ProtocolVersion(), exchange, 0, 0)));
    AMQFrame header((AMQHeaderBody()));
    AMQFrame content1((AMQContentBody(data1)));
    AMQFrame content2((AMQContentBody(data2)));
    method.setEof(false);
    header.setBof(false);
    header.setEof(false);
    content1.setBof(false);
    content1.setEof(false);
    content2.setBof(false);

    header.castBody<AMQHeaderBody>()->get<MessageProperties>(true)->setContentLength(data1.size() + data2.size());
    header.castBody<AMQHeaderBody>()->get<DeliveryProperties>(true)->setRoutingKey(key);

    builder.handle(method);
    builder.handle(header);
    builder.handle(content1);
    BOOST_CHECK(builder.getMessage());
    BOOST_CHECK(!builder.getMessage()->getFrames().isComplete());

    builder.handle(content2);
    BOOST_CHECK(builder.getMessage());
    BOOST_CHECK(builder.getMessage()->getFrames().isComplete());
}

QPID_AUTO_TEST_CASE(testStaging)
{
    MockMessageStore store;
    MessageBuilder builder(&store, 5);
    builder.start(SequenceNumber());

    std::string data1("abcdefg");
    std::string data2("hijklmn");
    std::string exchange("builder-exchange");
    std::string key("builder-exchange");

    AMQFrame method(MessageTransferBody(ProtocolVersion(), exchange, 0, 0));
    AMQFrame header((AMQHeaderBody()));
    AMQFrame content1((AMQContentBody(data1)));
    AMQFrame content2((AMQContentBody(data2)));

    header.castBody<AMQHeaderBody>()->get<MessageProperties>(true)->setContentLength(data1.size() + data2.size());
    header.castBody<AMQHeaderBody>()->get<DeliveryProperties>(true)->setRoutingKey(key);

    builder.handle(method);
    builder.handle(header);

    store.expectStage(*builder.getMessage());
    builder.handle(content1);
    BOOST_CHECK(store.expectationsMet());
    BOOST_CHECK_EQUAL((uint64_t) 1, builder.getMessage()->getPersistenceId());

    store.expectAppendContent(*builder.getMessage(), data2);
    builder.handle(content2);
    BOOST_CHECK(store.expectationsMet());
    //were the content frames dropped?
    BOOST_CHECK(!builder.getMessage()->isContentLoaded());
}

QPID_AUTO_TEST_CASE(testNoManagementStaging)
{
    // Make sure management messages don't stage
    MockMessageStore store;
    MessageBuilder builder(&store, 5);
    builder.start(SequenceNumber());

    std::string data1("abcdefg");
    std::string exchange("qpid.management");
    std::string key("builder-exchange");

    AMQFrame method(MessageTransferBody(ProtocolVersion(), exchange, 0, 0));
    AMQFrame header((AMQHeaderBody()));
    AMQFrame content1((AMQContentBody(data1)));

    header.castBody<AMQHeaderBody>()->get<MessageProperties>(true)->setContentLength(data1.size());
    header.castBody<AMQHeaderBody>()->get<DeliveryProperties>(true)->setRoutingKey(key);

    builder.handle(method);
    builder.handle(header);

    builder.handle(content1);
    BOOST_CHECK(store.expectationsMet());
    BOOST_CHECK_EQUAL((uint64_t) 0, builder.getMessage()->getPersistenceId());
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
