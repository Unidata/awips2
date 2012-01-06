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
#include "unit_test.h"
#include "test_tools.h"

#include "qpid/broker/QueuePolicy.h"
#include "qpid/client/QueueOptions.h"
#include "qpid/sys/Time.h"
#include "qpid/framing/reply_exceptions.h"
#include "MessageUtils.h"
#include "BrokerFixture.h"

using namespace qpid::broker;
using namespace qpid::client;
using namespace qpid::framing;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(QueuePolicyTestSuite)

QueuedMessage createMessage(uint32_t size)
{
    QueuedMessage msg;
    msg.payload = MessageUtils::createMessage();
    MessageUtils::addContent(msg.payload, std::string (size, 'x'));
    return msg;
}


QPID_AUTO_TEST_CASE(testCount)
{
    std::auto_ptr<QueuePolicy> policy(QueuePolicy::createQueuePolicy("test", 5, 0));
    BOOST_CHECK_EQUAL((uint64_t) 0, policy->getMaxSize());
    BOOST_CHECK_EQUAL((uint32_t) 5, policy->getMaxCount());

    QueuedMessage msg = createMessage(10);
    for (size_t i = 0; i < 5; i++) {
        policy->tryEnqueue(msg.payload);
    }
    try {
        policy->tryEnqueue(msg.payload);
        BOOST_FAIL("Policy did not fail on enqueuing sixth message");
    } catch (const ResourceLimitExceededException&) {}

    policy->dequeued(msg);
    policy->tryEnqueue(msg.payload);

    try {
        policy->tryEnqueue(msg.payload);
        BOOST_FAIL("Policy did not fail on enqueuing sixth message (after dequeue)");
    } catch (const ResourceLimitExceededException&) {}
}

QPID_AUTO_TEST_CASE(testSize)
{
    std::auto_ptr<QueuePolicy> policy(QueuePolicy::createQueuePolicy("test", 0, 50));
    QueuedMessage msg = createMessage(10);

    for (size_t i = 0; i < 5; i++) {
        policy->tryEnqueue(msg.payload);
    }
    try {
        policy->tryEnqueue(msg.payload);
        BOOST_FAIL("Policy did not fail on aggregate size exceeding 50. " << *policy);
    } catch (const ResourceLimitExceededException&) {}

    policy->dequeued(msg);
    policy->tryEnqueue(msg.payload);

    try {
        policy->tryEnqueue(msg.payload);
        BOOST_FAIL("Policy did not fail on aggregate size exceeding 50 (after dequeue). " << *policy);
    } catch (const ResourceLimitExceededException&) {}
}

QPID_AUTO_TEST_CASE(testBoth)
{
    std::auto_ptr<QueuePolicy> policy(QueuePolicy::createQueuePolicy("test", 5, 50));
    try {
        QueuedMessage msg = createMessage(51);
        policy->tryEnqueue(msg.payload);
        BOOST_FAIL("Policy did not fail on single message exceeding 50. " << *policy);
    } catch (const ResourceLimitExceededException&) {}

    std::vector<QueuedMessage> messages;
    messages.push_back(createMessage(15));
    messages.push_back(createMessage(10));
    messages.push_back(createMessage(11));
    messages.push_back(createMessage(2));
    messages.push_back(createMessage(7));
    for (size_t i = 0; i < messages.size(); i++) {
        policy->tryEnqueue(messages[i].payload);
    }
    //size = 45 at this point, count = 5
    try {
        QueuedMessage msg = createMessage(5);
        policy->tryEnqueue(msg.payload);
        BOOST_FAIL("Policy did not fail on count exceeding 6. " << *policy);
    } catch (const ResourceLimitExceededException&) {}
    try {
        QueuedMessage msg = createMessage(10);
        policy->tryEnqueue(msg.payload);
        BOOST_FAIL("Policy did not fail on aggregate size exceeding 50. " << *policy);
    } catch (const ResourceLimitExceededException&) {}


    policy->dequeued(messages[0]);
    try {
        QueuedMessage msg = createMessage(20);
        policy->tryEnqueue(msg.payload);
    } catch (const ResourceLimitExceededException&) {
        BOOST_FAIL("Policy failed incorrectly after dequeue. " << *policy);
    }
}

QPID_AUTO_TEST_CASE(testSettings)
{
    //test reading and writing the policy from/to field table
    std::auto_ptr<QueuePolicy> a(QueuePolicy::createQueuePolicy("test", 101, 303));
    FieldTable settings;
    a->update(settings);
    std::auto_ptr<QueuePolicy> b(QueuePolicy::createQueuePolicy("test", settings));
    BOOST_CHECK_EQUAL(a->getMaxCount(), b->getMaxCount());
    BOOST_CHECK_EQUAL(a->getMaxSize(), b->getMaxSize());
}

QPID_AUTO_TEST_CASE(testRingPolicy)
{
    FieldTable args;
    std::auto_ptr<QueuePolicy> policy = QueuePolicy::createQueuePolicy("test", 5, 0, QueuePolicy::RING);
    policy->update(args);

    ProxySessionFixture f;
    std::string q("my-ring-queue");
    f.session.queueDeclare(arg::queue=q, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);
    for (int i = 0; i < 10; i++) {
        f.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    client::Message msg;
    for (int i = 5; i < 10; i++) {
        BOOST_CHECK(f.subs.get(msg, q, qpid::sys::TIME_SEC));
        BOOST_CHECK_EQUAL((boost::format("%1%_%2%") % "Message" % (i+1)).str(), msg.getData());
    }
    BOOST_CHECK(!f.subs.get(msg, q));

    for (int i = 10; i < 20; i++) {
        f.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    for (int i = 15; i < 20; i++) {
        BOOST_CHECK(f.subs.get(msg, q, qpid::sys::TIME_SEC));
        BOOST_CHECK_EQUAL((boost::format("%1%_%2%") % "Message" % (i+1)).str(), msg.getData());
    }
    BOOST_CHECK(!f.subs.get(msg, q));
}

QPID_AUTO_TEST_CASE(testStrictRingPolicy)
{
    FieldTable args;
    std::auto_ptr<QueuePolicy> policy = QueuePolicy::createQueuePolicy("test", 5, 0, QueuePolicy::RING_STRICT);
    policy->update(args);

    ProxySessionFixture f;
    std::string q("my-ring-queue");
    f.session.queueDeclare(arg::queue=q, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);
    LocalQueue incoming;
    SubscriptionSettings settings(FlowControl::unlimited());
    settings.autoAck = 0; // no auto ack.
    Subscription sub = f.subs.subscribe(incoming, q, settings);
    for (int i = 0; i < 5; i++) {
        f.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    for (int i = 0; i < 5; i++) {
        BOOST_CHECK_EQUAL(incoming.pop().getData(), (boost::format("%1%_%2%") % "Message" % (i+1)).str());
    }
    try {
        ScopedSuppressLogging sl; // Suppress messages for expected errors.
        f.session.messageTransfer(arg::content=client::Message("Message_6", q));
        BOOST_FAIL("expecting ResourceLimitExceededException.");
    } catch (const ResourceLimitExceededException&) {}
}

QPID_AUTO_TEST_CASE(testPolicyWithDtx)
{
    FieldTable args;
    std::auto_ptr<QueuePolicy> policy = QueuePolicy::createQueuePolicy("test", 5, 0, QueuePolicy::REJECT);
    policy->update(args);

    ProxySessionFixture f;
    std::string q("my-policy-queue");
    f.session.queueDeclare(arg::queue=q, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);
    LocalQueue incoming;
    SubscriptionSettings settings(FlowControl::unlimited());
    settings.autoAck = 0; // no auto ack.
    Subscription sub = f.subs.subscribe(incoming, q, settings);
    f.session.dtxSelect();
    Xid tx1(1, "test-dtx-mgr", "tx1");
    f.session.dtxStart(arg::xid=tx1);
    for (int i = 0; i < 5; i++) {
        f.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    f.session.dtxEnd(arg::xid=tx1);
    f.session.dtxCommit(arg::xid=tx1, arg::onePhase=true);

    Xid tx2(1, "test-dtx-mgr", "tx2");
    f.session.dtxStart(arg::xid=tx2);
    for (int i = 0; i < 5; i++) {
        BOOST_CHECK_EQUAL(incoming.pop().getData(), (boost::format("%1%_%2%") % "Message" % (i+1)).str());
    }
    SequenceSet accepting=sub.getUnaccepted();
    f.session.messageAccept(accepting);
    f.session.dtxEnd(arg::xid=tx2);
    f.session.dtxPrepare(arg::xid=tx2);
    f.session.dtxRollback(arg::xid=tx2);
    f.session.messageRelease(accepting);

    Xid tx3(1, "test-dtx-mgr", "tx3");
    f.session.dtxStart(arg::xid=tx3);
    for (int i = 0; i < 5; i++) {
        incoming.pop();
    }
    accepting=sub.getUnaccepted();
    f.session.messageAccept(accepting);
    f.session.dtxEnd(arg::xid=tx3);
    f.session.dtxPrepare(arg::xid=tx3);

    Session other = f.connection.newSession();
    try {
        ScopedSuppressLogging sl; // Suppress messages for expected errors.
        other.messageTransfer(arg::content=client::Message("Message_6", q));
        BOOST_FAIL("expecting ResourceLimitExceededException.");
    } catch (const ResourceLimitExceededException&) {}

    f.session.dtxCommit(arg::xid=tx3);
    //now retry and this time should succeed
    other = f.connection.newSession();
    other.messageTransfer(arg::content=client::Message("Message_6", q));
}

QPID_AUTO_TEST_CASE(testFlowToDiskWithNoStore)
{
    //Ensure that with no store loaded, we don't flow to disk but
    //fallback to rejecting messages
    QueueOptions args;
    args.setSizePolicy(FLOW_TO_DISK, 0, 5);

    ProxySessionFixture f;
    std::string q("my-queue");
    f.session.queueDeclare(arg::queue=q, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);
    LocalQueue incoming;
    SubscriptionSettings settings(FlowControl::unlimited());
    settings.autoAck = 0; // no auto ack.
    Subscription sub = f.subs.subscribe(incoming, q, settings);
    for (int i = 0; i < 5; i++) {
        f.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    for (int i = 0; i < 5; i++) {
        BOOST_CHECK_EQUAL(incoming.pop().getData(), (boost::format("%1%_%2%") % "Message" % (i+1)).str());
    }
    try {
        ScopedSuppressLogging sl; // Suppress messages for expected errors.
        f.session.messageTransfer(arg::content=client::Message("Message_6", q));
        BOOST_FAIL("expecting ResourceLimitExceededException.");
    } catch (const ResourceLimitExceededException&) {}
}

QPID_AUTO_TEST_CASE(testPolicyFailureOnCommit)
{
    FieldTable args;
    std::auto_ptr<QueuePolicy> policy = QueuePolicy::createQueuePolicy("test", 5, 0, QueuePolicy::REJECT);
    policy->update(args);

    ProxySessionFixture f;
    std::string q("q");
    f.session.queueDeclare(arg::queue=q, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);
    f.session.txSelect();
    for (int i = 0; i < 10; i++) {
        f.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    ScopedSuppressLogging sl; // Suppress messages for expected errors.
    BOOST_CHECK_THROW(f.session.txCommit(), InternalErrorException);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
