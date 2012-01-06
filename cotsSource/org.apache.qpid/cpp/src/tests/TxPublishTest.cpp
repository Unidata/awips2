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
#include "qpid/broker/NullMessageStore.h"
#include "qpid/broker/RecoveryManager.h"
#include "qpid/broker/TxPublish.h"
#include "unit_test.h"
#include <iostream>
#include <list>
#include <vector>
#include "MessageUtils.h"
#include "TestMessageStore.h"

using std::list;
using std::pair;
using std::vector;
using boost::intrusive_ptr;
using namespace qpid::broker;
using namespace qpid::framing;

namespace qpid {
namespace tests {

struct TxPublishTest
{

    TestMessageStore store;
    Queue::shared_ptr queue1;
    Queue::shared_ptr queue2;
    intrusive_ptr<Message> msg;
    TxPublish op;

    TxPublishTest() :
        queue1(new Queue("queue1", false, &store, 0)),
        queue2(new Queue("queue2", false, &store, 0)),
        msg(MessageUtils::createMessage("exchange", "routing_key", false, "id")),
        op(msg)
    {
        msg->getProperties<DeliveryProperties>()->setDeliveryMode(PERSISTENT);
        op.deliverTo(queue1);
        op.deliverTo(queue2);
    }
};


QPID_AUTO_TEST_SUITE(TxPublishTestSuite)

QPID_AUTO_TEST_CASE(testPrepare)
{
    TxPublishTest t;

    intrusive_ptr<PersistableMessage> pmsg = boost::static_pointer_cast<PersistableMessage>(t.msg);
    //ensure messages are enqueued in store
    t.op.prepare(0);
    BOOST_CHECK_EQUAL((size_t) 2, t.store.enqueued.size());
    BOOST_CHECK_EQUAL(string("queue1"), t.store.enqueued[0].first);
    BOOST_CHECK_EQUAL(pmsg, t.store.enqueued[0].second);
    BOOST_CHECK_EQUAL(string("queue2"), t.store.enqueued[1].first);
    BOOST_CHECK_EQUAL(pmsg, t.store.enqueued[1].second);
    BOOST_CHECK_EQUAL( true, ( boost::static_pointer_cast<PersistableMessage>(t.msg))->isEnqueueComplete());
}

QPID_AUTO_TEST_CASE(testCommit)
{
    TxPublishTest t;

    //ensure messages are delivered to queue
    t.op.prepare(0);
    t.op.commit();
    BOOST_CHECK_EQUAL((uint32_t) 1, t.queue1->getMessageCount());
    intrusive_ptr<Message> msg_dequeue = t.queue1->get().payload;

    BOOST_CHECK_EQUAL( true, (boost::static_pointer_cast<PersistableMessage>(msg_dequeue))->isEnqueueComplete());
    BOOST_CHECK_EQUAL(t.msg, msg_dequeue);

    BOOST_CHECK_EQUAL((uint32_t) 1, t.queue2->getMessageCount());
    BOOST_CHECK_EQUAL(t.msg, t.queue2->get().payload);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
