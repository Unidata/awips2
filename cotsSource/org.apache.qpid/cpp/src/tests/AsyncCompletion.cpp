/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */


#include "unit_test.h"
#include "test_tools.h"
#include "BrokerFixture.h"
#include "qpid/broker/NullMessageStore.h"
#include "qpid/sys/BlockingQueue.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/sys/Time.h"
#include "qpid/framing/QueueQueryResult.h"
#include "qpid/client/TypedResult.h"

using namespace std;
using namespace qpid;
using namespace client;
using namespace framing;

namespace qpid { namespace broker {
class TransactionContext;
class PersistableQueue;
}}

using broker::PersistableMessage;
using broker::NullMessageStore;
using broker::TransactionContext;
using broker::PersistableQueue;
using sys::TIME_SEC;
using boost::intrusive_ptr;

/** @file Unit tests for async completion.
 * Using a dummy store, verify that the broker indicates async completion of
 * message enqueues at the correct time.
 */

namespace qpid {
namespace tests {

class AsyncCompletionMessageStore : public NullMessageStore {
  public:
    sys::BlockingQueue<boost::intrusive_ptr<PersistableMessage> > enqueued;

    AsyncCompletionMessageStore() : NullMessageStore() {}
    ~AsyncCompletionMessageStore(){}

    void enqueue(TransactionContext*,
                 const boost::intrusive_ptr<PersistableMessage>& msg,
                 const PersistableQueue& )
    {
        enqueued.push(msg);
    }
};

QPID_AUTO_TEST_SUITE(AsyncCompletionTestSuite)

QPID_AUTO_TEST_CASE(testWaitTillComplete) {
    SessionFixture fix;
    AsyncCompletionMessageStore* store = new AsyncCompletionMessageStore;
    boost::shared_ptr<qpid::broker::MessageStore> p;
    p.reset(store);
    fix.broker->setStore(p);
    AsyncSession s = fix.session;

    static const int count = 3;

    s.queueDeclare("q", arg::durable=true);
    Completion transfers[count];
    for (int i = 0; i < count; ++i) {
        Message msg(boost::lexical_cast<string>(i), "q");
        msg.getDeliveryProperties().setDeliveryMode(PERSISTENT);
        transfers[i] = s.messageTransfer(arg::content=msg);
    }

    // Get hold of the broker-side messages.
    typedef vector<intrusive_ptr<PersistableMessage> > BrokerMessages;
    BrokerMessages enqueued;
    for (int j = 0; j < count; ++j)
        enqueued.push_back(store->enqueued.pop(TIME_SEC));

    // Send a sync, make sure it does not complete till all messages are complete.
    // In reverse order for fun.
    Completion sync = s.executionSync(arg::sync=true);
    for (int k = count-1; k >= 0; --k) {
        BOOST_CHECK(!transfers[k].isComplete()); // Should not be complete yet.
        BOOST_CHECK(!sync.isComplete()); // Should not be complete yet.
        enqueued[k]->enqueueComplete();
    }
    sync.wait();                // Should complete now, all messages are completed.
}

QPID_AUTO_TEST_CASE(testGetResult) {
    SessionFixture fix;
    AsyncSession s = fix.session;

    s.queueDeclare("q", arg::durable=true);
    TypedResult<QueueQueryResult> tr = s.queueQuery("q");
    QueueQueryResult qq = tr.get();
    BOOST_CHECK_EQUAL(qq.getQueue(), "q");
    BOOST_CHECK_EQUAL(qq.getMessageCount(), 0U);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
