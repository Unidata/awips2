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
#include <iostream>
#include <sstream>
#include "qpid/broker/Message.h"
#include "qpid/broker/NullMessageStore.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/IncompleteMessageList.h"

#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(IncompleteMessageListTestSuite)

using namespace qpid::broker;
using namespace qpid::framing;

struct Checker
{
    std::list<SequenceNumber> ids;

    Checker() { }

    Checker(uint start, uint end) {
        for (uint i = start; i <= end; i++) {
            ids.push_back(i);
        }
    }

    Checker& expect(const SequenceNumber& id) {
        ids.push_back(id);
        return *this;
    }

    void operator()(boost::intrusive_ptr<Message> msg) {
        BOOST_CHECK(!ids.empty());
        BOOST_CHECK_EQUAL(msg->getCommandId(), ids.front());
        ids.pop_front();
    }
};

QPID_AUTO_TEST_CASE(testProcessSimple)
{
    IncompleteMessageList list;
    SequenceNumber counter(1);
    //fill up list with messages
    for (int i = 0; i < 5; i++) {
        boost::intrusive_ptr<Message> msg(new Message(counter++));
        list.add(msg);
    }
    //process and ensure they are all passed to completion listener
    list.process(Checker(1, 5), false);
    //process again and ensure none are resent to listener
    list.process(Checker(), false);
}

QPID_AUTO_TEST_CASE(testProcessWithIncomplete)
{
    Queue::shared_ptr queue;
    IncompleteMessageList list;
    SequenceNumber counter(1);
    boost::intrusive_ptr<Message> middle;
    //fill up list with messages
    for (int i = 0; i < 5; i++) {
        boost::intrusive_ptr<Message> msg(new Message(counter++));
        list.add(msg);
        if (i == 2) {
            //mark a message in the middle as incomplete
            msg->enqueueAsync(queue, 0);
            middle = msg;
        }
    }
    //process and ensure only message upto incomplete message are passed to listener
    list.process(Checker(1, 2), false);
    //mark message complete and re-process to get remaining messages sent to listener
    middle->enqueueComplete();
    list.process(Checker(3, 5), false);
}


struct MockStore : public NullMessageStore
{
    Queue::shared_ptr queue;
    boost::intrusive_ptr<Message> msg;

    void flush(const qpid::broker::PersistableQueue& q) {
        BOOST_CHECK_EQUAL(queue.get(), &q);
        msg->enqueueComplete();
    }
};

QPID_AUTO_TEST_CASE(testSyncProcessWithIncomplete)
{
    IncompleteMessageList list;
    SequenceNumber counter(1);
    MockStore store;
    store.queue = Queue::shared_ptr(new Queue("mock-queue"));
    //fill up list with messages
    for (int i = 0; i < 5; i++) {
        boost::intrusive_ptr<Message> msg(new Message(counter++));
        list.add(msg);
        if (i == 2) {
            //mark a message in the middle as incomplete
            msg->enqueueAsync(store.queue, &store);
            store.msg = msg;
        }
    }
    //process with sync bit specified and ensure that all messages are passed to listener
    list.process(Checker(1, 5), true);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
