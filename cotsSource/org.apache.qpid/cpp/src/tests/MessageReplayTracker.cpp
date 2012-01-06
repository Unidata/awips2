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
#include "BrokerFixture.h"
#include "qpid/client/MessageReplayTracker.h"
#include "qpid/sys/Time.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(MessageReplayTrackerTests)

using namespace qpid::client;
using namespace qpid::sys;
using std::string;

class ReplayBufferChecker
{
  public:

    ReplayBufferChecker(uint from, uint to) : end(to), i(from) {}

    void operator()(const Message& m)
    {
        if (i > end) BOOST_FAIL("Extra message found: " + m.getData());
        BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i++)).str(), m.getData());
    }
  private:
    const uint end;
    uint i;

};

QPID_AUTO_TEST_CASE(testReplay)
{
    ProxySessionFixture fix;
    fix.session.queueDeclare(arg::queue="my-queue", arg::exclusive=true, arg::autoDelete=true);

    MessageReplayTracker tracker(10);
    tracker.init(fix.session);
    for (uint i = 0; i < 5; i++) {
        Message message((boost::format("Message_%1%") % (i+1)).str(), "my-queue");
        tracker.send(message);
    }
    ReplayBufferChecker checker(1, 10);
    tracker.foreach(checker);

    tracker.replay(fix.session);
    for (uint j = 0; j < 2; j++) {//each message should have been sent twice
        for (uint i = 0; i < 5; i++) {
            Message m;
            BOOST_CHECK(fix.subs.get(m, "my-queue", TIME_SEC));
            BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i+1)).str(), m.getData());
        }
    }
    Message m;
    BOOST_CHECK(!fix.subs.get(m, "my-queue"));
}

QPID_AUTO_TEST_CASE(testCheckCompletion)
{
    ProxySessionFixture fix;
    fix.session.queueDeclare(arg::queue="my-queue", arg::exclusive=true, arg::autoDelete=true);

    MessageReplayTracker tracker(10);
    tracker.init(fix.session);
    for (uint i = 0; i < 5; i++) {
        Message message((boost::format("Message_%1%") % (i+1)).str(), "my-queue");
        tracker.send(message);
    }
    fix.session.sync();//ensures all messages are complete
    tracker.checkCompletion();
    tracker.replay(fix.session);
    Message received;
    for (uint i = 0; i < 5; i++) {
        BOOST_CHECK(fix.subs.get(received, "my-queue"));
        BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i+1)).str(), received.getData());
    }
    BOOST_CHECK(!fix.subs.get(received, "my-queue"));
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
