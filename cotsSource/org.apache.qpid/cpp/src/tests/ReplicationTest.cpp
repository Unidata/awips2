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
#include "BrokerFixture.h"

#include "qpid/Plugin.h"
#include "qpid/broker/Broker.h"
#include "qpid/client/QueueOptions.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/SequenceNumber.h"
#include "qpid/replication/constants.h"
#include "qpid/sys/Shlib.h"

#include <string>
#include <sstream>
#include <vector>

#include <boost/assign.hpp>
#include <boost/bind.hpp>

using namespace qpid::client;
using namespace qpid::framing;
using namespace qpid::replication::constants;
using boost::assign::list_of;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ReplicationTestSuite)

// FIXME aconway 2009-11-26: clean this up.
// The CMake-based build passes in the module suffix; if it's not there, this
// is a Linux/UNIX libtool-based build.
#if defined (QPID_MODULE_SUFFIX)
qpid::sys::Shlib plugin("replicating_listener" QPID_MODULE_SUFFIX);
#else
qpid::sys::Shlib plugin(getLibPath("REPLICATING_LISTENER_LIB"));
#endif

qpid::broker::Broker::Options getBrokerOpts(const std::vector<std::string>& args)
{
    std::vector<const char*> argv(args.size());
    transform(args.begin(), args.end(), argv.begin(), boost::bind(&string::c_str, _1));

    qpid::broker::Broker::Options opts;
    qpid::Plugin::addOptions(opts);
    opts.parse(argv.size(), &argv[0], "", true);
    return opts;
}

QPID_AUTO_TEST_CASE(testReplicationExchange)
{
    qpid::broker::Broker::Options brokerOpts(getBrokerOpts(list_of<string>("qpidd")
                                                           ("--replication-exchange-name=qpid.replication")));
    ProxySessionFixture f(brokerOpts);


    std::string dataQ("queue-1");
    std::string eventQ("event-queue-1");
    std::string dataQ2("queue-2");
    std::string eventQ2("event-queue-2");
    FieldTable eventQopts;
    eventQopts.setString("qpid.insert_sequence_numbers", REPLICATION_EVENT_SEQNO);

    f.session.queueDeclare(arg::queue=eventQ, arg::exclusive=true, arg::autoDelete=true, arg::arguments=eventQopts);
    f.session.exchangeBind(arg::exchange="qpid.replication", arg::queue=eventQ, arg::bindingKey=dataQ);

    f.session.queueDeclare(arg::queue=eventQ2, arg::exclusive=true, arg::autoDelete=true, arg::arguments=eventQopts);
    f.session.exchangeBind(arg::exchange="qpid.replication", arg::queue=eventQ2, arg::bindingKey=dataQ2);

    QueueOptions args;
    args.enableQueueEvents(false);
    f.session.queueDeclare(arg::queue=dataQ, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);
    f.session.queueDeclare(arg::queue=dataQ2, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);
    for (int i = 0; i < 10; i++) {
        f.session.messageTransfer(arg::content=Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), dataQ));
        f.session.messageTransfer(arg::content=Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), dataQ2));
    }
    Message msg;
    LocalQueue incoming;
    Subscription sub = f.subs.subscribe(incoming, dataQ);
    for (int i = 0; i < 10; i++) {
        BOOST_CHECK(incoming.get(msg, qpid::sys::TIME_SEC));
        BOOST_CHECK_EQUAL((boost::format("%1%_%2%") % "Message" % (i+1)).str(), msg.getData());
    }
    BOOST_CHECK(!f.subs.get(msg, dataQ));

    sub.cancel();
    sub = f.subs.subscribe(incoming, eventQ);
    //check that we received enqueue events for first queue:
    for (int i = 0; i < 10; i++) {
        BOOST_CHECK(incoming.get(msg, qpid::sys::TIME_SEC));
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsString(REPLICATION_TARGET_QUEUE), dataQ);
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsInt(REPLICATION_EVENT_TYPE), ENQUEUE);
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsInt64(REPLICATION_EVENT_SEQNO), (i+1));
        BOOST_CHECK_EQUAL((boost::format("%1%_%2%") % "Message" % (i+1)).str(), msg.getData());
    }
    //check that we received dequeue events for first queue:
    for (int i = 0; i < 10; i++) {
        BOOST_CHECK(incoming.get(msg, qpid::sys::TIME_SEC));
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsString(REPLICATION_TARGET_QUEUE), dataQ);
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsInt(REPLICATION_EVENT_TYPE), DEQUEUE);
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsInt(DEQUEUED_MESSAGE_POSITION), (i+1));
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsInt64(REPLICATION_EVENT_SEQNO), (i+11));
    }

    sub.cancel();
    sub = f.subs.subscribe(incoming, eventQ2);
    //check that we received enqueue events for second queue:
    for (int i = 0; i < 10; i++) {
        BOOST_CHECK(incoming.get(msg, qpid::sys::TIME_SEC));
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsString(REPLICATION_TARGET_QUEUE), dataQ2);
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsInt(REPLICATION_EVENT_TYPE), ENQUEUE);
        BOOST_CHECK_EQUAL(msg.getHeaders().getAsInt64(REPLICATION_EVENT_SEQNO), (i+1));
        BOOST_CHECK_EQUAL((boost::format("%1%_%2%") % "Message" % (i+1)).str(), msg.getData());
    }
}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
