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

/**@file Tests for partial failure in a cluster.
 * Partial failure means some nodes experience a failure while others do not.
 * In this case the failed nodes must shut down.
 */

#include "test_tools.h"
#include "unit_test.h"
#include "ClusterFixture.h"
#include <boost/assign.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/bind.hpp>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(PartialFailureTestSuite)

using namespace std;
using namespace qpid;
using namespace qpid::cluster;
using namespace qpid::framing;
using namespace qpid::client;
using namespace qpid::client::arg;
using namespace boost::assign;
using broker::Broker;
using boost::shared_ptr;

// Timeout for tests that wait for messages
const sys::Duration TIMEOUT=sys::TIME_SEC/4;

static bool isLogOption(const std::string& s) { return boost::starts_with(s, "--log-enable"); }

void updateArgs(ClusterFixture::Args& args, size_t index) {
    ostringstream clusterLib, testStoreLib, storeName;
    clusterLib << getLibPath("CLUSTER_LIB");
    testStoreLib << getLibPath("TEST_STORE_LIB");
    storeName << "s" << index;
    args.push_back("--auth");
    args.push_back("no");
    args.push_back("--no-module-dir");
    args.push_back("--load-module");
    args.push_back(clusterLib.str());
    args.push_back("--load-module");
    args.push_back(testStoreLib.str());
    args.push_back("--test-store-name");
    args.push_back(storeName.str());
    args.push_back("TMP_DATA_DIR");

    // These tests generate errors deliberately, disable error logging unless a log env var is set.
    if (!::getenv("QPID_TRACE") && !::getenv("QPID_LOG_ENABLE")) {
        remove_if(args.begin(), args.end(), isLogOption);
        args.push_back("--log-enable=critical+:DISABLED"); // hacky way to disable logs.
    }
}

Message pMessage(string data, string q) {
    Message msg(data, q);
    msg.getDeliveryProperties().setDeliveryMode(PERSISTENT);
    return msg;
}

void queueAndSub(Client& c) {
    c.session.queueDeclare(c.name, durable=true);
    c.subs.subscribe(c.lq, c.name);
}

// Handle near-simultaneous errors
QPID_AUTO_TEST_CASE(testCoincidentErrors) {
    ClusterFixture cluster(2, updateArgs, -1);
    Client c0(cluster[0], "c0");
    Client c1(cluster[1], "c1");

    c0.session.queueDeclare("q", durable=true);
    {
        ScopedSuppressLogging allQuiet;
        async(c0.session).messageTransfer(content=pMessage("TEST_STORE_DO: s0[exception]", "q"));
        async(c1.session).messageTransfer(content=pMessage("TEST_STORE_DO: s1[exception]", "q"));

        int alive=0;
        try { Client c00(cluster[0], "c00"); ++alive; c00.close(); } catch (...) {}
        try { Client c11(cluster[1], "c11"); ++alive; c11.close(); } catch (...) {}

        BOOST_CHECK_EQUAL(alive, 1);

        // Close inside ScopedSuppressLogging to avoid warnings 
        c0.close();
        c1.close();
    }
}

// Verify normal cluster-wide errors.
QPID_AUTO_TEST_CASE(testNormalErrors) {
    // FIXME aconway 2009-04-10: Would like to put a scope just around
    // the statements expected to fail (in BOOST_CHECK_yTHROW) but that
    // sproadically lets out messages, possibly because they're in
    // Connection thread.

    ClusterFixture cluster(3, updateArgs, -1);
    Client c0(cluster[0], "c0");
    Client c1(cluster[1], "c1");
    Client c2(cluster[2], "c2");

    {
        ScopedSuppressLogging allQuiet;
        queueAndSub(c0);
        c0.session.messageTransfer(content=Message("x", "c0"));
        BOOST_CHECK_EQUAL(c0.lq.get(TIMEOUT).getData(), "x");

        // Session error.
        BOOST_CHECK_THROW(c0.session.exchangeBind(), SessionException);
        c1.session.messageTransfer(content=Message("stay", "c0")); // Will stay on queue, session c0 is dead.

        // Connection error, kill c1 on all members.
        queueAndSub(c1);
        BOOST_CHECK_THROW(
            c1.session.messageTransfer(
                content=pMessage("TEST_STORE_DO: s0[exception] s1[exception] s2[exception] testNormalErrors", "c1")),
                ConnectionException);
        c2.session.messageTransfer(content=Message("stay", "c1")); // Will stay on queue, session/connection c1 is dead.

        BOOST_CHECK_EQUAL(3u, knownBrokerPorts(c2.connection, 3).size());
        BOOST_CHECK_EQUAL(c2.subs.get("c0", TIMEOUT).getData(), "stay");
        BOOST_CHECK_EQUAL(c2.subs.get("c1", TIMEOUT).getData(), "stay");

        // Close inside ScopedSuppressLogging to avoid warnings 
        c0.close();
        c1.close();
        c2.close();
    }
}


// Test errors after a new member joins to verify frame-sequence-numbers are ok in update.
QPID_AUTO_TEST_CASE(testErrorAfterJoin) {
    ClusterFixture cluster(1, updateArgs, -1);
    Client c0(cluster[0]);
    {
        ScopedSuppressLogging allQuiet;

        c0.session.queueDeclare("q", durable=true);
        c0.session.messageTransfer(content=pMessage("a", "q"));

        // Kill the new guy
        cluster.add();
        Client c1(cluster[1]);
        c0.session.messageTransfer(content=pMessage("TEST_STORE_DO: s1[exception] testErrorAfterJoin", "q"));
        BOOST_CHECK_THROW(c1.session.messageTransfer(content=pMessage("xxx", "q")), TransportFailure);
        BOOST_CHECK_EQUAL(1u, knownBrokerPorts(c0.connection, 1).size());

        // Kill the old guy
        cluster.add();
        Client c2(cluster[2]);
        c2.session.messageTransfer(content=pMessage("TEST_STORE_DO: s0[exception] testErrorAfterJoin2", "q"));
        BOOST_CHECK_THROW(c0.session.messageTransfer(content=pMessage("xxx", "q")), TransportFailure);

        BOOST_CHECK_EQUAL(1u, knownBrokerPorts(c2.connection, 1).size());

        // Close inside ScopedSuppressLogging to avoid warnings 
        c0.close();
        c1.close();
        c2.close();
    }
}

// Test that if one member fails and  others do not, the failure leaves the cluster.
QPID_AUTO_TEST_CASE(testSinglePartialFailure) {
    ClusterFixture cluster(3, updateArgs, -1);
    Client c0(cluster[0], "c0");
    Client c1(cluster[1], "c1");
    Client c2(cluster[2], "c2");

    {
        ScopedSuppressLogging allQuiet;

        c0.session.queueDeclare("q", durable=true);
        c0.session.messageTransfer(content=pMessage("a", "q"));
        // Cause partial failure on c1
        c0.session.messageTransfer(content=pMessage("TEST_STORE_DO: s1[exception] testSinglePartialFailure", "q"));
        BOOST_CHECK_THROW(c1.session.queueQuery("q"), TransportFailure);

        c0.session.messageTransfer(content=pMessage("b", "q"));
        BOOST_CHECK_EQUAL(c0.session.queueQuery("q").getMessageCount(), 3u);
        BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c0.connection, 2).size());

        // Cause partial failure on c2
        c0.session.messageTransfer(content=pMessage("TEST_STORE_DO: s2[exception] testSinglePartialFailure2", "q"));
        BOOST_CHECK_THROW(c2.session.queueQuery("q"), TransportFailure);

        c0.session.messageTransfer(content=pMessage("c", "q"));
        BOOST_CHECK_EQUAL(c0.session.queueQuery("q").getMessageCount(), 5u);
        BOOST_CHECK_EQUAL(1u, knownBrokerPorts(c0.connection, 1).size());

        // Close inside ScopedSuppressLogging to avoid warnings 
        c0.close();
        c1.close();
        c2.close();
    }
}

// Test multiple partial falures: 2 fail 2 pass
QPID_AUTO_TEST_CASE(testMultiPartialFailure) {
    ClusterFixture cluster(4, updateArgs, -1);
    Client c0(cluster[0], "c0");
    Client c1(cluster[1], "c1");
    Client c2(cluster[2], "c2");
    Client c3(cluster[3], "c3");

    {
        ScopedSuppressLogging allQuiet;

        c0.session.queueDeclare("q", durable=true);
        c0.session.messageTransfer(content=pMessage("a", "q"));

        // Cause partial failure on c1, c2
        c0.session.messageTransfer(content=pMessage("TEST_STORE_DO: s1[exception] s2[exception] testMultiPartialFailure", "q"));
        BOOST_CHECK_THROW(c1.session.queueQuery("q"), TransportFailure);
        BOOST_CHECK_THROW(c2.session.queueQuery("q"), TransportFailure);

        c0.session.messageTransfer(content=pMessage("b", "q"));
        c3.session.messageTransfer(content=pMessage("c", "q"));
        BOOST_CHECK_EQUAL(c3.session.queueQuery("q").getMessageCount(), 4u);
        // FIXME aconway 2009-06-30: This check fails sporadically with 2 != 3.
        // It should pass reliably.
        // BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c0.connection, 2).size());

        // Close inside ScopedSuppressLogging to avoid warnings 
        c0.close();
        c1.close();
        c2.close();
        c3.close();
    }
}

/** FIXME aconway 2009-04-10:
 * The current approach to shutting down a process in test_store
 * sometimes leads to assertion failures and errors in the shut-down
 * process. Need a cleaner solution
 */
#if 0
QPID_AUTO_TEST_CASE(testPartialFailureMemberLeaves) {
    ClusterFixture cluster(2, updateArgs, -1);
    Client c0(cluster[0], "c0");
    Client c1(cluster[1], "c1");

    {
        ScopedSuppressLogging allQuiet;

        c0.session.queueDeclare("q", durable=true);
        c0.session.messageTransfer(content=pMessage("a", "q"));

        // Cause failure on member 0 and simultaneous crash on member 1.
        BOOST_CHECK_THROW(
            c0.session.messageTransfer(
                content=pMessage("TEST_STORE_DO: s0[exception] s1[exit_process] testPartialFailureMemberLeaves", "q")),
                ConnectionException);
        cluster.wait(1);

        Client c00(cluster[0], "c00"); // Old connection is dead.
        BOOST_CHECK_EQUAL(c00.session.queueQuery("q").getMessageCount(), 1u);
        BOOST_CHECK_EQUAL(1u, knownBrokerPorts(c00.connection, 1).size());

        // Close inside ScopedSuppressLogging to avoid warnings 
        c0.close();
    }
}
#endif

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
