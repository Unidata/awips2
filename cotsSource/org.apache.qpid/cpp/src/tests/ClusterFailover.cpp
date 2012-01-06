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
#include "qpid/client/FailoverManager.h"
#include <boost/assign.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/bind.hpp>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ClusterFailoverTestSuite)

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


// Test re-connecting with same session name after a failure.
QPID_AUTO_TEST_CASE(testReconnectSameSessionName) {
    ostringstream clusterLib;
    clusterLib << getLibPath("CLUSTER_LIB");
    ClusterFixture::Args args = list_of<string>("--auth")("no")("--no-module-dir")("--no-data-dir")("--load-module")(clusterLib.str());
    ClusterFixture cluster(2, args, -1);
    Client c0(cluster[0], "foo");
    BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c0.connection, 2).size()); // wait for both.
    cluster.killWithSilencer(0, c0.connection, 9);
    Client c1(cluster[1], "foo"); // Using same name, should be cleaned up.
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
