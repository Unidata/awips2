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
#include "qpid/cluster/InitialStatusMap.h"
#include "qpid/framing/Uuid.h"
#include <boost/assign.hpp>

using namespace std;
using namespace qpid::cluster;
using namespace qpid::framing;
using namespace qpid::framing::cluster;
using namespace boost::assign;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(InitialStatusMapTestSuite)

typedef InitialStatusMap::Status Status;

Status activeStatus(const Uuid& id=Uuid()) {
    return Status(ProtocolVersion(), 0, true, id, STORE_STATE_NO_STORE, Uuid());
}

Status newcomerStatus(const Uuid& id=Uuid()) {
    return Status(ProtocolVersion(), 0, false, id, STORE_STATE_NO_STORE, Uuid());
}

Status storeStatus(bool active, StoreState state, Uuid start=Uuid(), Uuid stop=Uuid()) {
    return Status(ProtocolVersion(), 0, active, start, state, stop);
}

QPID_AUTO_TEST_CASE(testFirstInCluster) {
    // Single member is first in cluster.
    InitialStatusMap map(MemberId(0), 1);
    Uuid id(true);
    BOOST_CHECK(!map.isComplete());
    MemberSet members = list_of(MemberId(0));
    map.configChange(members);
    BOOST_CHECK(!map.isComplete());
    map.received(MemberId(0), newcomerStatus(id));
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK(map.getElders().empty());
    BOOST_CHECK(!map.isUpdateNeeded());
    BOOST_CHECK_EQUAL(id, map.getClusterId());
}

QPID_AUTO_TEST_CASE(testJoinExistingCluster) {
    // Single member 0 joins existing cluster 1,2
    InitialStatusMap map(MemberId(0), 1);
    Uuid id(true);
    MemberSet members = list_of(MemberId(0))(MemberId(1))(MemberId(2));
    map.configChange(members);
    BOOST_CHECK(map.isResendNeeded());
    BOOST_CHECK(!map.isComplete());
    map.received(MemberId(0), newcomerStatus());
    map.received(MemberId(1), activeStatus(id));
    BOOST_CHECK(!map.isComplete());
    map.received(MemberId(2), activeStatus(id));
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK_EQUAL(map.getElders(), list_of<MemberId>(1)(2));
    BOOST_CHECK(map.isUpdateNeeded());
    BOOST_CHECK_EQUAL(map.getClusterId(), id);

    // Check that transitionToComplete is reset.
    map.configChange(list_of<MemberId>(0)(1));
    BOOST_CHECK(!map.transitionToComplete());
}

QPID_AUTO_TEST_CASE(testMultipleFirstInCluster) {
    // Multiple members 0,1,2 join at same time.
    InitialStatusMap map(MemberId(1), 1); // self is 1
    Uuid id(true);
    MemberSet members = list_of(MemberId(0))(MemberId(1))(MemberId(2));
    map.configChange(members);
    BOOST_CHECK(map.isResendNeeded());

    // All new members
    map.received(MemberId(0), newcomerStatus(id));
    map.received(MemberId(1), newcomerStatus());
    map.received(MemberId(2), newcomerStatus());
    BOOST_CHECK(!map.isResendNeeded());
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK_EQUAL(map.getElders(), list_of(MemberId(2)));
    BOOST_CHECK(!map.isUpdateNeeded());
    BOOST_CHECK_EQUAL(map.getClusterId(), id);
}

QPID_AUTO_TEST_CASE(testMultipleJoinExisting) {
    // Multiple members 1,2,3 join existing cluster containing 0.
    InitialStatusMap map(MemberId(2), 1); // self is 2
    Uuid id(true);
    MemberSet members = list_of(MemberId(0))(MemberId(1))(MemberId(2))(MemberId(3));
    map.configChange(members);
    BOOST_CHECK(map.isResendNeeded());

    map.received(MemberId(1), newcomerStatus());
    map.received(MemberId(2), newcomerStatus());
    map.received(MemberId(3), newcomerStatus());
    map.received(MemberId(0), activeStatus(id));
    BOOST_CHECK(!map.isResendNeeded());
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK_EQUAL(map.getElders(), list_of(MemberId(0))(MemberId(3)));
    BOOST_CHECK(map.isUpdateNeeded());
    BOOST_CHECK_EQUAL(map.getClusterId(), id);
}

QPID_AUTO_TEST_CASE(testMembersLeave) {
    // Test that map completes if members leave rather than send status.
    InitialStatusMap map(MemberId(0), 1);
    Uuid id(true);
    map.configChange(list_of(MemberId(0))(MemberId(1))(MemberId(2)));
    map.received(MemberId(0), newcomerStatus());
    map.received(MemberId(1), activeStatus(id));
    BOOST_CHECK(!map.isComplete());
    map.configChange(list_of(MemberId(0))(MemberId(1))); // 2 left
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK_EQUAL(map.getElders(), list_of(MemberId(1)));
    BOOST_CHECK_EQUAL(map.getClusterId(), id);
}

QPID_AUTO_TEST_CASE(testInteveningConfig) {
    // Multiple config changes arrives before we complete the map.
    InitialStatusMap map(MemberId(0), 1);
    Uuid id(true);

    map.configChange(list_of<MemberId>(0)(1));
    BOOST_CHECK(map.isResendNeeded());
    map.received(MemberId(0), newcomerStatus());
    BOOST_CHECK(!map.isComplete());
    BOOST_CHECK(!map.isResendNeeded());
    // New member 2 joins before we receive 1
    map.configChange(list_of<MemberId>(0)(1)(2));
    BOOST_CHECK(!map.isComplete());
    BOOST_CHECK(map.isResendNeeded());
    map.received(1, activeStatus(id));
    map.received(2, newcomerStatus());
    // We should not be complete as we haven't received 0 since new member joined
    BOOST_CHECK(!map.isComplete());
    BOOST_CHECK(!map.isResendNeeded());

    map.received(0, newcomerStatus());
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK_EQUAL(map.getElders(), list_of<MemberId>(1));
    BOOST_CHECK_EQUAL(map.getClusterId(), id);
}

QPID_AUTO_TEST_CASE(testInitialSize) {
    InitialStatusMap map(MemberId(0), 3);
    map.configChange(list_of<MemberId>(0)(1));
    map.received(MemberId(0), newcomerStatus());
    map.received(MemberId(1), newcomerStatus());
    BOOST_CHECK(!map.isComplete());

    map.configChange(list_of<MemberId>(0)(1)(2));
    map.received(MemberId(0), newcomerStatus());
    map.received(MemberId(1), newcomerStatus());
    map.received(MemberId(2), newcomerStatus());
    BOOST_CHECK(map.isComplete());
}

QPID_AUTO_TEST_CASE(testAllCleanNoUpdate) {
    InitialStatusMap map(MemberId(0), 3);
    map.configChange(list_of<MemberId>(0)(1)(2));
    map.received(MemberId(0), storeStatus(false, STORE_STATE_CLEAN_STORE));
    map.received(MemberId(1), storeStatus(false, STORE_STATE_CLEAN_STORE));
    map.received(MemberId(2), storeStatus(false, STORE_STATE_CLEAN_STORE));
    BOOST_CHECK(!map.isUpdateNeeded());
}

QPID_AUTO_TEST_CASE(testAllEmptyNoUpdate) {
    InitialStatusMap map(MemberId(0), 3);
    map.configChange(list_of<MemberId>(0)(1)(2));
    map.received(MemberId(0), storeStatus(false, STORE_STATE_EMPTY_STORE));
    map.received(MemberId(1), storeStatus(false, STORE_STATE_EMPTY_STORE));
    map.received(MemberId(2), storeStatus(false, STORE_STATE_EMPTY_STORE));
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(!map.isUpdateNeeded());
}

QPID_AUTO_TEST_CASE(testAllNoStoreNoUpdate) {
    InitialStatusMap map(MemberId(0), 3);
    map.configChange(list_of<MemberId>(0)(1)(2));
    map.received(MemberId(0), storeStatus(false, STORE_STATE_NO_STORE));
    map.received(MemberId(1), storeStatus(false, STORE_STATE_NO_STORE));
    map.received(MemberId(2), storeStatus(false, STORE_STATE_NO_STORE));
    BOOST_CHECK(map.isComplete());
    BOOST_CHECK(!map.isUpdateNeeded());
}

QPID_AUTO_TEST_CASE(testDirtyNeedUpdate) {
    InitialStatusMap map(MemberId(0), 3);
    map.configChange(list_of<MemberId>(0)(1)(2));
    map.received(MemberId(0), storeStatus(false, STORE_STATE_DIRTY_STORE));
    map.received(MemberId(1), storeStatus(false, STORE_STATE_CLEAN_STORE));
    map.received(MemberId(2), storeStatus(false, STORE_STATE_CLEAN_STORE));
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK(map.isUpdateNeeded());
}

QPID_AUTO_TEST_CASE(testEmptyNeedUpdate) {
    InitialStatusMap map(MemberId(0), 3);
    map.configChange(list_of<MemberId>(0)(1)(2));
    map.received(MemberId(0), storeStatus(false, STORE_STATE_EMPTY_STORE));
    map.received(MemberId(1), storeStatus(false, STORE_STATE_CLEAN_STORE));
    map.received(MemberId(2), storeStatus(false, STORE_STATE_CLEAN_STORE));
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK(map.isUpdateNeeded());
}

QPID_AUTO_TEST_CASE(testEmptyAlone) {
    InitialStatusMap map(MemberId(0), 1);
    map.configChange(list_of<MemberId>(0));
    map.received(MemberId(0), storeStatus(false, STORE_STATE_EMPTY_STORE));
    BOOST_CHECK(map.transitionToComplete());
    BOOST_CHECK(!map.isUpdateNeeded());
}

// FIXME aconway 2009-11-20: consistency tests for mixed stores,
// tests for manual intervention case.

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
