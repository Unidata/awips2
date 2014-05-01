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
#include "qpid/cluster/StoreStatus.h"
#include "qpid/framing/Uuid.h"
#include <boost/assign.hpp>
#include <boost/filesystem/operations.hpp>

using namespace std;
using namespace qpid::cluster;
using namespace qpid::framing;
using namespace qpid::framing::cluster;
using namespace boost::assign;
using namespace boost::filesystem;


namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(StoreStatusTestSuite)

const char* TEST_DIR = "StoreStatus.tmp";

QPID_AUTO_TEST_CASE(testLoadEmpty) {
    create_directory(TEST_DIR);
    StoreStatus ss(TEST_DIR);
    BOOST_CHECK_EQUAL(ss.getState(), STORE_STATE_NO_STORE);
    BOOST_CHECK(!ss.getClusterId());
    BOOST_CHECK(!ss.getShutdownId());
    ss.load();
    BOOST_CHECK_EQUAL(ss.getState(), STORE_STATE_EMPTY_STORE);
    BOOST_CHECK(!ss.getShutdownId());
    remove_all(TEST_DIR);
}

QPID_AUTO_TEST_CASE(testSaveLoadDirty) {
    create_directory(TEST_DIR);
    Uuid clusterId = Uuid(true);
    StoreStatus ss(TEST_DIR);
    ss.load();
    ss.dirty(clusterId);
    BOOST_CHECK_EQUAL(ss.getState(), STORE_STATE_DIRTY_STORE);

    StoreStatus ss2(TEST_DIR);
    ss2.load();
    BOOST_CHECK_EQUAL(ss2.getState(), STORE_STATE_DIRTY_STORE);
    BOOST_CHECK_EQUAL(ss2.getClusterId(), clusterId);
    BOOST_CHECK(!ss2.getShutdownId());
    remove_all(TEST_DIR);
}

QPID_AUTO_TEST_CASE(testSaveLoadClean) {
    create_directory(TEST_DIR);
    Uuid clusterId = Uuid(true);
    Uuid shutdownId = Uuid(true);
    StoreStatus ss(TEST_DIR);
    ss.load();
    ss.dirty(clusterId);
    ss.clean(shutdownId);
    BOOST_CHECK_EQUAL(ss.getState(), STORE_STATE_CLEAN_STORE);

    StoreStatus ss2(TEST_DIR);
    ss2.load();
    BOOST_CHECK_EQUAL(ss2.getState(), STORE_STATE_CLEAN_STORE);
    BOOST_CHECK_EQUAL(ss2.getClusterId(), clusterId);
    BOOST_CHECK_EQUAL(ss2.getShutdownId(), shutdownId);
    remove_all(TEST_DIR);
}

QPID_AUTO_TEST_CASE(testMarkDirty) {
    // Save clean then mark to dirty.
    create_directory(TEST_DIR);
    Uuid clusterId = Uuid(true);
    Uuid shutdownId = Uuid(true);
    StoreStatus ss(TEST_DIR);
    ss.load();
    ss.dirty(clusterId);
    ss.clean(shutdownId);
    ss.dirty(clusterId);
    
    StoreStatus ss2(TEST_DIR);
    ss2.load();
    BOOST_CHECK_EQUAL(ss2.getState(), STORE_STATE_DIRTY_STORE);
    BOOST_CHECK_EQUAL(ss2.getClusterId(), clusterId);
    BOOST_CHECK(!ss2.getShutdownId());
    remove_all(TEST_DIR);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
