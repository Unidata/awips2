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

#include "qpid/management/ManagementObject.h"
#include "qpid/framing/Buffer.h"
#include "qpid/console/ObjectId.h"
#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ManagementTestSuite)

using namespace qpid::framing;
using namespace qpid::management;

QPID_AUTO_TEST_CASE(testObjectIdSerializeStream) {
    std::string text("0-10-4-2500-80000000000");
    std::stringstream input(text);

    ObjectId oid(input);

    std::stringstream output;
    output << oid;

    BOOST_CHECK_EQUAL(text, output.str());
}

QPID_AUTO_TEST_CASE(testObjectIdSerializeString) {
    std::string text("0-10-4-2500-80000000000");

    ObjectId oid(text);

    std::stringstream output;
    output << oid;

    BOOST_CHECK_EQUAL(text, output.str());
}

QPID_AUTO_TEST_CASE(testObjectIdEncode) {
    char buffer[100];
    Buffer msgBuf(buffer, 100);
    msgBuf.putLongLong(0x1002000030000004LL);
    msgBuf.putLongLong(0x0000000000000005LL);
    msgBuf.reset();

    ObjectId oid(msgBuf);

    std::stringstream out1;
    out1 << oid;

    BOOST_CHECK_EQUAL(out1.str(), "1-2-3-4-5");
}

QPID_AUTO_TEST_CASE(testObjectIdAttach) {
    AgentAttachment   agent;
    ObjectId          oid(&agent, 10, 20, 50);

    std::stringstream out1;
    out1 << oid;
    BOOST_CHECK_EQUAL(out1.str(), "10-20-0-0-50");

    agent.setBanks(30, 40);
    std::stringstream out2;
    out2 << oid;
    BOOST_CHECK_EQUAL(out2.str(), "10-20-30-40-50");
}

QPID_AUTO_TEST_CASE(testConsoleObjectId) {
    qpid::console::ObjectId oid1, oid2;

    oid1.setValue(1, 2);
    oid2.setValue(3, 4);

    BOOST_CHECK(oid1 < oid2);
    BOOST_CHECK(oid1 <= oid2);
    BOOST_CHECK(oid2 > oid1);
    BOOST_CHECK(oid2 >= oid1);
    BOOST_CHECK(oid1 != oid2);
    BOOST_CHECK(oid1 == oid1);

    oid1.setValue(3, 6);
    oid2.setValue(3, 4);

    BOOST_CHECK(oid1 > oid2);
    BOOST_CHECK(oid1 >= oid2);
    BOOST_CHECK(oid2 < oid1);
    BOOST_CHECK(oid2 <= oid1);
    BOOST_CHECK(oid1 != oid2);

    oid2.setValue(3, 6);
    BOOST_CHECK(oid1 == oid2);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
