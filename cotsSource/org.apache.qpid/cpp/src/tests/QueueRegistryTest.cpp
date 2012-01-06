/*
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
 */

#include "qpid/broker/QueueRegistry.h"
#include "unit_test.h"
#include <string>

using namespace qpid::broker;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(QueueRegistryTest)

QPID_AUTO_TEST_CASE(testDeclare)
{
    std::string foo("foo");
    std::string bar("bar");
    QueueRegistry reg;
    std::pair<Queue::shared_ptr,  bool> qc;

    qc = reg.declare(foo, false, 0, 0);
    Queue::shared_ptr q = qc.first;
    BOOST_CHECK(q);
    BOOST_CHECK(qc.second); // New queue
    BOOST_CHECK_EQUAL(foo, q->getName());

    qc = reg.declare(foo, false, 0, 0);
    BOOST_CHECK_EQUAL(q, qc.first);
    BOOST_CHECK(!qc.second);

    qc = reg.declare(bar, false, 0, 0);
    q = qc.first;
    BOOST_CHECK(q);
    BOOST_CHECK_EQUAL(true, qc.second);
    BOOST_CHECK_EQUAL(bar, q->getName());
}

QPID_AUTO_TEST_CASE(testDeclareTmp)
{
    QueueRegistry reg;
    std::pair<Queue::shared_ptr,  bool> qc;

    qc = reg.declare(std::string(), false, 0, 0);
    BOOST_CHECK(qc.second);
    BOOST_CHECK_EQUAL(std::string("tmp_1"), qc.first->getName());
}

QPID_AUTO_TEST_CASE(testFind)
{
    std::string foo("foo");
    std::string bar("bar");
    QueueRegistry reg;
    std::pair<Queue::shared_ptr,  bool> qc;

    BOOST_CHECK(reg.find(foo) == 0);

    reg.declare(foo, false, 0, 0);
    reg.declare(bar, false, 0, 0);
    Queue::shared_ptr q = reg.find(bar);
    BOOST_CHECK(q);
    BOOST_CHECK_EQUAL(bar, q->getName());
}

QPID_AUTO_TEST_CASE(testDestroy)
{
    std::string foo("foo");
    QueueRegistry reg;
    std::pair<Queue::shared_ptr,  bool> qc;

    qc = reg.declare(foo, false, 0, 0);
    reg.destroy(foo);
    // Queue is gone from the registry.
    BOOST_CHECK(reg.find(foo) == 0);
    // Queue is not actually destroyed till we drop our reference.
    BOOST_CHECK_EQUAL(foo, qc.first->getName());
    // We shoud be the only reference.
    BOOST_CHECK_EQUAL(1L, qc.first.use_count());
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
