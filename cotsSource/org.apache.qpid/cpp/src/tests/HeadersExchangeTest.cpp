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

#include "qpid/Exception.h"
#include "qpid/broker/HeadersExchange.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/FieldValue.h"
#include "unit_test.h"

using namespace qpid::broker;
using namespace qpid::framing;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(HeadersExchangeTestSuite)

QPID_AUTO_TEST_CASE(testMatchAll)
{
    FieldTable b, m, n;
    b.setString("x-match", "all");
    b.setString("foo", "FOO");
    b.setInt("n", 42);
    m.setString("foo", "FOO");
    m.setInt("n", 42);
    BOOST_CHECK(HeadersExchange::match(b, m));

    // Ignore extras.
    m.setString("extra", "x");
    BOOST_CHECK(HeadersExchange::match(b, m));

    // Fail mismatch, wrong value.
    m.setString("foo", "NotFoo");
    BOOST_CHECK(!HeadersExchange::match(b, m));

    // Fail mismatch, missing value
    n.setInt("n", 42);
    n.setString("extra", "x");
    BOOST_CHECK(!HeadersExchange::match(b, n));
}

QPID_AUTO_TEST_CASE(testMatchAny)
{
    FieldTable b, m, n;
    b.setString("x-match", "any");
    b.setString("foo", "FOO");
    b.setInt("n", 42);
    m.setString("foo", "FOO");
    BOOST_CHECK(!HeadersExchange::match(b, n));
    BOOST_CHECK(HeadersExchange::match(b, m));
    m.setInt("n", 42);
    BOOST_CHECK(HeadersExchange::match(b, m));
}

QPID_AUTO_TEST_CASE(testMatchEmptyValue)
{
    FieldTable b, m;
    b.setString("x-match", "all");
    b.set("foo", FieldTable::ValuePtr());
    b.set("n", FieldTable::ValuePtr());
    BOOST_CHECK(!HeadersExchange::match(b, m));
    m.setString("foo", "blah");
    m.setInt("n", 123);
}

QPID_AUTO_TEST_CASE(testMatchEmptyArgs)
{
    FieldTable b, m;
    m.setString("foo", "FOO");

    b.setString("x-match", "all");
    BOOST_CHECK(HeadersExchange::match(b, m));
    b.setString("x-match", "any");
    BOOST_CHECK(!HeadersExchange::match(b, m));
}


QPID_AUTO_TEST_CASE(testMatchNoXMatch)
{
    FieldTable b, m;
    b.setString("foo", "FOO");
    m.setString("foo", "FOO");
    BOOST_CHECK(!HeadersExchange::match(b, m));
}

QPID_AUTO_TEST_CASE(testBindNoXMatch)
{
    HeadersExchange exchange("test");
    Queue::shared_ptr queue;
    std::string key;
    FieldTable args;
    try {
        //just checking this doesn't cause assertion etc
        exchange.bind(queue, key, &args);
    } catch(qpid::Exception&) {
        //expected
    }
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
