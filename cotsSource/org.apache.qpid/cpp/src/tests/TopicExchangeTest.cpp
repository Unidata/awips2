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
#include "qpid/broker/TopicExchange.h"
#include "unit_test.h"
#include "test_tools.h"

using namespace qpid::broker;
using namespace std;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(TopicExchangeTestSuite)

#define CHECK_NORMALIZED(expect, pattern) BOOST_CHECK_EQUAL(expect, TopicExchange::normalize(pattern));

QPID_AUTO_TEST_CASE(testNormalize)
{
    CHECK_NORMALIZED("", "");
    CHECK_NORMALIZED("a.b.c", "a.b.c");
    CHECK_NORMALIZED("a.*.c", "a.*.c");
    CHECK_NORMALIZED("#", "#");
    CHECK_NORMALIZED("#", "#.#.#.#");
    CHECK_NORMALIZED("*.*.*.#", "#.*.#.*.#.#.*");
    CHECK_NORMALIZED("a.*.*.*.#", "a.*.#.*.#.*.#");
    CHECK_NORMALIZED("a.*.*.*.#", "a.*.#.*.#.*");
    CHECK_NORMALIZED("*.*.*.#", "*.#.#.*.*.#");
}

QPID_AUTO_TEST_CASE(testPlain)
{
    string pattern("ab.cd.e");
    BOOST_CHECK(TopicExchange::match(pattern, "ab.cd.e"));
    BOOST_CHECK(!TopicExchange::match(pattern, "abx.cd.e"));
    BOOST_CHECK(!TopicExchange::match(pattern, "ab.cd"));
    BOOST_CHECK(!TopicExchange::match(pattern, "ab.cd..e."));
    BOOST_CHECK(!TopicExchange::match(pattern, "ab.cd.e."));
    BOOST_CHECK(!TopicExchange::match(pattern, ".ab.cd.e"));

    pattern = "";
    BOOST_CHECK(TopicExchange::match(pattern, ""));

    pattern = ".";
    BOOST_CHECK(TopicExchange::match(pattern, "."));
}


QPID_AUTO_TEST_CASE(testStar)
{
    string pattern("a.*.b");
    BOOST_CHECK(TopicExchange::match(pattern, "a.xx.b"));
    BOOST_CHECK(!TopicExchange::match(pattern, "a.b"));

    pattern = "*.x";
    BOOST_CHECK(TopicExchange::match(pattern, "y.x"));
    BOOST_CHECK(TopicExchange::match(pattern, ".x"));
    BOOST_CHECK(!TopicExchange::match(pattern, "x"));

    pattern = "x.x.*";
    BOOST_CHECK(TopicExchange::match(pattern, "x.x.y"));
    BOOST_CHECK(TopicExchange::match(pattern, "x.x."));
    BOOST_CHECK(!TopicExchange::match(pattern, "x.x"));
    BOOST_CHECK(!TopicExchange::match(pattern, "q.x.y"));
}

QPID_AUTO_TEST_CASE(testHash)
{
    string pattern("a.#.b");
    BOOST_CHECK(TopicExchange::match(pattern, "a.b"));
    BOOST_CHECK(TopicExchange::match(pattern, "a.x.b"));
    BOOST_CHECK(TopicExchange::match(pattern, "a..x.y.zz.b"));
    BOOST_CHECK(!TopicExchange::match(pattern, "a.b."));
    BOOST_CHECK(!TopicExchange::match(pattern, "q.x.b"));

    pattern = "a.#";
    BOOST_CHECK(TopicExchange::match(pattern, "a"));
    BOOST_CHECK(TopicExchange::match(pattern, "a.b"));
    BOOST_CHECK(TopicExchange::match(pattern, "a.b.c"));

    pattern = "#.a";
    BOOST_CHECK(TopicExchange::match(pattern, "a"));
    BOOST_CHECK(TopicExchange::match(pattern, "x.y.a"));

    pattern = "a.#.b.#.c";
    BOOST_CHECK(TopicExchange::match(pattern, "a.b.c"));
    BOOST_CHECK(TopicExchange::match(pattern, "a.x.b.y.c"));
    BOOST_CHECK(TopicExchange::match(pattern, "a.x.x.b.y.y.c"));
}

QPID_AUTO_TEST_CASE(testMixed)
{
    string pattern("*.x.#.y");
    BOOST_CHECK(TopicExchange::match(pattern, "a.x.y"));
    BOOST_CHECK(TopicExchange::match(pattern, "a.x.p.qq.y"));
    BOOST_CHECK(!TopicExchange::match(pattern, "a.a.x.y"));
    BOOST_CHECK(!TopicExchange::match(pattern, "aa.x.b.c"));

    pattern = "a.#.b.*";
    BOOST_CHECK(TopicExchange::match(pattern, "a.b.x"));
    BOOST_CHECK(TopicExchange::match(pattern, "a.x.x.x.b.x"));

    pattern = "*.*.*.#";
    BOOST_CHECK(TopicExchange::match(pattern, "x.y.z"));
    BOOST_CHECK(TopicExchange::match(pattern, "x.y.z.a.b.c"));
    BOOST_CHECK(!TopicExchange::match(pattern, "x.y"));
    BOOST_CHECK(!TopicExchange::match(pattern, "x"));
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
