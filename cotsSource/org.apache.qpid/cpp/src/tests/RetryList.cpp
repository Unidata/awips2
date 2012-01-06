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
#include "qpid/broker/RetryList.h"

using namespace qpid;
using namespace qpid::broker;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(RetryListTestSuite)

struct RetryListFixture
{
    RetryList list;
    std::vector<Url> urls;
    std::vector<TcpAddress> expected;

    void addUrl(const std::string& s)
    {
        urls.push_back(Url(s));
    }

    void addExpectation(const std::string& host, uint16_t port)
    {
        expected.push_back(TcpAddress(host, port));
    }

    void check()
    {
        list.reset(urls);
        for (int t = 0; t < 2; t++) {
            TcpAddress next;
            for (std::vector<TcpAddress>::const_iterator i = expected.begin(); i != expected.end(); ++i) {
                BOOST_CHECK(list.next(next));
                BOOST_CHECK_EQUAL(i->host, next.host);
                BOOST_CHECK_EQUAL(i->port, next.port);
            }
            BOOST_CHECK(!list.next(next));
        }
    }
};

QPID_AUTO_TEST_CASE(testWithSingleAddress)
{
    RetryListFixture test;
    test.addUrl("amqp:host:5673");
    test.addExpectation("host", 5673);
    test.check();
}

QPID_AUTO_TEST_CASE(testWithSingleUrlOfMultipleAddresses)
{
    RetryListFixture test;
    test.addUrl("amqp:host1,host2:2222,tcp:host3:5673,host4:1");

    test.addExpectation("host1", 5672);
    test.addExpectation("host2", 2222);
    test.addExpectation("host3", 5673);
    test.addExpectation("host4", 1);

    test.check();
}

QPID_AUTO_TEST_CASE(testWithMultipleUrlsOfMultipleAddresses)
{
    RetryListFixture test;
    test.addUrl("amqp:my-host");
    test.addUrl("amqp:host1:6666,host2:2222,tcp:host3:5673,host4:1");
    test.addUrl("amqp:host5,host6:2222,tcp:host7:5673");

    test.addExpectation("my-host", 5672);
    test.addExpectation("host1", 6666);
    test.addExpectation("host2", 2222);
    test.addExpectation("host3", 5673);
    test.addExpectation("host4", 1);
    test.addExpectation("host5", 5672);
    test.addExpectation("host6", 2222);
    test.addExpectation("host7", 5673);

    test.check();
}

QPID_AUTO_TEST_CASE(testEmptyList)
{
    RetryListFixture test;
    test.check();
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
