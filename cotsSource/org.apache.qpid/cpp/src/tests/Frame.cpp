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
#include "qpid/framing/Frame.h"

#include <boost/lexical_cast.hpp>
#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(FrameTestSuite)

using namespace std;
using namespace qpid::framing;
using namespace boost;

QPID_AUTO_TEST_CASE(testContentBody) {
    Frame f(42, AMQContentBody("foobar"));
    AMQBody* body=f.getBody();
    BOOST_CHECK(dynamic_cast<AMQContentBody*>(body));
    Buffer b(f.encodedSize();
    f.encode(b);
    b.flip();
    Frame g;
    g.decode(b);
    AMQContentBody* content=dynamic_cast<AMQContentBody*>(g.getBody());
    BOOST_REQUIRE(content);
    BOOST_CHECK_EQUAL(content->getData(), "foobar");
}

QPID_AUTO_TEST_CASE(testMethodBody) {
    FieldTable args;
    args.setString("foo", "bar");
    Frame f(
        42, QueueDeclareBody(ProtocolVersion(), 1, "q", "altex",
                             true, false, true, false, true, args));
    BOOST_CHECK_EQUAL(f.getChannel(), 42);
    Buffer b(f.encodedSize();
    f.encode(b);
    b.flip();
    Frame g;
    g.decode(b);
    BOOST_CHECK_EQUAL(f.getChannel(), g.getChannel());
    QueueDeclareBody* declare=dynamic_cast<QueueDeclareBody*>(g.getBody());
    BOOST_REQUIRE(declare);
    BOOST_CHECK_EQUAL(declare->getAlternateExchange(), "altex");
    BOOST_CHECK_EQUAL(lexical_cast<string>(*f.getBody()), lexical_cast<string>(*g.getBody()));
}

QPID_AUTO_TEST_CASE(testLoop) {
    // Run in a loop so heap profiler can spot any allocations.
    Buffer b(1024);
    for (int i = 0; i < 100; ++i) {
        Frame ctor(2, AccessRequestOkBody(ProtocolVersion(), 42));
        Frame assign(3);
        assign.body = AccessRequestOkBody(ProtocolVersion(), 42);
        assign.encode(b);
        b.flip();
        Frame g;
        g.decode(b);
        BOOST_REQUIRE(dynamic_cast<AccessRequestOkBody*>(g.getBody())->getTicket() == 42);
    }
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
