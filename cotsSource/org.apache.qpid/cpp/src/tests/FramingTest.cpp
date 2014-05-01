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
#include "qpid/client/Connection.h"
#include "qpid/client/Connector.h"
#include "qpid/framing/AMQP_HighestVersion.h"
#include "qpid/framing/ProtocolVersion.h"
#include "qpid/framing/all_method_bodies.h"
#include "qpid/framing/amqp_framing.h"
#include "qpid/framing/reply_exceptions.h"
#include "unit_test.h"

#include <boost/bind.hpp>
#include <boost/lexical_cast.hpp>
#include <iostream>

#include <memory>
#include <sstream>
#include <typeinfo>

using namespace qpid;
using namespace qpid::framing;
using namespace std;

namespace qpid {
namespace tests {

template <class T>
std::string tostring(const T& x)
{
    std::ostringstream out;
    out << x;
    return out.str();
}

QPID_AUTO_TEST_SUITE(FramingTestSuite)

QPID_AUTO_TEST_CASE(testMessageTransferBody)
{
    char buffer[1024];
    ProtocolVersion version(highestProtocolVersion);
    Buffer wbuff(buffer, sizeof(buffer));
    MessageTransferBody in(version, "my-exchange", 1, 1);
    in.encode(wbuff);

    Buffer rbuff(buffer, sizeof(buffer));
    MessageTransferBody out(version);
    out.decode(rbuff);
    BOOST_CHECK_EQUAL(tostring(in), tostring(out));
}

QPID_AUTO_TEST_CASE(testConnectionSecureBody)
{
    char buffer[1024];
    ProtocolVersion version(highestProtocolVersion);
    Buffer wbuff(buffer, sizeof(buffer));
    std::string s = "security credential";
    ConnectionSecureBody in(version, s);
    in.encode(wbuff);

    Buffer rbuff(buffer, sizeof(buffer));
    ConnectionSecureBody out(version);
    out.decode(rbuff);
    BOOST_CHECK_EQUAL(tostring(in), tostring(out));
}

QPID_AUTO_TEST_CASE(testConnectionRedirectBody)
{
    char buffer[1024];
    ProtocolVersion version(highestProtocolVersion);
    Buffer wbuff(buffer, sizeof(buffer));
    std::string a = "hostA";
    std::string b = "hostB";
    Array hosts(0x95);
    hosts.add(boost::shared_ptr<FieldValue>(new Str16Value(a)));
    hosts.add(boost::shared_ptr<FieldValue>(new Str16Value(b)));

    ConnectionRedirectBody in(version, a, hosts);
    in.encode(wbuff);

    Buffer rbuff(buffer, sizeof(buffer));
    ConnectionRedirectBody out(version);
    out.decode(rbuff);
    BOOST_CHECK_EQUAL(tostring(in), tostring(out));
}

QPID_AUTO_TEST_CASE(testQueueDeclareBody)
{
    char buffer[1024];
    ProtocolVersion version(highestProtocolVersion);
    Buffer wbuff(buffer, sizeof(buffer));
    QueueDeclareBody in(version, "name", "dlq", true, false, true, false, FieldTable());
    in.encode(wbuff);

    Buffer rbuff(buffer, sizeof(buffer));
    QueueDeclareBody out(version);
    out.decode(rbuff);
    BOOST_CHECK_EQUAL(tostring(in), tostring(out));
}

QPID_AUTO_TEST_CASE(testConnectionRedirectBodyFrame)
{
    char buffer[1024];
    ProtocolVersion version(highestProtocolVersion);
    Buffer wbuff(buffer, sizeof(buffer));
    std::string a = "hostA";
    std::string b = "hostB";
    Array hosts(0x95);
    hosts.add(boost::shared_ptr<FieldValue>(new Str16Value(a)));
    hosts.add(boost::shared_ptr<FieldValue>(new Str16Value(b)));

    AMQFrame in((ConnectionRedirectBody(version, a, hosts)));
    in.setChannel(999);
    in.encode(wbuff);

    Buffer rbuff(buffer, sizeof(buffer));
    AMQFrame out;
    out.decode(rbuff);
    BOOST_CHECK_EQUAL(tostring(in), tostring(out));
}

QPID_AUTO_TEST_CASE(testMessageCancelBodyFrame)
{
    char buffer[1024];
    ProtocolVersion version(highestProtocolVersion);
    Buffer wbuff(buffer, sizeof(buffer));
    AMQFrame in((MessageCancelBody(version, "tag")));
    in.setChannel(999);
    in.encode(wbuff);

    Buffer rbuff(buffer, sizeof(buffer));
    AMQFrame out;
    out.decode(rbuff);
    BOOST_CHECK_EQUAL(tostring(in), tostring(out));
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
