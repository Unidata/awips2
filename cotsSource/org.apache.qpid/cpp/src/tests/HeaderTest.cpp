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
#include <iostream>
#include "qpid/framing/amqp_framing.h"
#include "qpid/framing/FieldValue.h"
#include "unit_test.h"

using namespace qpid::framing;
using namespace std;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(HeaderTestSuite)

QPID_AUTO_TEST_CASE(testGenericProperties)
{
    AMQHeaderBody body;
    body.get<MessageProperties>(true)->getApplicationHeaders().setString(
        "A", "BCDE");
    char buff[100];
    Buffer wbuffer(buff, 100);
    body.encode(wbuffer);

    Buffer rbuffer(buff, 100);
    AMQHeaderBody body2;
    body2.decode(rbuffer, body.encodedSize());
    MessageProperties* props =
        body2.get<MessageProperties>(true);
    BOOST_CHECK_EQUAL(
        string("BCDE"),
        props->getApplicationHeaders().get("A")->get<string>());
}

QPID_AUTO_TEST_CASE(testMessageProperties)
{
    AMQFrame out((AMQHeaderBody()));
    MessageProperties* props1 =
        out.castBody<AMQHeaderBody>()->get<MessageProperties>(true);

    props1->setContentLength(42);
    props1->setMessageId(Uuid(true));
    props1->setCorrelationId("correlationId");
    props1->setReplyTo(ReplyTo("ex","key"));
    props1->setContentType("contentType");
    props1->setContentEncoding("contentEncoding");
    props1->setUserId("userId");
    props1->setAppId("appId");

    char buff[10000];
    Buffer wbuffer(buff, 10000);
    out.encode(wbuffer);

    Buffer rbuffer(buff, 10000);
    AMQFrame in;
    in.decode(rbuffer);
    MessageProperties* props2 =
        in.castBody<AMQHeaderBody>()->get<MessageProperties>(true);

    BOOST_CHECK_EQUAL(props1->getContentLength(), props2->getContentLength());
    BOOST_CHECK_EQUAL(props1->getMessageId(), props2->getMessageId());
    BOOST_CHECK_EQUAL(props1->getCorrelationId(), props2->getCorrelationId());
    BOOST_CHECK_EQUAL(props1->getContentType(), props2->getContentType());
    BOOST_CHECK_EQUAL(props1->getContentEncoding(), props2->getContentEncoding());
    BOOST_CHECK_EQUAL(props1->getUserId(), props2->getUserId());
    BOOST_CHECK_EQUAL(props1->getAppId(), props2->getAppId());

}

QPID_AUTO_TEST_CASE(testDeliveryProperies)
{
    AMQFrame out((AMQHeaderBody()));
    DeliveryProperties* props1 =
        out.castBody<AMQHeaderBody>()->get<DeliveryProperties>(true);

    props1->setDiscardUnroutable(true);
    props1->setExchange("foo");

    char buff[10000];
    Buffer wbuffer(buff, 10000);
    out.encode(wbuffer);

    Buffer rbuffer(buff, 10000);
    AMQFrame in;
    in.decode(rbuffer);
    DeliveryProperties* props2 =
        in.castBody<AMQHeaderBody>()->get<DeliveryProperties>(true);

    BOOST_CHECK(props2->getDiscardUnroutable());
    BOOST_CHECK_EQUAL(string("foo"), props2->getExchange());
    BOOST_CHECK(!props2->hasTimestamp());
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
