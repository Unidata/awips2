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

#include "amqp_0_10/unit_test.h"
#include "qpid/amqp_0_10/ProxyTemplate.h"
#include <boost/any.hpp>

QPID_AUTO_TEST_SUITE(ProxyTemplateTestSuite)

using  namespace qpid::amqp_0_10;

struct ToAny {
    template <class T> 
    boost::any operator()(const T& t) { return boost::any(t); }
};

struct AnyProxy : public ProxyTemplate<ToAny, boost::any> {};

QPID_AUTO_TEST_CASE(testAnyProxy) {
    AnyProxy p;
    boost::any a=p.connectionTune(1,2,3,4);
    BOOST_CHECK_EQUAL(a.type().name(), typeid(connection::Tune).name());
    connection::Tune* tune=boost::any_cast<connection::Tune>(&a);
    BOOST_REQUIRE(tune);
    BOOST_CHECK_EQUAL(tune->channelMax, 1u);
    BOOST_CHECK_EQUAL(tune->maxFrameSize, 2u);
    BOOST_CHECK_EQUAL(tune->heartbeatMin, 3u);
    BOOST_CHECK_EQUAL(tune->heartbeatMax, 4u);
}

QPID_AUTO_TEST_SUITE_END()
