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
#include "qpid/amqp_0_10/specification.h"
#include "qpid/amqp_0_10/ApplyControl.h"

QPID_AUTO_TEST_SUITE(VisitorTestSuite)

using  namespace qpid::amqp_0_10;

struct GetCode : public ApplyFunctor<uint8_t> {
    template <class T> uint8_t operator()(const T&) const { return T::CODE; }
};

struct SetChannelMax : ApplyFunctor<void> {
    template <class T> void operator()(T&) const { BOOST_FAIL(""); }
    void operator()(connection::Tune& t) const { t.channelMax=42; }
};
    
struct TestFunctor {
    typedef bool result_type;
    bool operator()(const connection::Tune& tune) {
        BOOST_CHECK_EQUAL(tune.channelMax, 1u);
        BOOST_CHECK_EQUAL(tune.maxFrameSize, 2u);
        BOOST_CHECK_EQUAL(tune.heartbeatMin, 3u);
        BOOST_CHECK_EQUAL(tune.heartbeatMax, 4u);
        return true;
    }
    template <class T>
    bool operator()(const T&) { return false; }
};

QPID_AUTO_TEST_CASE(testApply) {
    connection::Tune tune(1,2,3,4);
    Control* p = &tune;

    // boost oddity - without the cast we get undefined symbol errors.
    BOOST_CHECK_EQUAL(apply(GetCode(), *p), (uint8_t)connection::Tune::CODE); 
    
    TestFunctor tf; 
    BOOST_CHECK(apply(tf, *p));

    connection::Start start;
    p = &start;
    BOOST_CHECK(!apply(tf, *p));

    apply(SetChannelMax(), tune);
    BOOST_CHECK_EQUAL(tune.channelMax, 42);
}

struct VoidTestFunctor {
    typedef void result_type;

    int code;
    VoidTestFunctor() : code() {}
    
    void operator()(const connection::Tune& tune) {
        BOOST_CHECK_EQUAL(tune.channelMax, 1u);
        BOOST_CHECK_EQUAL(tune.maxFrameSize, 2u);
        BOOST_CHECK_EQUAL(tune.heartbeatMin, 3u);
        BOOST_CHECK_EQUAL(tune.heartbeatMax, 4u);
        code=connection::Tune::CODE;        
    }
    template <class T>
    void operator()(const T&) { code=0xFF; }
};

QPID_AUTO_TEST_CASE(testApplyVoid) {
    connection::Tune tune(1,2,3,4);
    Control* p = &tune;
    VoidTestFunctor tf;
    apply(tf, *p);
    BOOST_CHECK_EQUAL(uint8_t(connection::Tune::CODE), tf.code);

    connection::Start start;
    p = &start;
    apply(tf, *p);
    BOOST_CHECK_EQUAL(0xFF, tf.code);
}

QPID_AUTO_TEST_SUITE_END()
