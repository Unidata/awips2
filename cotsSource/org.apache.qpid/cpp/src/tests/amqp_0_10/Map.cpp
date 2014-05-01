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
#include "qpid/amqp_0_10/Map.h"
#include "qpid/amqp_0_10/Array.h"
#include "qpid/amqp_0_10/Struct32.h"
#include "qpid/amqp_0_10/UnknownType.h"
#include "qpid/amqp_0_10/Codec.h"
#include <iostream>

using namespace qpid::amqp_0_10;
using namespace std;

QPID_AUTO_TEST_SUITE(MapTestSuite)

 QPID_AUTO_TEST_CASE(testGetSet) {
    MapValue v;
    v = Str8("foo");
    BOOST_CHECK(v.get<Str8>());
    BOOST_CHECK(!v.get<uint8_t>());
    BOOST_CHECK_EQUAL(*v.get<Str8>(), "foo");
    
    v = uint8_t(42);
    BOOST_CHECK(!v.get<Str8>());
    BOOST_CHECK(v.get<uint8_t>());
    BOOST_CHECK_EQUAL(*v.get<uint8_t>(), 42);

    v = uint16_t(12);
    BOOST_CHECK(v.get<uint16_t>());
    BOOST_CHECK_EQUAL(*v.get<uint16_t>(), 12);
}

template <class R> struct TestVisitor : public MapValue::Visitor<R> {
    template <class T> R operator()(const T&) const { throw MapValue::BadTypeException(); }
    R operator()(const R& r) const { return r; }
};
    
QPID_AUTO_TEST_CASE(testVisit) {
    MapValue v;
    v = Str8("foo");
    BOOST_CHECK_EQUAL(v.apply_visitor(TestVisitor<Str8>()), "foo");
    v = Uint16(42);
    BOOST_CHECK_EQUAL(v.apply_visitor(TestVisitor<Uint16>()), 42);
    try {
        v.apply_visitor(TestVisitor<bool>());
        BOOST_FAIL("Expecting exception");
    }
    catch(const MapValue::BadTypeException&) {}
}


QPID_AUTO_TEST_CASE(testEncodeMapValue) {
    MapValue mv;
    std::string data;
    mv = Str8("hello");
    Codec::encode(back_inserter(data))(mv);
    BOOST_CHECK_EQUAL(data.size(), Codec::size(mv));
    MapValue mv2;
    Codec::decode(data.begin())(mv2);
    BOOST_CHECK_EQUAL(mv2.getCode(), 0x85);
    BOOST_REQUIRE(mv2.get<Str8>());
    BOOST_CHECK_EQUAL(*mv2.get<Str8>(), "hello");
}

QPID_AUTO_TEST_CASE(testEncode) {
    Map map;
    std::string data;
    map["A"] = true;
    map["b"] = Str8("hello");
    Codec::encode(back_inserter(data))(map);
    BOOST_CHECK_EQUAL(Codec::size(map), data.size());
    Map map2;
    Codec::decode(data.begin())(map2);
    BOOST_CHECK_EQUAL(map.size(), 2u);
    BOOST_CHECK(map["A"].get<bool>());
    BOOST_CHECK_EQUAL(*map["b"].get<Str8>(), "hello");
}


QPID_AUTO_TEST_SUITE_END()
