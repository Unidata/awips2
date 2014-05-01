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
#include "amqp_0_10/allSegmentTypes.h"

#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/Buffer.h"

#include "qpid/amqp_0_10/Packer.h"
#include "qpid/amqp_0_10/built_in_types.h"
#include "qpid/amqp_0_10/Codec.h"
#include "qpid/amqp_0_10/specification.h"
#include "qpid/amqp_0_10/ControlHolder.h"
#include "qpid/amqp_0_10/Struct32.h"
#include "qpid/amqp_0_10/FrameHeader.h"
#include "qpid/amqp_0_10/Map.h"
#include "qpid/amqp_0_10/Unit.h"
#include "allSegmentTypes.h"

#include <boost/test/test_case_template.hpp>
#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/optional.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/back_inserter.hpp>
#include <boost/mpl/copy.hpp>
#include <boost/mpl/empty_sequence.hpp>
#include <boost/current_function.hpp>
#include <iterator>
#include <string>
#include <sstream>
#include <iostream>
#include <netinet/in.h>

// Missing operators needed for tests.
namespace boost {
template <class T, size_t N>
std::ostream& operator<<(std::ostream& out, const array<T,N>& a) {
    std::ostream_iterator<T> o(out, " ");
    std::copy(a.begin(), a.end(), o);
    return out;
}
} // boost

QPID_AUTO_TEST_SUITE(SerializeTestSuite)

using namespace std;
namespace mpl=boost::mpl;
using namespace qpid::amqp_0_10;
using qpid::framing::in_place;

template <class A, class B> struct concat2 { typedef typename mpl::copy<B, typename mpl::back_inserter<A> >::type type; };
template <class A, class B, class C> struct concat3 { typedef typename concat2<A, typename concat2<B, C>::type>::type type; };
template <class A, class B, class C, class D> struct concat4 { typedef typename concat2<A, typename concat3<B, C, D>::type>::type type; };

typedef mpl::vector<Boolean, Char, Int32, Int64, Int8, Uint16, CharUtf32, Uint32, Uint64, Bin8, Uint8>::type IntegralTypes;
typedef mpl::vector<Bin1024, Bin128, Bin16, Bin256, Bin32, Bin40, Bin512, Bin64, Bin72>::type BinTypes;
typedef mpl::vector<Double, Float>::type FloatTypes;
typedef mpl::vector<SequenceNo, Uuid, Datetime, Dec32, Dec64> FixedSizeClassTypes;
typedef mpl::vector<Map, Vbin8, Str8Latin, Str8, Str8Utf16, Vbin16, Str16Latin, Str16, Str16Utf16, Vbin32> VariableSizeTypes;

typedef concat4<IntegralTypes, BinTypes, FloatTypes, FixedSizeClassTypes>::type FixedSizeTypes;
typedef concat2<FixedSizeTypes, VariableSizeTypes>::type AllTypes;

// TODO aconway 2008-02-20: should test 64 bit integrals for order also.
QPID_AUTO_TEST_CASE(testNetworkByteOrder) {
    string data;

    uint32_t l = 0x11223344;
    Codec::encode(std::back_inserter(data))(l);
    uint32_t enc=reinterpret_cast<const uint32_t&>(*data.data());
    uint32_t l2 = ntohl(enc);
    BOOST_CHECK_EQUAL(l, l2);

    data.clear();
    uint16_t s = 0x1122;
    Codec::encode(std::back_inserter(data))(s);
    uint32_t s2 = ntohs(*reinterpret_cast<const uint32_t*>(data.data()));
    BOOST_CHECK_EQUAL(s, s2);
}

QPID_AUTO_TEST_CASE(testSetLimit) {
    typedef Codec::Encoder<back_insert_iterator<string> > Encoder;
    string data;
    Encoder encode(back_inserter(data), 3);
    encode('1')('2')('3');
    try {
        encode('4');
        BOOST_FAIL("Expected exception");
    } catch (...) {}            // FIXME aconway 2008-04-03: catch proper exception
    BOOST_CHECK_EQUAL(data, "123");
}

QPID_AUTO_TEST_CASE(testScopedLimit) {
    typedef Codec::Encoder<back_insert_iterator<string> > Encoder;
    string data;
    Encoder encode(back_inserter(data), 10);
    encode(Str8("123"));        // 4 bytes
    {
        Encoder::ScopedLimit l(encode, 3);
        encode('a')('b')('c');
        try {
            encode('d');
            BOOST_FAIL("Expected exception");
        } catch(...) {}         // FIXME aconway 2008-04-03: catch proper exception
    }
    BOOST_CHECK_EQUAL(data, "\003123abc");
    encode('x')('y')('z');
    try {
        encode('!');
        BOOST_FAIL("Expected exception");
    } catch(...) {}         // FIXME aconway 2008-04-03: catch proper exception
    BOOST_CHECK_EQUAL(data.size(), 10u);
}

// Assign test values to the various types.
void testValue(bool& b) { b = true; }
void testValue(Bit&) { }
template <class T> typename boost::enable_if<boost::is_arithmetic<T> >::type testValue(T& n) { n=42; }
void testValue(CharUtf32& c) { c = 43; }
void testValue(long long& l) { l = 0x012345; }
void testValue(Datetime& dt) { dt = qpid::sys::now(); }
void testValue(Uuid& uuid) { uuid=Uuid(true); }
template <class E, class M> void testValue(Decimal<E,M>& d) { d.exponent=2; d.mantissa=0x1122; }
void testValue(SequenceNo& s) { s = 42; }
template <size_t N> void testValue(Bin<N>& a) { a.assign(42); }
template <class T, class S, int Unique> void testValue(SerializableString<T, S, Unique>& s) {
    char msg[]="foobar";
    s.assign(msg, msg+sizeof(msg));
}
void testValue(Str16& s) { s = "the quick brown fox jumped over the lazy dog"; }
void testValue(Str8& s) { s = "foobar"; }
void testValue(Map& m) { m["s"] = Str8("foobar"); m["b"] = true; m["c"] = uint16_t(42); }

//typedef mpl::vector<Str8, Str16>::type TestTypes;
/*BOOST_AUTO_TEST_CASE_TEMPLATE(testEncodeDecode, T, AllTypes)
{
    string data;
    T t;
    testValue(t);
    Codec::encode(std::back_inserter(data))(t);

    BOOST_CHECK_EQUAL(Codec::size(t), data.size());

    T t2;
    Codec::decode(data.begin())(t2);
    BOOST_CHECK_EQUAL(t,t2);
}
*/

struct TestMe {
    bool encoded, decoded;
    char value;
    TestMe(char v) : encoded(), decoded(), value(v) {}
    template <class S> void encode(S& s) const {
        const_cast<TestMe*>(this)->encoded=true; s(value);
    }
    template <class S> void decode(S& s) { decoded=true; s(value); }
    template <class S> void serialize(S& s) { s.split(*this); }
};

QPID_AUTO_TEST_CASE(testSplit) {
    string data;
    TestMe t1('x');
    Codec::encode(std::back_inserter(data))(t1);
    BOOST_CHECK(t1.encoded);
    BOOST_CHECK(!t1.decoded);
    BOOST_CHECK_EQUAL(data, "x");

    TestMe t2('y');
    Codec::decode(data.begin())(t2);
    BOOST_CHECK(!t2.encoded);
    BOOST_CHECK(t2.decoded);
    BOOST_CHECK_EQUAL(t2.value, 'x');
}

QPID_AUTO_TEST_CASE(testControlEncodeDecode) {
    string data;
    Control::Holder h(in_place<connection::Tune>(1,2,3,4));
    Codec::encode(std::back_inserter(data))(h);
    
    BOOST_CHECK_EQUAL(data.size(), Codec::size(h));

    Codec::Decoder<string::iterator> decode(data.begin());
    Control::Holder h2;
    decode(h2);

    BOOST_REQUIRE(h2.get());
    BOOST_CHECK_EQUAL(h2.get()->getClassCode(), connection::CODE);
    BOOST_CHECK_EQUAL(h2.get()->getCode(), uint8_t(connection::Tune::CODE));
    connection::Tune& tune=static_cast<connection::Tune&>(*h2.get());
    BOOST_CHECK_EQUAL(tune.channelMax, 1u);
    BOOST_CHECK_EQUAL(tune.maxFrameSize, 2u);
    BOOST_CHECK_EQUAL(tune.heartbeatMin, 3u);
    BOOST_CHECK_EQUAL(tune.heartbeatMax, 4u);
}

QPID_AUTO_TEST_CASE(testStruct32) {
    message::DeliveryProperties dp;
    dp.priority=message::MEDIUM;
    dp.routingKey="foo";
    Struct32 s(dp);
    string data;
    Codec::encode(back_inserter(data))(s);

    uint32_t structSize;        // Starts with size
    Codec::decode(data.begin())(structSize);
    BOOST_CHECK_EQUAL(structSize, Codec::size(dp) + 2);  // +2 for code
    BOOST_CHECK_EQUAL(structSize, data.size()-4);        // encoded body
    
    BOOST_CHECK_EQUAL(data.size(), Codec::size(s));
    Struct32 s2;
    Codec::decode(data.begin())(s2);
    message::DeliveryProperties* dp2 = s2.getIf<message::DeliveryProperties>();
    BOOST_REQUIRE(dp2);
    BOOST_CHECK_EQUAL(dp2->priority, message::MEDIUM);
    BOOST_CHECK_EQUAL(dp2->routingKey, "foo");
}

QPID_AUTO_TEST_CASE(testStruct32Unknown) {
    // Verify we can recode an unknown struct unchanged.
    Struct32 s;
    string data;
    Codec::encode(back_inserter(data))(uint32_t(10));
    data.append(10, 'X');
    Codec::decode(data.begin())(s);
    string data2;
    Codec::encode(back_inserter(data2))(s);
    BOOST_CHECK_EQUAL(data.size(), data2.size());
    BOOST_CHECK_EQUAL(data, data2);
}

struct DummyPacked {
    static const uint8_t PACK=1;
    boost::optional<char> i, j;
    char k;
    Bit l,m;
    DummyPacked(char a=0, char b=0, char c=0) : i(a), j(b), k(c), l(), m() {}
    template <class S> void serialize(S& s) { s(i)(j)(k)(l)(m); }
};

Packer<DummyPacked> serializable(DummyPacked& d) { return Packer<DummyPacked>(d); }

QPID_AUTO_TEST_CASE(testPackBits) {
    DummyPacked d('a','b','c');
    BOOST_CHECK_EQUAL(packBits(d), 7u);
    d.j = boost::none;
    BOOST_CHECK_EQUAL(packBits(d), 5u);
    d.m = true;
    BOOST_CHECK_EQUAL(packBits(d), 0x15u);
}


QPID_AUTO_TEST_CASE(testPacked) {
    string data;

    Codec::encode(back_inserter(data))('a')(boost::optional<char>('b'))(boost::optional<char>())('c');
    BOOST_CHECK_EQUAL(data, "abc");
    data.clear();
    
    DummyPacked dummy('a','b','c');

    Codec::encode(back_inserter(data))(dummy);
    BOOST_CHECK_EQUAL(data.size(), 4u);
    BOOST_CHECK_EQUAL(data, string("\007abc"));
    data.clear();

    dummy.i = boost::none;
    Codec::encode(back_inserter(data))(dummy);
    BOOST_CHECK_EQUAL(data, string("\6bc"));
    data.clear();

    const char* missing = "\5xy";
    Codec::decode(missing)(dummy);
    BOOST_CHECK(dummy.i);
    BOOST_CHECK_EQUAL(*dummy.i, 'x');
    BOOST_CHECK(!dummy.j);
    BOOST_CHECK_EQUAL(dummy.k, 'y');
}

QPID_AUTO_TEST_CASE(testUnitControl) {
    string data;
    Control::Holder h(in_place<connection::Tune>(1,2,3,4));
    Codec::encode(std::back_inserter(data))(h);

    Unit unit(FrameHeader(FIRST_FRAME|LAST_FRAME, CONTROL));
    Codec::decode(data.begin())(unit);

    BOOST_REQUIRE(unit.get<ControlHolder>());

    string data2;
    Codec::encode(back_inserter(data2))(unit);
    
    BOOST_CHECK_EQUAL(data, data2);
}

QPID_AUTO_TEST_CASE(testArray) {
    ArrayDomain<char> a;
    a.resize(3, 'x');
    string data;
    Codec::encode(back_inserter(data))(a);

    ArrayDomain<char> b;
    Codec::decode(data.begin())(b);
    BOOST_CHECK_EQUAL(b.size(), 3u);
    string data3;
    Codec::encode(back_inserter(data3))(a);
    BOOST_CHECK_EQUAL(data, data3);
    
    Array x;
    Codec::decode(data.begin())(x);
    BOOST_CHECK_EQUAL(x.size(), 3u);
    BOOST_CHECK_EQUAL(x[0].size(), 1u);
    BOOST_CHECK_EQUAL(*x[0].begin(), 'x');
    BOOST_CHECK_EQUAL(*x[2].begin(), 'x');

    string data2;
    Codec::encode(back_inserter(data2))(x);
    BOOST_CHECK_EQUAL(data,data2);
}

QPID_AUTO_TEST_CASE(testStruct) {
    string data;

    message::DeliveryProperties dp;
    BOOST_CHECK(!dp.discardUnroutable);
    dp.immediate = true;
    dp.redelivered = false;
    dp.priority = message::MEDIUM;
    dp.exchange = "foo";

    Codec::encode(back_inserter(data))(dp);
    // Skip 4 bytes size, little-endian decode for pack bits.
    uint16_t encodedBits=uint8_t(data[5]);
    encodedBits <<= 8;
    encodedBits += uint8_t(data[4]);
    BOOST_CHECK_EQUAL(encodedBits, packBits(dp));
        
    data.clear();
    Struct32 h(dp);
    Codec::encode(back_inserter(data))(h);    

    Struct32 h2;
    Codec::decode(data.begin())(h2);
    BOOST_CHECK_EQUAL(h2.getClassCode(), Uint8(message::DeliveryProperties::CLASS_CODE));
    BOOST_CHECK_EQUAL(h2.getCode(), Uint8(message::DeliveryProperties::CODE));
    message::DeliveryProperties* dp2 =
        dynamic_cast<message::DeliveryProperties*>(h2.get());
    BOOST_CHECK(dp2);
    BOOST_CHECK(!dp2->discardUnroutable);
    BOOST_CHECK(dp2->immediate);
    BOOST_CHECK(!dp2->redelivered);
    BOOST_CHECK_EQUAL(dp2->priority, message::MEDIUM);
    BOOST_CHECK_EQUAL(dp2->exchange, "foo");
}

struct RecodeUnit {
    template <class T>
    void operator() (const T& t) {
        BOOST_MESSAGE(BOOST_CURRENT_FUNCTION  << " called with: " << t);
        using qpid::framing::Buffer;
        using qpid::framing::AMQFrame;

        session::Header sh;
        BOOST_CHECK_EQUAL(Codec::size(sh), 2u);

        // Encode unit.
        Unit u(t);
        string data;
        Codec::encode(back_inserter(data))(u.getHeader())(u);
        data.push_back(char(0xCE)); // Preview end-of-frame

        // Decode AMQFrame
        Buffer buf(&data[0], data.size());
        AMQFrame f;
        f.decode(buf);
        BOOST_MESSAGE("AMQFrame decoded: " << f);
        // Encode AMQFrame
        string data2(f.size(), ' ');
        Buffer buf2(&data2[0], data.size());
        f.encode(buf2);

        // Verify encoded by unit == encoded by AMQFrame
        BOOST_CHECK_MESSAGE(data == data2, BOOST_CURRENT_FUNCTION);

        // Decode unit
        // FIXME aconway 2008-04-15: must set limit to decode a header.
        Codec::Decoder<string::iterator> decode(data2.begin(), data2.size()-1);

        FrameHeader h;
        decode(h);
        BOOST_CHECK_EQUAL(u.getHeader(), h);
        Unit u2(h);
        decode(u2);

        // Re-encode unit
        string data3;
        Codec::encode(back_inserter(data3))(u2.getHeader())(u2);        
        data3.push_back(char(0xCE)); // Preview end-of-frame

        BOOST_CHECK_MESSAGE(data3 == data2, BOOST_CURRENT_FUNCTION);
    }
};

QPID_AUTO_TEST_CASE(testSerializeAllSegmentTypes) {
    RecodeUnit recode;
    allSegmentTypes(recode);
}

QPID_AUTO_TEST_SUITE_END()
