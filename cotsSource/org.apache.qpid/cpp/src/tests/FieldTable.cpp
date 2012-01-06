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
#include "qpid/framing/Array.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/framing/List.h"
#include "qpid/sys/alloca.h"

#include "unit_test.h"

using namespace qpid::framing;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(FieldTableTestSuite)

QPID_AUTO_TEST_CASE(testMe)
{
    FieldTable ft;
    ft.setString("A", "BCDE");
    BOOST_CHECK(string("BCDE") == ft.getAsString("A"));

    char buff[100];
    Buffer wbuffer(buff, 100);
    wbuffer.put(ft);

    Buffer rbuffer(buff, 100);
    FieldTable ft2;
    rbuffer.get(ft2);
    BOOST_CHECK(string("BCDE") == ft2.getAsString("A"));

}

QPID_AUTO_TEST_CASE(testAssignment)
{
    FieldTable a;
    FieldTable b;

    a.setString("A", "BBBB");
    a.setInt("B", 1234);
    b = a;
    a.setString("A", "CCCC");

    BOOST_CHECK(string("CCCC") == a.getAsString("A"));
    BOOST_CHECK(string("BBBB") == b.getAsString("A"));
    BOOST_CHECK_EQUAL(1234, a.getAsInt("B"));
    BOOST_CHECK_EQUAL(1234, b.getAsInt("B"));
    BOOST_CHECK(IntegerValue(1234) == *a.get("B"));
    BOOST_CHECK(IntegerValue(1234) == *b.get("B"));

    FieldTable d;
    {
        FieldTable c;
        c = a;

        char* buff = static_cast<char*>(::alloca(c.encodedSize()));
        Buffer wbuffer(buff, c.encodedSize());
        wbuffer.put(c);

        Buffer rbuffer(buff, c.encodedSize());
        rbuffer.get(d);
        BOOST_CHECK_EQUAL(c, d);
        BOOST_CHECK(string("CCCC") == c.getAsString("A"));
        BOOST_CHECK(IntegerValue(1234) == *c.get("B"));
    }
    BOOST_CHECK(string("CCCC") == d.getAsString("A"));
    BOOST_CHECK(IntegerValue(1234) == *d.get("B"));
}


QPID_AUTO_TEST_CASE(testNestedValues)
{
    double d = 1.2345;
    uint32_t u = 101;
    char buff[1000];
    {
        FieldTable a;
        FieldTable b;
        std::vector<std::string> items;
        items.push_back("one");
        items.push_back("two");
        Array c(items);
        List list;
        list.push_back(List::ValuePtr(new Str16Value("red")));
        list.push_back(List::ValuePtr(new Unsigned32Value(u)));
        list.push_back(List::ValuePtr(new Str8Value("yellow")));
        list.push_back(List::ValuePtr(new DoubleValue(d)));

        a.setString("id", "A");
        b.setString("id", "B");
        a.setTable("B", b);
        a.setArray("C", c);
        a.set("my-list", FieldTable::ValuePtr(new ListValue(list)));


        Buffer wbuffer(buff, 100);
        wbuffer.put(a);
    }
    {
        Buffer rbuffer(buff, 100);
        FieldTable a;
        FieldTable b;
        Array c;
        rbuffer.get(a);
        BOOST_CHECK(string("A") == a.getAsString("id"));
        a.getTable("B", b);
        BOOST_CHECK(string("B") == b.getAsString("id"));
        a.getArray("C", c);
        std::vector<std::string> items;
        c.collect(items);
        BOOST_CHECK((uint) 2 == items.size());
        BOOST_CHECK(string("one") == items[0]);
        BOOST_CHECK(string("two") == items[1]);

        List list;
        BOOST_CHECK(a.get("my-list")->get<List>(list));
        List::const_iterator i = list.begin();
        BOOST_CHECK(i != list.end());
        BOOST_CHECK_EQUAL(std::string("red"), (*i)->get<std::string>());

        i++;
        BOOST_CHECK(i != list.end());
        BOOST_CHECK_EQUAL(u, (uint32_t) (*i)->get<int>());

        i++;
        BOOST_CHECK(i != list.end());
        BOOST_CHECK_EQUAL(std::string("yellow"), (*i)->get<std::string>());

        i++;
        BOOST_CHECK(i != list.end());
        BOOST_CHECK_EQUAL(d, (*i)->get<double>());

        i++;
        BOOST_CHECK(i == list.end());
    }
}

QPID_AUTO_TEST_CASE(testFloatAndDouble)
{
    char buff[100];
    float f = 5.672f;
    double d = 56.720001;
    {
        FieldTable a;
        a.setString("string", "abc");
        a.setInt("int", 5672);
        a.setFloat("float", f);
        a.setDouble("double", d);

        Buffer wbuffer(buff, 100);
        wbuffer.put(a);
    }
    {
        Buffer rbuffer(buff, 100);
        FieldTable a;
        rbuffer.get(a);
        BOOST_CHECK(string("abc") == a.getAsString("string"));
        BOOST_CHECK(5672 == a.getAsInt("int"));
        float f2;
        BOOST_CHECK(!a.getFloat("string", f2));
        BOOST_CHECK(!a.getFloat("int", f2));
        BOOST_CHECK(a.getFloat("float", f2));
        BOOST_CHECK(f2 == f);

        double d2;
        BOOST_CHECK(!a.getDouble("string", d2));
        BOOST_CHECK(!a.getDouble("int", d2));
        BOOST_CHECK(a.getDouble("double", d2));
        BOOST_CHECK(d2 == d);
    }
}

QPID_AUTO_TEST_CASE(test64GetAndSetConverts)
{
    FieldTable args;
    args.setInt64("a",100);
    args.setInt64("b",-(int64_t) ((int64_t) 1<<34));

    args.setUInt64("c",1u);
    args.setUInt64("d",(uint64_t) ((uint64_t) 1<<34));
    BOOST_CHECK_EQUAL(1u, args.getAsUInt64("c"));
    BOOST_CHECK_EQUAL(100u, args.getAsUInt64("a"));
    BOOST_CHECK_EQUAL(1, args.getAsInt64("c"));
    BOOST_CHECK_EQUAL(100, args.getAsInt64("a"));
    BOOST_CHECK_EQUAL(-(int64_t) ((int64_t) 1<<34), args.getAsInt64("b"));
    BOOST_CHECK_EQUAL((uint64_t) ((uint64_t) 1<<34), args.getAsUInt64("d"));
    BOOST_CHECK_EQUAL((int64_t) ((int64_t) 1<<34), args.getAsInt64("d"));

}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
