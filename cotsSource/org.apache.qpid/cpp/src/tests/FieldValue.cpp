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
#include "qpid/framing/FieldValue.h"

#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(FieldValueTestSuite)

using namespace qpid::framing;

Str16Value s("abc");
IntegerValue i(42);
//DecimalValue d(1234,2);
//FieldTableValue ft;
//EmptyValue e;

QPID_AUTO_TEST_CASE(testStr16ValueEquals)
{

    BOOST_CHECK(Str16Value("abc") == s);
    BOOST_CHECK(Str16Value("foo") != s);
    BOOST_CHECK(s != i);
    BOOST_CHECK(s.convertsTo<std::string>() == true);
    BOOST_CHECK(s.convertsTo<int>() == false);
    BOOST_CHECK(s.get<std::string>() == "abc");
    BOOST_CHECK_THROW(s.get<int>(), InvalidConversionException);
//    BOOST_CHECK(s != ft);

}

QPID_AUTO_TEST_CASE(testIntegerValueEquals)
{
    BOOST_CHECK(IntegerValue(42) == i);
    BOOST_CHECK(IntegerValue(5) != i);
    BOOST_CHECK(i != s);
    BOOST_CHECK(i.convertsTo<std::string>() == false);
    BOOST_CHECK(i.convertsTo<int>() == true);
    BOOST_CHECK_THROW(i.get<std::string>(), InvalidConversionException);
    BOOST_CHECK(i.get<int>() == 42);
//    BOOST_CHECK(i != ft);
}

#if 0
QPID_AUTO_TEST_CASE(testDecimalValueEquals)
{
    BOOST_CHECK(DecimalValue(1234, 2) == d);
    BOOST_CHECK(DecimalValue(12345, 2) != d);
    BOOST_CHECK(DecimalValue(1234, 3) != d);
    BOOST_CHECK(d != s);
}

QPID_AUTO_TEST_CASE(testFieldTableValueEquals)
{
    ft.getValue().setString("foo", "FOO");
    ft.getValue().setInt("magic", 7);

    BOOST_CHECK_EQUAL(std::string("FOO"),
                            ft.getValue().getString("foo"));
    BOOST_CHECK_EQUAL(7, ft.getValue().getInt("magic"));

    FieldTableValue f2;
    BOOST_CHECK(ft != f2);
    f2.getValue().setString("foo", "FOO");
    BOOST_CHECK(ft != f2);
    f2.getValue().setInt("magic", 7);
    BOOST_CHECK_EQUAL(ft,f2);
    BOOST_CHECK(ft == f2);
    f2.getValue().setString("foo", "BAR");
    BOOST_CHECK(ft != f2);
    BOOST_CHECK(ft != i);
}
#endif

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
