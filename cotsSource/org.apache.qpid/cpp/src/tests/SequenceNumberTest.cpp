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

#include "unit_test.h"
#include <iostream>
#include "qpid/framing/SequenceNumber.h"
#include "qpid/framing/SequenceNumberSet.h"

using namespace qpid::framing;

namespace qpid {
namespace tests {

void checkDifference(SequenceNumber& a, SequenceNumber& b, int gap)
{
    BOOST_CHECK_EQUAL(gap, a - b);
    BOOST_CHECK_EQUAL(-gap, b - a);

    //increment until b wraps around
    for (int i = 0; i < (gap + 2); i++, ++a, ++b) {
        BOOST_CHECK_EQUAL(gap, a - b);
    }
    //keep incrementing until a also wraps around
    for (int i = 0; i < (gap + 2); i++, ++a, ++b) {
        BOOST_CHECK_EQUAL(gap, a - b);
    }
    //let b catch up and overtake
    for (int i = 0; i < (gap*2); i++, ++b) {
        BOOST_CHECK_EQUAL(gap - i, a - b);
        BOOST_CHECK_EQUAL(i - gap, b - a);
    }
}

void checkComparison(SequenceNumber& a, SequenceNumber& b, int gap)
{
    //increment until b wraps around
    for (int i = 0; i < (gap + 2); i++) {
        BOOST_CHECK(++a < ++b);//test prefix
    }
    //keep incrementing until a also wraps around
    for (int i = 0; i < (gap + 2); i++) {
        BOOST_CHECK(a++ < b++);//test postfix
    }
    //let a 'catch up'
    for (int i = 0; i < gap; i++) {
        a++;
    }
    BOOST_CHECK(a == b);
    BOOST_CHECK(++a > b);
}


QPID_AUTO_TEST_SUITE(SequenceNumberTestSuite)

QPID_AUTO_TEST_CASE(testIncrementPostfix)
{
    SequenceNumber a;
    SequenceNumber b;
    BOOST_CHECK(!(a > b));
    BOOST_CHECK(!(b < a));
    BOOST_CHECK(a == b);

    SequenceNumber c = a++;
    BOOST_CHECK(a > b);
    BOOST_CHECK(b < a);
    BOOST_CHECK(a != b);
    BOOST_CHECK(c < a);
    BOOST_CHECK(a != c);

    b++;
    BOOST_CHECK(!(a > b));
    BOOST_CHECK(!(b < a));
    BOOST_CHECK(a == b);
    BOOST_CHECK(c < b);
    BOOST_CHECK(b != c);
}

QPID_AUTO_TEST_CASE(testIncrementPrefix)
{
    SequenceNumber a;
    SequenceNumber b;
    BOOST_CHECK(!(a > b));
    BOOST_CHECK(!(b < a));
    BOOST_CHECK(a == b);

    SequenceNumber c = ++a;
    BOOST_CHECK(a > b);
    BOOST_CHECK(b < a);
    BOOST_CHECK(a != b);
    BOOST_CHECK(a == c);

    ++b;
    BOOST_CHECK(!(a > b));
    BOOST_CHECK(!(b < a));
    BOOST_CHECK(a == b);
}

QPID_AUTO_TEST_CASE(testWrapAround)
{
    const uint32_t max = 0xFFFFFFFF;
    SequenceNumber a(max - 10);
    SequenceNumber b(max - 5);
    checkComparison(a, b, 5);

    const uint32_t max_signed = 0x7FFFFFFF;
    SequenceNumber c(max_signed - 10);
    SequenceNumber d(max_signed - 5);
    checkComparison(c, d, 5);
}

QPID_AUTO_TEST_CASE(testCondense)
{
    SequenceNumberSet set;
    for (uint i = 0; i < 6; i++) {
        set.push_back(SequenceNumber(i));
    }
    set.push_back(SequenceNumber(7));
    for (uint i = 9; i < 13; i++) {
        set.push_back(SequenceNumber(i));
    }
    set.push_back(SequenceNumber(13));
    SequenceNumberSet actual = set.condense();

    SequenceNumberSet expected;
    expected.addRange(SequenceNumber(0), SequenceNumber(5));
    expected.addRange(SequenceNumber(7), SequenceNumber(7));
    expected.addRange(SequenceNumber(9), SequenceNumber(13));
    BOOST_CHECK_EQUAL(expected, actual);
}

QPID_AUTO_TEST_CASE(testCondenseSingleRange)
{
    SequenceNumberSet set;
    for (uint i = 0; i < 6; i++) {
        set.push_back(SequenceNumber(i));
    }
    SequenceNumberSet actual = set.condense();

    SequenceNumberSet expected;
    expected.addRange(SequenceNumber(0), SequenceNumber(5));
    BOOST_CHECK_EQUAL(expected, actual);
}

QPID_AUTO_TEST_CASE(testCondenseSingleItem)
{
    SequenceNumberSet set;
    set.push_back(SequenceNumber(1));
    SequenceNumberSet actual = set.condense();

    SequenceNumberSet expected;
    expected.addRange(SequenceNumber(1), SequenceNumber(1));
    BOOST_CHECK_EQUAL(expected, actual);
}

QPID_AUTO_TEST_CASE(testDifference)
{
    SequenceNumber a;
    SequenceNumber b;

    for (int i = 0; i < 10; i++, ++a) {
        BOOST_CHECK_EQUAL(i, a - b);
        BOOST_CHECK_EQUAL(-i, b - a);
    }

    b = a;

    for (int i = 0; i < 10; i++, ++b) {
        BOOST_CHECK_EQUAL(-i, a - b);
        BOOST_CHECK_EQUAL(i, b - a);
    }
}

QPID_AUTO_TEST_CASE(testDifferenceWithWrapAround1)
{
    const uint32_t max = 0xFFFFFFFF;
    SequenceNumber a(max - 5);
    SequenceNumber b(max - 10);
    checkDifference(a, b, 5);
}

QPID_AUTO_TEST_CASE(testDifferenceWithWrapAround2)
{
    const uint32_t max_signed = 0x7FFFFFFF;
    SequenceNumber c(max_signed - 5);
    SequenceNumber d(max_signed - 10);
    checkDifference(c, d, 5);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
