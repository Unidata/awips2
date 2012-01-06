/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */


#include "unit_test.h"
#include "test_tools.h"
#include "qpid/RangeSet.h"

using namespace std;
using namespace qpid;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(RangeSetTestSuite)

typedef qpid::Range<int> TestRange;
typedef qpid::RangeSet<int> TestRangeSet;

QPID_AUTO_TEST_CASE(testEmptyRange) {
    TestRange r;
    BOOST_CHECK(r.empty());
    BOOST_CHECK(!r.contains(0));
    //    BOOST_CHECK(r.contiguous(0));
}

QPID_AUTO_TEST_CASE(testRangeSetAddPoint) {
    TestRangeSet r;
    BOOST_CHECK(r.empty());
    r += 3;
    BOOST_CHECK_MESSAGE(r.contains(3), r);
    BOOST_CHECK_MESSAGE(r.contains(TestRange(3,4)), r);
    BOOST_CHECK(!r.empty());
    r += 5;
    BOOST_CHECK_MESSAGE(r.contains(5), r);
    BOOST_CHECK_MESSAGE(r.contains(TestRange(5,6)), r);
    BOOST_CHECK_MESSAGE(!r.contains(TestRange(3,6)), r);
    r += 4;
    BOOST_CHECK_MESSAGE(r.contains(TestRange(3,6)), r);
}

QPID_AUTO_TEST_CASE(testRangeSetAddRange) {
    TestRangeSet r;
    r += TestRange(0,3);
    BOOST_CHECK(r.contains(TestRange(0,3)));
    r += TestRange(4,6);
    BOOST_CHECK_MESSAGE(r.contains(TestRange(4,6)), r);
    r += 3;
    BOOST_CHECK_MESSAGE(r.contains(TestRange(0,6)), r);
    BOOST_CHECK(r.front() == 0);
    BOOST_CHECK(r.back() == 6);
}

QPID_AUTO_TEST_CASE(testRangeSetAddSet) {
    TestRangeSet r;
    TestRangeSet s = TestRangeSet(0,3)+TestRange(5,10);
    r += s;
    BOOST_CHECK_EQUAL(r,s);
    r += TestRangeSet(3,5) + TestRange(7,12) + 15;
    BOOST_CHECK_EQUAL(r, TestRangeSet(0,12) + 15);

    r.clear();
    BOOST_CHECK(r.empty());
    r += TestRange::makeClosed(6,10);
    BOOST_CHECK_EQUAL(r, TestRangeSet(6,11));
    r += TestRangeSet(2,6)+8;
    BOOST_CHECK_EQUAL(r, TestRangeSet(2,11));
}

QPID_AUTO_TEST_CASE(testRangeSetIterate) {
    TestRangeSet r;
    (((r += 1) += 10) += TestRange(4,7)) += 2;
    BOOST_MESSAGE(r);
    std::vector<int> actual;
    std::copy(r.begin(), r.end(), std::back_inserter(actual));
    std::vector<int> expect = boost::assign::list_of(1)(2)(4)(5)(6)(10);
    BOOST_CHECK_EQUAL(expect, actual);
}

QPID_AUTO_TEST_CASE(testRangeSetRemove) {
    // points
    BOOST_CHECK_EQUAL(TestRangeSet(0,5)-3, TestRangeSet(0,3)+TestRange(4,5));
    BOOST_CHECK_EQUAL(TestRangeSet(1,5)-5, TestRangeSet(1,5));
    BOOST_CHECK_EQUAL(TestRangeSet(1,5)-0, TestRangeSet(1,5));

    TestRangeSet r(TestRangeSet(0,5)+TestRange(10,15)+TestRange(20,25));

    // TestRanges
    BOOST_CHECK_EQUAL(r-TestRange(0,5), TestRangeSet(10,15)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(10,15), TestRangeSet(0,5)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(20,25), TestRangeSet(0,5)+TestRange(10,15));

    BOOST_CHECK_EQUAL(r-TestRange(-5, 30), TestRangeSet());

    BOOST_CHECK_EQUAL(r-TestRange(-5, 7), TestRangeSet(10,15)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(8,19), TestRangeSet(0,5)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(17,30), TestRangeSet(0,5)+TestRange(10,15));
    BOOST_CHECK_EQUAL(r-TestRange(17,30), TestRangeSet(0,5)+TestRange(10,15));

    BOOST_CHECK_EQUAL(r-TestRange(-5, 5), TestRangeSet(10,15)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(10,19), TestRangeSet(0,5)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(18,25), TestRangeSet(0,5)+TestRange(10,15));
    BOOST_CHECK_EQUAL(r-TestRange(23,25), TestRangeSet(0,5)+TestRange(10,15)+TestRange(20,23));

    BOOST_CHECK_EQUAL(r-TestRange(-3, 3), TestRangeSet(3,5)+TestRange(10,15)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(3, 7), TestRangeSet(0,2)+TestRange(10,15)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(3, 12), TestRangeSet(0,3)+TestRange(12,15)+TestRange(20,25));
    BOOST_CHECK_EQUAL(r-TestRange(3, 22), TestRangeSet(12,15)+TestRange(22,25));
    BOOST_CHECK_EQUAL(r-TestRange(12, 22), TestRangeSet(0,5)+TestRange(10,11)+TestRange(22,25));

    // Sets
    BOOST_CHECK_EQUAL(r-(TestRangeSet(-1,6)+TestRange(11,14)+TestRange(23,25)),
                      TestRangeSet(10,11)+TestRange(14,15)+TestRange(20,23));
}

QPID_AUTO_TEST_CASE(testRangeContaining) {
    TestRangeSet r;
    (((r += 1) += TestRange(3,5)) += 7);
    BOOST_CHECK_EQUAL(r.rangeContaining(0), TestRange(0,0));
    BOOST_CHECK_EQUAL(r.rangeContaining(1), TestRange(1,2));
    BOOST_CHECK_EQUAL(r.rangeContaining(2), TestRange(2,2));
    BOOST_CHECK_EQUAL(r.rangeContaining(3), TestRange(3,5));
    BOOST_CHECK_EQUAL(r.rangeContaining(4), TestRange(3,5));
    BOOST_CHECK_EQUAL(r.rangeContaining(5), TestRange(5,5));
    BOOST_CHECK_EQUAL(r.rangeContaining(6), TestRange(6,6));
    BOOST_CHECK_EQUAL(r.rangeContaining(7), TestRange(7,8));
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
