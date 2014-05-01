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

#include "qpid/framing/SequenceSet.h"
#include "unit_test.h"
#include <list>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(SequenceSetTestSuite)

using namespace qpid::framing;

struct RangeExpectations
{
    typedef std::pair<SequenceNumber, SequenceNumber> Range;
    typedef std::list<Range> Ranges;

    Ranges ranges;

    RangeExpectations& expect(const SequenceNumber& start, const SequenceNumber& end) {
        ranges.push_back(Range(start, end));
        return *this;
    }

    void operator()(const SequenceNumber& start, const SequenceNumber& end) {
        BOOST_CHECK(!ranges.empty());
        if (!ranges.empty()) {
            BOOST_CHECK_EQUAL(start, ranges.front().first);
            BOOST_CHECK_EQUAL(end, ranges.front().second);
            ranges.pop_front();
        }
    }

    void check(SequenceSet& set) {
        set.for_each(*this);
        BOOST_CHECK(ranges.empty());
    }
};

QPID_AUTO_TEST_CASE(testAdd) {
    SequenceSet s;
    s.add(2);
    s.add(8,8);
    s.add(3,5);

    for (uint32_t i = 0; i <= 1; i++)
        BOOST_CHECK(!s.contains(i));

    for (uint32_t i = 2; i <= 5; i++)
        BOOST_CHECK(s.contains(i));

    for (uint32_t i = 6; i <= 7; i++)
        BOOST_CHECK(!s.contains(i));

    BOOST_CHECK(s.contains(8));

    for (uint32_t i = 9; i <= 10; i++)
        BOOST_CHECK(!s.contains(i));

    RangeExpectations().expect(2, 5).expect(8, 8).check(s);

    SequenceSet t;
    t.add(6, 10);
    t.add(s);

    for (uint32_t i = 0; i <= 1; i++)
        BOOST_CHECK(!t.contains(i));

    for (uint32_t i = 2; i <= 10; i++)
        BOOST_CHECK_MESSAGE(t.contains(i), t << " contains " << i);

    RangeExpectations().expect(2, 10).check(t);
}

QPID_AUTO_TEST_CASE(testAdd2) {
    SequenceSet s;
    s.add(7,6);
    s.add(4,4);
    s.add(3,10);
    s.add(2);
    RangeExpectations().expect(2, 10).check(s);
}

QPID_AUTO_TEST_CASE(testRemove) {
    SequenceSet s;
    SequenceSet t;
    s.add(0, 10);
    t.add(0, 10);

    s.remove(7);
    s.remove(3, 5);
    s.remove(9, 10);

    t.remove(s);

    for (uint32_t i = 0; i <= 2; i++) {
        BOOST_CHECK(s.contains(i));
        BOOST_CHECK(!t.contains(i));
    }

    for (uint32_t i = 3; i <= 5; i++) {
        BOOST_CHECK(!s.contains(i));
        BOOST_CHECK(t.contains(i));
    }

    BOOST_CHECK(s.contains(6));
    BOOST_CHECK(!t.contains(6));

    BOOST_CHECK(!s.contains(7));
    BOOST_CHECK(t.contains(7));

    BOOST_CHECK(s.contains(8));
    BOOST_CHECK(!t.contains(8));

    for (uint32_t i = 9; i <= 10; i++) {
        BOOST_CHECK(!s.contains(i));
        BOOST_CHECK(t.contains(i));
    }

    RangeExpectations().expect(0, 2).expect(6, 6).expect(8, 8).check(s);
    RangeExpectations().expect(3, 5).expect(7, 7).expect(9, 10).check(t);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
