
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
#include "qpid/framing/AccumulatedAck.h"
#include "unit_test.h"
#include <iostream>
#include <list>

using std::list;
using namespace qpid::framing;


namespace qpid {
namespace tests {

bool covers(const AccumulatedAck& ack, int i)
{
    return ack.covers(SequenceNumber(i));
}

void update(AccumulatedAck& ack, int start, int end)
{
    ack.update(SequenceNumber(start), SequenceNumber(end));
}

QPID_AUTO_TEST_SUITE(AccumulatedAckTestSuite)

QPID_AUTO_TEST_CASE(testGeneral)
{
    AccumulatedAck ack(0);
    ack.clear();
    update(ack, 3,3);
    update(ack, 7,7);
    update(ack, 9,9);
    update(ack, 1,2);
    update(ack, 4,5);
    update(ack, 6,6);

    for(int i = 1; i <= 7; i++) BOOST_CHECK(covers(ack, i));
    BOOST_CHECK(covers(ack, 9));

    BOOST_CHECK(!covers(ack, 8));
    BOOST_CHECK(!covers(ack, 10));

    ack.consolidate();

    for(int i = 1; i <= 7; i++) BOOST_CHECK(covers(ack, i));
    BOOST_CHECK(covers(ack, 9));

    BOOST_CHECK(!covers(ack, 8));
    BOOST_CHECK(!covers(ack, 10));
}

QPID_AUTO_TEST_CASE(testCovers)
{
    AccumulatedAck ack(5);
    update(ack, 7, 7);
    update(ack, 9, 9);

    BOOST_CHECK(covers(ack, 1));
    BOOST_CHECK(covers(ack, 2));
    BOOST_CHECK(covers(ack, 3));
    BOOST_CHECK(covers(ack, 4));
    BOOST_CHECK(covers(ack, 5));
    BOOST_CHECK(covers(ack, 7));
    BOOST_CHECK(covers(ack, 9));

    BOOST_CHECK(!covers(ack, 6));
    BOOST_CHECK(!covers(ack, 8));
    BOOST_CHECK(!covers(ack, 10));
}

QPID_AUTO_TEST_CASE(testUpdateFromCompletionData)
{
    AccumulatedAck ack(0);
    SequenceNumber mark(2);
    SequenceNumberSet ranges;
    ranges.addRange(SequenceNumber(5), SequenceNumber(8));
    ranges.addRange(SequenceNumber(10), SequenceNumber(15));
    ranges.addRange(SequenceNumber(9), SequenceNumber(9));
    ranges.addRange(SequenceNumber(3), SequenceNumber(4));

    ack.update(mark, ranges);

    for(int i = 0; i <= 15; i++) {
        BOOST_CHECK(covers(ack, i));
    }
    BOOST_CHECK(!covers(ack, 16));
    BOOST_CHECK_EQUAL((uint32_t) 15, ack.mark.getValue());
}

QPID_AUTO_TEST_CASE(testCase1)
{
    AccumulatedAck ack(3);
    update(ack, 1,2);
    for(int i = 1; i <= 3; i++) BOOST_CHECK(covers(ack, i));
    BOOST_CHECK(!covers(ack, 4));
}

QPID_AUTO_TEST_CASE(testCase2)
{
    AccumulatedAck ack(3);
    update(ack, 3,6);
    for(int i = 1; i <= 6; i++) BOOST_CHECK(covers(ack, i));
    BOOST_CHECK(!covers(ack, 7));
}

QPID_AUTO_TEST_CASE(testCase3)
{
    AccumulatedAck ack(3);
    update(ack, 4,6);
    for(int i = 1; i <= 6; i++) {
        BOOST_CHECK(covers(ack, i));
    }
    BOOST_CHECK(!covers(ack, 7));
}

QPID_AUTO_TEST_CASE(testCase4)
{
    AccumulatedAck ack(3);
    update(ack, 5,6);
    for(int i = 1; i <= 6; i++) {
        if (i == 4) BOOST_CHECK(!covers(ack, i));
        else BOOST_CHECK(covers(ack, i));
    }
    BOOST_CHECK(!covers(ack, 7));
}

QPID_AUTO_TEST_CASE(testConsolidation1)
{
    AccumulatedAck ack(3);
    update(ack, 7,7);
    BOOST_CHECK_EQUAL((uint32_t) 3, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());

    update(ack, 8,9);
    BOOST_CHECK_EQUAL((uint32_t) 3, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());

    update(ack, 1,2);
    BOOST_CHECK_EQUAL((uint32_t) 3, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());

    update(ack, 4,5);
    BOOST_CHECK_EQUAL((uint32_t) 5, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());

    update(ack, 6,6);
    BOOST_CHECK_EQUAL((uint32_t) 9, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 0, ack.ranges.size());

    for(int i = 1; i <= 9; i++) BOOST_CHECK(covers(ack, i));
    BOOST_CHECK(!covers(ack, 10));
}

QPID_AUTO_TEST_CASE(testConsolidation2)
{
    AccumulatedAck ack(0);
    update(ack, 10,12);
    BOOST_CHECK_EQUAL((uint32_t) 0, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());

    update(ack, 7,9);
    BOOST_CHECK_EQUAL((uint32_t) 0, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());
    BOOST_CHECK_EQUAL((uint32_t) 7, ack.ranges.front().start.getValue());
    BOOST_CHECK_EQUAL((uint32_t) 12, ack.ranges.front().end.getValue());

    update(ack, 5,7);
    BOOST_CHECK_EQUAL((uint32_t) 0, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());
    BOOST_CHECK_EQUAL((uint32_t) 5, ack.ranges.front().start.getValue());
    BOOST_CHECK_EQUAL((uint32_t) 12, ack.ranges.front().end.getValue());

    update(ack, 3,4);
    BOOST_CHECK_EQUAL((uint32_t) 0, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 1, ack.ranges.size());
    BOOST_CHECK_EQUAL((uint32_t) 3, ack.ranges.front().start.getValue());
    BOOST_CHECK_EQUAL((uint32_t) 12, ack.ranges.front().end.getValue());

    update(ack, 1,2);
    BOOST_CHECK_EQUAL((uint32_t) 12, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 0, ack.ranges.size());

    for(int i = 1; i <= 12; i++) BOOST_CHECK(covers(ack, i));
    BOOST_CHECK(!covers(ack, 13));
}

QPID_AUTO_TEST_CASE(testConsolidation3)
{
    AccumulatedAck ack(0);
    update(ack, 10,12);
    update(ack, 6,7);
    update(ack, 3,4);
    update(ack, 1,15);
    BOOST_CHECK_EQUAL((uint32_t) 15, ack.mark.getValue());
    BOOST_CHECK_EQUAL((size_t) 0, ack.ranges.size());
}

QPID_AUTO_TEST_CASE(testConsolidation4)
{
    AccumulatedAck ack(0);
    ack.update(SequenceNumber(0), SequenceNumber(2));
    ack.update(SequenceNumber(5), SequenceNumber(8));
    ack.update(SequenceNumber(10), SequenceNumber(15));
    ack.update(SequenceNumber(9), SequenceNumber(9));
    ack.update(SequenceNumber(3), SequenceNumber(4));

    for(int i = 0; i <= 15; i++) {
        BOOST_CHECK(covers(ack, i));
    }
    BOOST_CHECK(!covers(ack, 16));
    BOOST_CHECK_EQUAL((uint32_t) 15, ack.mark.getValue());
}

QPID_AUTO_TEST_SUITE_END()


}} // namespace qpid::tests
