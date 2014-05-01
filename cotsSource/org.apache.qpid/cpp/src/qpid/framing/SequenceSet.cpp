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

#include "qpid/framing/SequenceSet.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/reply_exceptions.h"

using namespace qpid::framing;
using std::max;
using std::min;

namespace qpid {
namespace framing {

namespace {
//each range contains 2 numbers, 4 bytes each
uint16_t RANGE_SIZE = 2 * 4; 
}

void SequenceSet::encode(Buffer& buffer) const
{
    buffer.putShort(rangesSize() * RANGE_SIZE);
    for (RangeIterator i = rangesBegin(); i != rangesEnd(); i++) {
        buffer.putLong(i->first().getValue());
        buffer.putLong(i->last().getValue());
    }
}

void SequenceSet::decode(Buffer& buffer)
{
    clear();
    uint16_t size = buffer.getShort();
    uint16_t count = size / RANGE_SIZE;//number of ranges
    if (size % RANGE_SIZE)
        throw IllegalArgumentException(QPID_MSG("Invalid size for sequence set: " << size)); 

    for (uint16_t i = 0; i < count; i++) {
        add(SequenceNumber(buffer.getLong()), SequenceNumber(buffer.getLong()));
    }
}

uint32_t SequenceSet::encodedSize() const {
    return 2 /*size field*/ + (rangesSize() * RANGE_SIZE);
}

bool SequenceSet::contains(const SequenceNumber& s) const {
    return RangeSet<SequenceNumber>::contains(s);
}

void SequenceSet::add(const SequenceNumber& s) { *this += s; }

void SequenceSet::add(const SequenceNumber& start, const SequenceNumber& finish) {
    *this += Range<SequenceNumber>::makeClosed(std::min(start,finish), std::max(start, finish));
}

void SequenceSet::add(const SequenceSet& set) { *this += set; }

void SequenceSet::remove(const SequenceSet& set) { *this -= set; }

void SequenceSet::remove(const SequenceNumber& start, const SequenceNumber& finish) {
    *this -= Range<SequenceNumber>::makeClosed(std::min(start,finish), std::max(start, finish));
}

void SequenceSet::remove(const SequenceNumber& s) { *this -= s; }


struct RangePrinter {
    std::ostream& out;
    RangePrinter(std::ostream& o) : out(o) {}
    void operator()(SequenceNumber i, SequenceNumber j) const {
        out << "[" << i.getValue() << "," << j.getValue() << "] ";
    }
};

std::ostream& operator<<(std::ostream& o, const SequenceSet& s) {
    RangePrinter print(o);
    o << "{ ";
    s.for_each(print);
    return o << "}";
}

}} // namespace qpid::framing

