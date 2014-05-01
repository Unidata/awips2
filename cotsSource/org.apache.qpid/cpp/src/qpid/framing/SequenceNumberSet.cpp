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

#include "qpid/framing/SequenceNumberSet.h"

using namespace qpid::framing;

void SequenceNumberSet::encode(Buffer& buffer) const
{
    buffer.putShort(size() * 4);
    for (const_iterator i = begin(); i != end(); i++) {
        buffer.putLong(i->getValue());
    }
}

void SequenceNumberSet::decode(Buffer& buffer)
{
    clear();
    uint16_t count = (buffer.getShort() / 4);
    for (uint16_t i = 0; i < count; i++) {
        push_back(SequenceNumber(buffer.getLong()));
    }
}

uint32_t SequenceNumberSet::encodedSize() const
{
    return 2 /*count*/ + (size() * 4);
}

SequenceNumberSet SequenceNumberSet::condense() const
{
    SequenceNumberSet result;
    const_iterator last = end();
    const_iterator start = end();
    for (const_iterator i = begin(); i != end(); i++) {
        if (start == end()) {
            start = i;
        } else if (*i - *last > 1) {
            result.push_back(*start);
            result.push_back(*last);            
            start = i;
        }
        last = i;
    }
    if (start != end()) {
        result.push_back(*start);
        result.push_back(*last);            
    }
    return result;
}

void SequenceNumberSet::addRange(const SequenceNumber& start, const SequenceNumber& end)
{
    push_back(start);
    push_back(end);
}

namespace qpid{
namespace framing{

std::ostream& operator<<(std::ostream& out, const SequenceNumberSet& set) {
    out << "{";
    for (SequenceNumberSet::const_iterator i = set.begin(); i != set.end(); i++) {
        if (i != set.begin()) out << ", "; 
        out << (i->getValue());
    }
    out << "}";
    return out;
}

}
}
