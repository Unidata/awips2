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
#include "qpid/framing/List.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/Exception.h"
#include "qpid/framing/reply_exceptions.h"

namespace qpid {
namespace framing {

uint32_t List::encodedSize() const
{
    uint32_t len(4/*size*/ + 4/*count*/);
    for(Values::const_iterator i = values.begin(); i != values.end(); ++i) {
	len += (*i)->encodedSize();
    }
    return len;
}

void List::encode(Buffer& buffer) const
{
    buffer.putLong(encodedSize() - 4);
    buffer.putLong(size());
    for (Values::const_iterator i = values.begin(); i!=values.end(); ++i) {
    	(*i)->encode(buffer);
    }
}

void List::decode(Buffer& buffer)
{
    values.clear();
    uint32_t size = buffer.getLong();
    uint32_t available = buffer.available();
    if (available < size) {
        throw IllegalArgumentException(QPID_MSG("Not enough data for list, expected " 
                                                << size << " bytes but only " << available << " available"));
    }
    if (size) {
        uint32_t count = buffer.getLong();        
        for (uint32_t i = 0; i < count; i++) {
            ValuePtr value(new FieldValue);
            value->decode(buffer);
            values.push_back(value);
        }    
    }
}


bool List::operator==(const List& other) const {
    return values.size() == other.values.size() && 
        std::equal(values.begin(), values.end(), other.values.begin());
}

std::ostream& operator<<(std::ostream& out, const List& l)
{
    out << "{";
    for(List::Values::const_iterator i = l.values.begin(); i != l.values.end(); ++i) {
        if (i != l.values.begin()) out << ", ";
        (*i)->print(out);
    }
    return out << "}";
}

}} // namespace qpid::framing
