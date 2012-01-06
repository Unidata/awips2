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
#include "qpid/framing/Array.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/Exception.h"
#include "qpid/framing/reply_exceptions.h"
#include <assert.h>

namespace qpid {
namespace framing {

Array::Array() : type(TYPE_CODE_VOID) {}

Array::Array(TypeCode t) : type(t) {}

Array::Array(uint8_t t) : type(typeCode(t)) {}

Array::Array(const std::vector<std::string>& in)
{
    type = TYPE_CODE_STR16;
    for (std::vector<std::string>::const_iterator i = in.begin(); i != in.end(); ++i) {
        ValuePtr value(new Str16Value(*i));
        values.push_back(value);
    }
} 

uint32_t Array::encodedSize() const {
    //note: size is only included when used as a 'top level' type
    uint32_t len(4/*size*/ + 1/*type*/ + 4/*count*/);
    for(ValueVector::const_iterator i = values.begin(); i != values.end(); ++i) {
	len += (*i)->getData().encodedSize();
    }
    return len;
}

int Array::count() const {
    return values.size();
}

std::ostream& operator<<(std::ostream& out, const Array& a) {
    out << typeName(a.getType()) << "{";
    for(Array::ValueVector::const_iterator i = a.values.begin(); i != a.values.end(); ++i) {
        if (i != a.values.begin()) out << ", ";
        (*i)->print(out);
    }
    return out << "}";
}

void Array::encode(Buffer& buffer) const{
    buffer.putLong(encodedSize() - 4);//size added only when array is a top-level type
    buffer.putOctet(type);
    buffer.putLong(count());
    for (ValueVector::const_iterator i = values.begin(); i!=values.end(); ++i) {
    	(*i)->getData().encode(buffer);
    }
}

void Array::decode(Buffer& buffer){
    values.clear();
    uint32_t size = buffer.getLong();//size added only when array is a top-level type
    uint32_t available = buffer.available();
    if (available < size) {
        throw IllegalArgumentException(QPID_MSG("Not enough data for array, expected " 
                                            << size << " bytes but only " << available << " available"));
    }
    if (size) {
        type = TypeCode(buffer.getOctet());
        uint32_t count = buffer.getLong();
        
        FieldValue dummy;
        dummy.setType(type);
        available = buffer.available();
        if (available < count * dummy.getData().encodedSize()) {
            throw IllegalArgumentException(QPID_MSG("Not enough data for array, expected " 
                                                << count << " items of " << dummy.getData().encodedSize()
                                                << " bytes each  but only " << available << " bytes available"));
        }
        
        for (uint32_t i = 0; i < count; i++) {
            ValuePtr value(new FieldValue);
            value->setType(type);
            value->getData().decode(buffer);
            values.push_back(ValuePtr(value));
        }    
    }
}


bool Array::operator==(const Array& x) const {
    if (type != x.type) return false;
    if (values.size() != x.values.size()) return false;

    for (ValueVector::const_iterator i =  values.begin(), j = x.values.begin(); i != values.end(); ++i, ++j) {
        if (*(i->get()) != *(j->get())) return false;
    }

    return true;
}

void Array::insert(iterator i, ValuePtr value) {
    if (type != value->getType()) {
        // FIXME aconway 2008-10-31:  put meaningful strings in this message.
        throw Exception(QPID_MSG("Wrong type of value in Array, expected " << type
                                 << " but found " << TypeCode(value->getType())));
    }
    values.insert(i, value);
}


}
}
