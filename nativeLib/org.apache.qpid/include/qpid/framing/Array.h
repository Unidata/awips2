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
#include "qpid/framing/amqp_types.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/framing/TypeCode.h"
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <vector>
#include "qpid/CommonImportExport.h"

#ifndef _Array_
#define _Array_

namespace qpid {
namespace framing {

class Buffer;

class Array
{
  public:
    typedef boost::shared_ptr<FieldValue> ValuePtr;
    typedef std::vector<ValuePtr> ValueVector;
    typedef ValueVector::const_iterator const_iterator;
    typedef ValueVector::iterator iterator;

    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN void encode(Buffer& buffer) const;
    QPID_COMMON_EXTERN void decode(Buffer& buffer);

    QPID_COMMON_EXTERN int count() const;
    QPID_COMMON_EXTERN bool operator==(const Array& other) const;

    QPID_COMMON_EXTERN Array();
    QPID_COMMON_EXTERN Array(TypeCode type);
    QPID_COMMON_EXTERN Array(uint8_t type);
    //creates a longstr array
    QPID_COMMON_EXTERN Array(const std::vector<std::string>& in);

    QPID_COMMON_EXTERN TypeCode getType() const { return type; }

    // std collection interface.
    QPID_COMMON_EXTERN const_iterator begin() const { return values.begin(); }
    QPID_COMMON_EXTERN const_iterator end() const { return values.end(); }
    QPID_COMMON_EXTERN iterator begin() { return values.begin(); }
    QPID_COMMON_EXTERN iterator end(){ return values.end(); }

    QPID_COMMON_EXTERN ValuePtr front() const { return values.front(); }
    QPID_COMMON_EXTERN ValuePtr back() const { return values.back(); }
    QPID_COMMON_EXTERN size_t size() const { return values.size(); }

    QPID_COMMON_EXTERN void insert(iterator i, ValuePtr value);
    QPID_COMMON_EXTERN void erase(iterator i) { values.erase(i); }
    QPID_COMMON_EXTERN void push_back(ValuePtr value) { values.insert(end(), value); }
    QPID_COMMON_EXTERN void pop_back() { values.pop_back(); }

    // Non-std interface
    QPID_COMMON_EXTERN void add(ValuePtr value) { push_back(value); }

    template <class T>
    void collect(std::vector<T>& out) const
    {
        for (ValueVector::const_iterator i = values.begin(); i != values.end(); ++i) {
            out.push_back((*i)->get<T>());
        }
    }

  private:
    TypeCode type;
    ValueVector values;

    friend QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream& out, const Array& body);
};

}
}


#endif
