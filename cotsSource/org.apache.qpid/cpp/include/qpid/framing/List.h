#ifndef QPID_FRAMING_LIST_H
#define QPID_FRAMING_LIST_H

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
#include "qpid/CommonImportExport.h"
#include "qpid/framing/amqp_types.h"
#include <iostream>
#include <list>
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace framing {

class Buffer;
class FieldValue;

/**
 * Representation of an AMQP 0-10 list
 */
class List
{
  public:
    typedef boost::shared_ptr<FieldValue> ValuePtr;
    typedef std::list<ValuePtr> Values;
    typedef Values::const_iterator const_iterator;
    typedef Values::iterator iterator;
    typedef Values::const_reference const_reference;
    typedef Values::reference reference;

    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN void encode(Buffer& buffer) const;
    QPID_COMMON_EXTERN void decode(Buffer& buffer);

    QPID_COMMON_EXTERN bool operator==(const List& other) const;

    // std collection interface.
    QPID_COMMON_EXTERN const_iterator begin() const { return values.begin(); }
    QPID_COMMON_EXTERN const_iterator end() const { return values.end(); }
    QPID_COMMON_EXTERN iterator begin() { return values.begin(); }
    QPID_COMMON_EXTERN iterator end(){ return values.end(); }

    QPID_COMMON_EXTERN ValuePtr front() const { return values.front(); }
    QPID_COMMON_EXTERN ValuePtr back() const { return values.back(); }
    QPID_COMMON_EXTERN size_t size() const { return values.size(); }

    QPID_COMMON_EXTERN iterator insert(iterator i, ValuePtr value) { return values.insert(i, value); }
    QPID_COMMON_EXTERN void erase(iterator i) { values.erase(i); }
    QPID_COMMON_EXTERN void push_back(ValuePtr value) { values.insert(end(), value); }
    QPID_COMMON_EXTERN void pop_back() { values.pop_back(); }

  private:
    Values values;

    friend QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream& out, const List& list);
};
}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_LIST_H*/
