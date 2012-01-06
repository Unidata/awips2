#ifndef QPID_FRAMING_EXCHANGEQUERYRESULT_H
#define QPID_FRAMING_EXCHANGEQUERYRESULT_H
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

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///



#include <ostream>
#include "qpid/framing/amqp_types_full.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class ExchangeQueryResult  {
    string type;
    FieldTable arguments;
    uint16_t flags;
public:
    static const uint16_t TYPE = 1793;
    ExchangeQueryResult(
        const string& _type,
        bool _durable,
        bool _notFound,
        const FieldTable& _arguments) : 
        type(_type),
        arguments(_arguments),
        flags(0){
        setDurable(_durable);
        setNotFound(_notFound);
        flags |= (1 << 8);
        flags |= (1 << 11);
    }
    ExchangeQueryResult()  : flags(0) {}
    
    QPID_COMMON_EXTERN void setType(const string& _type);
    QPID_COMMON_EXTERN const string& getType() const;
    QPID_COMMON_EXTERN bool hasType() const;
    QPID_COMMON_EXTERN void clearTypeFlag();
    QPID_COMMON_EXTERN void setDurable(bool _durable);
    QPID_COMMON_EXTERN bool getDurable() const;
    QPID_COMMON_EXTERN void setNotFound(bool _notFound);
    QPID_COMMON_EXTERN bool getNotFound() const;
    QPID_COMMON_EXTERN void setArguments(const FieldTable& _arguments);
    QPID_COMMON_EXTERN const FieldTable& getArguments() const;
    QPID_COMMON_EXTERN FieldTable& getArguments();
    QPID_COMMON_EXTERN bool hasArguments() const;
    QPID_COMMON_EXTERN void clearArgumentsFlag();
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const ExchangeQueryResult&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class ExchangeQueryResult */

}}
#endif  /*!QPID_FRAMING_EXCHANGEQUERYRESULT_H*/
