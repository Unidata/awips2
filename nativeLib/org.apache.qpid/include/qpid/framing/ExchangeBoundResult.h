#ifndef QPID_FRAMING_EXCHANGEBOUNDRESULT_H
#define QPID_FRAMING_EXCHANGEBOUNDRESULT_H
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

class ExchangeBoundResult  {
    uint16_t flags;
public:
    static const uint16_t TYPE = 1794;
    ExchangeBoundResult(
        bool _exchangeNotFound,
        bool _queueNotFound,
        bool _queueNotMatched,
        bool _keyNotMatched,
        bool _argsNotMatched) : 
        flags(0){
        setExchangeNotFound(_exchangeNotFound);
        setQueueNotFound(_queueNotFound);
        setQueueNotMatched(_queueNotMatched);
        setKeyNotMatched(_keyNotMatched);
        setArgsNotMatched(_argsNotMatched);
    }
    ExchangeBoundResult()  : flags(0) {}
    
    QPID_COMMON_EXTERN void setExchangeNotFound(bool _exchangeNotFound);
    QPID_COMMON_EXTERN bool getExchangeNotFound() const;
    QPID_COMMON_EXTERN void setQueueNotFound(bool _queueNotFound);
    QPID_COMMON_EXTERN bool getQueueNotFound() const;
    QPID_COMMON_EXTERN void setQueueNotMatched(bool _queueNotMatched);
    QPID_COMMON_EXTERN bool getQueueNotMatched() const;
    QPID_COMMON_EXTERN void setKeyNotMatched(bool _keyNotMatched);
    QPID_COMMON_EXTERN bool getKeyNotMatched() const;
    QPID_COMMON_EXTERN void setArgsNotMatched(bool _argsNotMatched);
    QPID_COMMON_EXTERN bool getArgsNotMatched() const;
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const ExchangeBoundResult&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class ExchangeBoundResult */

}}
#endif  /*!QPID_FRAMING_EXCHANGEBOUNDRESULT_H*/
