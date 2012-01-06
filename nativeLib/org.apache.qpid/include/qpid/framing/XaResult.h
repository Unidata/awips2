#ifndef QPID_FRAMING_XARESULT_H
#define QPID_FRAMING_XARESULT_H
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

class XaResult  {
    uint16_t status;
    uint16_t flags;
public:
    static const uint16_t TYPE = 1537;
    XaResult(
        uint16_t _status) : 
        status(_status),
        flags(0){
        flags |= (1 << 8);
    }
    XaResult()  : status(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setStatus(uint16_t _status);
    QPID_COMMON_EXTERN uint16_t getStatus() const;
    QPID_COMMON_EXTERN bool hasStatus() const;
    QPID_COMMON_EXTERN void clearStatusFlag();
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const XaResult&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class XaResult */

}}
#endif  /*!QPID_FRAMING_XARESULT_H*/
