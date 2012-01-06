#ifndef QPID_FRAMING_XID_H
#define QPID_FRAMING_XID_H
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

class Xid  {
    uint32_t format;
    string globalId;
    string branchId;
    uint16_t flags;
public:
    static const uint16_t TYPE = 1540;
    Xid(
        uint32_t _format,
        const string& _globalId,
        const string& _branchId) : 
        format(_format),
        globalId(_globalId),
        branchId(_branchId),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
    }
    Xid()  : format(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setFormat(uint32_t _format);
    QPID_COMMON_EXTERN uint32_t getFormat() const;
    QPID_COMMON_EXTERN bool hasFormat() const;
    QPID_COMMON_EXTERN void clearFormatFlag();
    QPID_COMMON_EXTERN void setGlobalId(const string& _globalId);
    QPID_COMMON_EXTERN const string& getGlobalId() const;
    QPID_COMMON_EXTERN bool hasGlobalId() const;
    QPID_COMMON_EXTERN void clearGlobalIdFlag();
    QPID_COMMON_EXTERN void setBranchId(const string& _branchId);
    QPID_COMMON_EXTERN const string& getBranchId() const;
    QPID_COMMON_EXTERN bool hasBranchId() const;
    QPID_COMMON_EXTERN void clearBranchIdFlag();
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const Xid&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class Xid */

}}
#endif  /*!QPID_FRAMING_XID_H*/
