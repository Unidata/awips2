#ifndef QPID_FRAMING_FRAGMENTPROPERTIES_H
#define QPID_FRAMING_FRAGMENTPROPERTIES_H
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

class FragmentProperties  {
    uint64_t fragmentSize;
    uint16_t flags;
public:
    static const uint16_t TYPE = 1026;
    FragmentProperties(
        bool _first,
        bool _last,
        uint64_t _fragmentSize) : 
        fragmentSize(_fragmentSize),
        flags(0){
        setFirst(_first);
        setLast(_last);
        flags |= (1 << 10);
    }
    FragmentProperties()  : fragmentSize(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setFirst(bool _first);
    QPID_COMMON_EXTERN bool getFirst() const;
    QPID_COMMON_EXTERN void setLast(bool _last);
    QPID_COMMON_EXTERN bool getLast() const;
    QPID_COMMON_EXTERN void setFragmentSize(uint64_t _fragmentSize);
    QPID_COMMON_EXTERN uint64_t getFragmentSize() const;
    QPID_COMMON_EXTERN bool hasFragmentSize() const;
    QPID_COMMON_EXTERN void clearFragmentSizeFlag();
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const FragmentProperties&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class FragmentProperties */

}}
#endif  /*!QPID_FRAMING_FRAGMENTPROPERTIES_H*/
