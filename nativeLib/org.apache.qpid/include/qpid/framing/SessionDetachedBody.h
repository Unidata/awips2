#ifndef QPID_FRAMING_SESSIONDETACHEDBODY_H
#define QPID_FRAMING_SESSIONDETACHEDBODY_H
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


#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/AMQP_ServerOperations.h"
#include "qpid/framing/MethodBodyConstVisitor.h"

#include <ostream>
#include "qpid/framing/amqp_types_full.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class SessionDetachedBody : public AMQMethodBody {
    string name;
    uint8_t code;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x2;
    static const MethodId METHOD_ID = 0x4;
    SessionDetachedBody(
        ProtocolVersion, const string& _name,
        uint8_t _code) : 
        name(_name),
        code(_code),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
    }
    SessionDetachedBody(ProtocolVersion=ProtocolVersion())  : code(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setName(const string& _name);
    QPID_COMMON_EXTERN const string& getName() const;
    QPID_COMMON_EXTERN bool hasName() const;
    QPID_COMMON_EXTERN void clearNameFlag();
    QPID_COMMON_EXTERN void setCode(uint8_t _code);
    QPID_COMMON_EXTERN uint8_t getCode() const;
    QPID_COMMON_EXTERN bool hasCode() const;
    QPID_COMMON_EXTERN void clearCodeFlag();
virtual uint8_t type() const { return 0;/*control segment*/ }
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.detached(getName(), getCode());
    }

    using  AMQMethodBody::accept;
    void accept(MethodBodyConstVisitor& v) const { v.visit(*this); }
    boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }

    ClassId amqpClassId() const { return CLASS_ID; }
    MethodId amqpMethodId() const { return METHOD_ID; }
    bool isContentBearing() const { return  false; }
    bool resultExpected() const { return  false; }
    bool responseExpected() const { return  false; }
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class SessionDetachedBody */

}}
#endif  /*!QPID_FRAMING_SESSIONDETACHEDBODY_H*/
