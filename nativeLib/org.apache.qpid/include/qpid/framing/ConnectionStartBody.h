#ifndef QPID_FRAMING_CONNECTIONSTARTBODY_H
#define QPID_FRAMING_CONNECTIONSTARTBODY_H
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

class ConnectionStartBody : public AMQMethodBody {
    FieldTable serverProperties;
    Array mechanisms;
    Array locales;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x1;
    static const MethodId METHOD_ID = 0x1;
    ConnectionStartBody(
        ProtocolVersion, const FieldTable& _serverProperties,
        const Array& _mechanisms,
        const Array& _locales) : 
        serverProperties(_serverProperties),
        mechanisms(_mechanisms),
        locales(_locales),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
    }
    ConnectionStartBody(ProtocolVersion=ProtocolVersion())  : flags(0) {}
    
    QPID_COMMON_EXTERN void setServerProperties(const FieldTable& _serverProperties);
    QPID_COMMON_EXTERN const FieldTable& getServerProperties() const;
    QPID_COMMON_EXTERN FieldTable& getServerProperties();
    QPID_COMMON_EXTERN bool hasServerProperties() const;
    QPID_COMMON_EXTERN void clearServerPropertiesFlag();
    QPID_COMMON_EXTERN void setMechanisms(const Array& _mechanisms);
    QPID_COMMON_EXTERN const Array& getMechanisms() const;
    QPID_COMMON_EXTERN bool hasMechanisms() const;
    QPID_COMMON_EXTERN void clearMechanismsFlag();
    QPID_COMMON_EXTERN void setLocales(const Array& _locales);
    QPID_COMMON_EXTERN const Array& getLocales() const;
    QPID_COMMON_EXTERN bool hasLocales() const;
    QPID_COMMON_EXTERN void clearLocalesFlag();
virtual uint8_t type() const { return 0;/*control segment*/ }
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.start(getServerProperties(), getMechanisms(), getLocales());
    }

    using  AMQMethodBody::accept;
    void accept(MethodBodyConstVisitor& v) const { v.visit(*this); }
    boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }

    ClassId amqpClassId() const { return CLASS_ID; }
    MethodId amqpMethodId() const { return METHOD_ID; }
    bool isContentBearing() const { return  false; }
    bool resultExpected() const { return  false; }
    bool responseExpected() const { return  true; }
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class ConnectionStartBody */

}}
#endif  /*!QPID_FRAMING_CONNECTIONSTARTBODY_H*/
