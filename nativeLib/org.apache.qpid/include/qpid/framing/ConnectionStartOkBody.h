#ifndef QPID_FRAMING_CONNECTIONSTARTOKBODY_H
#define QPID_FRAMING_CONNECTIONSTARTOKBODY_H
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

class ConnectionStartOkBody : public AMQMethodBody {
    FieldTable clientProperties;
    string mechanism;
    string response;
    string locale;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x1;
    static const MethodId METHOD_ID = 0x2;
    ConnectionStartOkBody(
        ProtocolVersion, const FieldTable& _clientProperties,
        const string& _mechanism,
        const string& _response,
        const string& _locale) : 
        clientProperties(_clientProperties),
        mechanism(_mechanism),
        response(_response),
        locale(_locale),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
    }
    ConnectionStartOkBody(ProtocolVersion=ProtocolVersion())  : flags(0) {}
    
    QPID_COMMON_EXTERN void setClientProperties(const FieldTable& _clientProperties);
    QPID_COMMON_EXTERN const FieldTable& getClientProperties() const;
    QPID_COMMON_EXTERN FieldTable& getClientProperties();
    QPID_COMMON_EXTERN bool hasClientProperties() const;
    QPID_COMMON_EXTERN void clearClientPropertiesFlag();
    QPID_COMMON_EXTERN void setMechanism(const string& _mechanism);
    QPID_COMMON_EXTERN const string& getMechanism() const;
    QPID_COMMON_EXTERN bool hasMechanism() const;
    QPID_COMMON_EXTERN void clearMechanismFlag();
    QPID_COMMON_EXTERN void setResponse(const string& _response);
    QPID_COMMON_EXTERN const string& getResponse() const;
    QPID_COMMON_EXTERN bool hasResponse() const;
    QPID_COMMON_EXTERN void clearResponseFlag();
    QPID_COMMON_EXTERN void setLocale(const string& _locale);
    QPID_COMMON_EXTERN const string& getLocale() const;
    QPID_COMMON_EXTERN bool hasLocale() const;
    QPID_COMMON_EXTERN void clearLocaleFlag();
virtual uint8_t type() const { return 0;/*control segment*/ }
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.startOk(getClientProperties(), getMechanism(), getResponse(), getLocale());
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
}; /* class ConnectionStartOkBody */

}}
#endif  /*!QPID_FRAMING_CONNECTIONSTARTOKBODY_H*/
