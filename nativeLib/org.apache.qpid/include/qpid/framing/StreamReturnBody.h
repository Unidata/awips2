#ifndef QPID_FRAMING_STREAMRETURNBODY_H
#define QPID_FRAMING_STREAMRETURNBODY_H
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
#include "qpid/framing/ModelMethod.h"

#include <ostream>
#include "qpid/framing/amqp_types_full.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class StreamReturnBody : public ModelMethod {
    uint16_t replyCode;
    string replyText;
    string exchange;
    string routingKey;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0xa;
    static const MethodId METHOD_ID = 0x7;
    StreamReturnBody(
        ProtocolVersion, uint16_t _replyCode,
        const string& _replyText,
        const string& _exchange,
        const string& _routingKey) : 
        replyCode(_replyCode),
        replyText(_replyText),
        exchange(_exchange),
        routingKey(_routingKey),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
    }
    StreamReturnBody(ProtocolVersion=ProtocolVersion())  : replyCode(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setReplyCode(uint16_t _replyCode);
    QPID_COMMON_EXTERN uint16_t getReplyCode() const;
    QPID_COMMON_EXTERN bool hasReplyCode() const;
    QPID_COMMON_EXTERN void clearReplyCodeFlag();
    QPID_COMMON_EXTERN void setReplyText(const string& _replyText);
    QPID_COMMON_EXTERN const string& getReplyText() const;
    QPID_COMMON_EXTERN bool hasReplyText() const;
    QPID_COMMON_EXTERN void clearReplyTextFlag();
    QPID_COMMON_EXTERN void setExchange(const string& _exchange);
    QPID_COMMON_EXTERN const string& getExchange() const;
    QPID_COMMON_EXTERN bool hasExchange() const;
    QPID_COMMON_EXTERN void clearExchangeFlag();
    QPID_COMMON_EXTERN void setRoutingKey(const string& _routingKey);
    QPID_COMMON_EXTERN const string& getRoutingKey() const;
    QPID_COMMON_EXTERN bool hasRoutingKey() const;
    QPID_COMMON_EXTERN void clearRoutingKeyFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.return_(getReplyCode(), getReplyText(), getExchange(), getRoutingKey());
    }

    using  AMQMethodBody::accept;
    void accept(MethodBodyConstVisitor& v) const { v.visit(*this); }
    boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }

    ClassId amqpClassId() const { return CLASS_ID; }
    MethodId amqpMethodId() const { return METHOD_ID; }
    bool isContentBearing() const { return  true; }
    bool resultExpected() const { return  false; }
    bool responseExpected() const { return  false; }
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class StreamReturnBody */

}}
#endif  /*!QPID_FRAMING_STREAMRETURNBODY_H*/
