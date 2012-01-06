#ifndef QPID_FRAMING_EXCHANGEDECLAREBODY_H
#define QPID_FRAMING_EXCHANGEDECLAREBODY_H
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

class ExchangeDeclareBody : public ModelMethod {
    string exchange;
    string type;
    string alternateExchange;
    FieldTable arguments;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x7;
    static const MethodId METHOD_ID = 0x1;
    ExchangeDeclareBody(
        ProtocolVersion, const string& _exchange,
        const string& _type,
        const string& _alternateExchange,
        bool _passive,
        bool _durable,
        bool _autoDelete,
        const FieldTable& _arguments) : 
        exchange(_exchange),
        type(_type),
        alternateExchange(_alternateExchange),
        arguments(_arguments),
        flags(0){
        setPassive(_passive);
        setDurable(_durable);
        setAutoDelete(_autoDelete);
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 14);
    }
    ExchangeDeclareBody(ProtocolVersion=ProtocolVersion())  : flags(0) {}
    
    QPID_COMMON_EXTERN void setExchange(const string& _exchange);
    QPID_COMMON_EXTERN const string& getExchange() const;
    QPID_COMMON_EXTERN bool hasExchange() const;
    QPID_COMMON_EXTERN void clearExchangeFlag();
    QPID_COMMON_EXTERN void setType(const string& _type);
    QPID_COMMON_EXTERN const string& getType() const;
    QPID_COMMON_EXTERN bool hasType() const;
    QPID_COMMON_EXTERN void clearTypeFlag();
    QPID_COMMON_EXTERN void setAlternateExchange(const string& _alternateExchange);
    QPID_COMMON_EXTERN const string& getAlternateExchange() const;
    QPID_COMMON_EXTERN bool hasAlternateExchange() const;
    QPID_COMMON_EXTERN void clearAlternateExchangeFlag();
    QPID_COMMON_EXTERN void setPassive(bool _passive);
    QPID_COMMON_EXTERN bool getPassive() const;
    QPID_COMMON_EXTERN void setDurable(bool _durable);
    QPID_COMMON_EXTERN bool getDurable() const;
    QPID_COMMON_EXTERN void setAutoDelete(bool _autoDelete);
    QPID_COMMON_EXTERN bool getAutoDelete() const;
    QPID_COMMON_EXTERN void setArguments(const FieldTable& _arguments);
    QPID_COMMON_EXTERN const FieldTable& getArguments() const;
    QPID_COMMON_EXTERN FieldTable& getArguments();
    QPID_COMMON_EXTERN bool hasArguments() const;
    QPID_COMMON_EXTERN void clearArgumentsFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.declare(getExchange(), getType(), getAlternateExchange(), getPassive(), getDurable(), getAutoDelete(), getArguments());
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
}; /* class ExchangeDeclareBody */

}}
#endif  /*!QPID_FRAMING_EXCHANGEDECLAREBODY_H*/
