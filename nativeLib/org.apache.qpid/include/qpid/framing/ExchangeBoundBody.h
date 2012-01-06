#ifndef QPID_FRAMING_EXCHANGEBOUNDBODY_H
#define QPID_FRAMING_EXCHANGEBOUNDBODY_H
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

class ExchangeBoundBody : public ModelMethod {
    string exchange;
    string queue;
    string bindingKey;
    FieldTable arguments;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x7;
    static const MethodId METHOD_ID = 0x6;
    ExchangeBoundBody(
        ProtocolVersion, const string& _exchange,
        const string& _queue,
        const string& _bindingKey,
        const FieldTable& _arguments) : 
        exchange(_exchange),
        queue(_queue),
        bindingKey(_bindingKey),
        arguments(_arguments),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
    }
    ExchangeBoundBody(ProtocolVersion=ProtocolVersion())  : flags(0) {}
    
    QPID_COMMON_EXTERN void setExchange(const string& _exchange);
    QPID_COMMON_EXTERN const string& getExchange() const;
    QPID_COMMON_EXTERN bool hasExchange() const;
    QPID_COMMON_EXTERN void clearExchangeFlag();
    QPID_COMMON_EXTERN void setQueue(const string& _queue);
    QPID_COMMON_EXTERN const string& getQueue() const;
    QPID_COMMON_EXTERN bool hasQueue() const;
    QPID_COMMON_EXTERN void clearQueueFlag();
    QPID_COMMON_EXTERN void setBindingKey(const string& _bindingKey);
    QPID_COMMON_EXTERN const string& getBindingKey() const;
    QPID_COMMON_EXTERN bool hasBindingKey() const;
    QPID_COMMON_EXTERN void clearBindingKeyFlag();
    QPID_COMMON_EXTERN void setArguments(const FieldTable& _arguments);
    QPID_COMMON_EXTERN const FieldTable& getArguments() const;
    QPID_COMMON_EXTERN FieldTable& getArguments();
    QPID_COMMON_EXTERN bool hasArguments() const;
    QPID_COMMON_EXTERN void clearArgumentsFlag();
    typedef ExchangeBoundResult ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.bound(getExchange(), getQueue(), getBindingKey(), getArguments());
    }

    using  AMQMethodBody::accept;
    void accept(MethodBodyConstVisitor& v) const { v.visit(*this); }
    boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }

    ClassId amqpClassId() const { return CLASS_ID; }
    MethodId amqpMethodId() const { return METHOD_ID; }
    bool isContentBearing() const { return  false; }
    bool resultExpected() const { return  true; }
    bool responseExpected() const { return  false; }
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class ExchangeBoundBody */

}}
#endif  /*!QPID_FRAMING_EXCHANGEBOUNDBODY_H*/
