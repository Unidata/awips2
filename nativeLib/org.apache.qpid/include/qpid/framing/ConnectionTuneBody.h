#ifndef QPID_FRAMING_CONNECTIONTUNEBODY_H
#define QPID_FRAMING_CONNECTIONTUNEBODY_H
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

class ConnectionTuneBody : public AMQMethodBody {
    uint16_t channelMax;
    uint16_t maxFrameSize;
    uint16_t heartbeatMin;
    uint16_t heartbeatMax;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x1;
    static const MethodId METHOD_ID = 0x5;
    ConnectionTuneBody(
        ProtocolVersion, uint16_t _channelMax,
        uint16_t _maxFrameSize,
        uint16_t _heartbeatMin,
        uint16_t _heartbeatMax) : 
        channelMax(_channelMax),
        maxFrameSize(_maxFrameSize),
        heartbeatMin(_heartbeatMin),
        heartbeatMax(_heartbeatMax),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
    }
    ConnectionTuneBody(ProtocolVersion=ProtocolVersion())  : channelMax(0), maxFrameSize(0), heartbeatMin(0), heartbeatMax(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setChannelMax(uint16_t _channelMax);
    QPID_COMMON_EXTERN uint16_t getChannelMax() const;
    QPID_COMMON_EXTERN bool hasChannelMax() const;
    QPID_COMMON_EXTERN void clearChannelMaxFlag();
    QPID_COMMON_EXTERN void setMaxFrameSize(uint16_t _maxFrameSize);
    QPID_COMMON_EXTERN uint16_t getMaxFrameSize() const;
    QPID_COMMON_EXTERN bool hasMaxFrameSize() const;
    QPID_COMMON_EXTERN void clearMaxFrameSizeFlag();
    QPID_COMMON_EXTERN void setHeartbeatMin(uint16_t _heartbeatMin);
    QPID_COMMON_EXTERN uint16_t getHeartbeatMin() const;
    QPID_COMMON_EXTERN bool hasHeartbeatMin() const;
    QPID_COMMON_EXTERN void clearHeartbeatMinFlag();
    QPID_COMMON_EXTERN void setHeartbeatMax(uint16_t _heartbeatMax);
    QPID_COMMON_EXTERN uint16_t getHeartbeatMax() const;
    QPID_COMMON_EXTERN bool hasHeartbeatMax() const;
    QPID_COMMON_EXTERN void clearHeartbeatMaxFlag();
virtual uint8_t type() const { return 0;/*control segment*/ }
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.tune(getChannelMax(), getMaxFrameSize(), getHeartbeatMin(), getHeartbeatMax());
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
}; /* class ConnectionTuneBody */

}}
#endif  /*!QPID_FRAMING_CONNECTIONTUNEBODY_H*/
