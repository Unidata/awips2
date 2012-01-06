#ifndef QPID_FRAMING_CONNECTIONTUNEOKBODY_H
#define QPID_FRAMING_CONNECTIONTUNEOKBODY_H
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

class ConnectionTuneOkBody : public AMQMethodBody {
    uint16_t channelMax;
    uint16_t maxFrameSize;
    uint16_t heartbeat;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x1;
    static const MethodId METHOD_ID = 0x6;
    ConnectionTuneOkBody(
        ProtocolVersion, uint16_t _channelMax,
        uint16_t _maxFrameSize,
        uint16_t _heartbeat) : 
        channelMax(_channelMax),
        maxFrameSize(_maxFrameSize),
        heartbeat(_heartbeat),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
    }
    ConnectionTuneOkBody(ProtocolVersion=ProtocolVersion())  : channelMax(0), maxFrameSize(0), heartbeat(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setChannelMax(uint16_t _channelMax);
    QPID_COMMON_EXTERN uint16_t getChannelMax() const;
    QPID_COMMON_EXTERN bool hasChannelMax() const;
    QPID_COMMON_EXTERN void clearChannelMaxFlag();
    QPID_COMMON_EXTERN void setMaxFrameSize(uint16_t _maxFrameSize);
    QPID_COMMON_EXTERN uint16_t getMaxFrameSize() const;
    QPID_COMMON_EXTERN bool hasMaxFrameSize() const;
    QPID_COMMON_EXTERN void clearMaxFrameSizeFlag();
    QPID_COMMON_EXTERN void setHeartbeat(uint16_t _heartbeat);
    QPID_COMMON_EXTERN uint16_t getHeartbeat() const;
    QPID_COMMON_EXTERN bool hasHeartbeat() const;
    QPID_COMMON_EXTERN void clearHeartbeatFlag();
virtual uint8_t type() const { return 0;/*control segment*/ }
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.tuneOk(getChannelMax(), getMaxFrameSize(), getHeartbeat());
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
}; /* class ConnectionTuneOkBody */

}}
#endif  /*!QPID_FRAMING_CONNECTIONTUNEOKBODY_H*/
