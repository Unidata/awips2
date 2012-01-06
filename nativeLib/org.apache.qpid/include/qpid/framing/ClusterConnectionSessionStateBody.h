#ifndef QPID_FRAMING_CLUSTERCONNECTIONSESSIONSTATEBODY_H
#define QPID_FRAMING_CLUSTERCONNECTIONSESSIONSTATEBODY_H
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

class ClusterConnectionSessionStateBody : public ModelMethod {
    SequenceNumber replayStart;
    SequenceNumber commandPoint;
    SequenceSet sentIncomplete;
    SequenceNumber expected;
    SequenceNumber received;
    SequenceSet unknownCompleted;
    SequenceSet receivedIncomplete;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x81;
    static const MethodId METHOD_ID = 0x1F;
    ClusterConnectionSessionStateBody(
        ProtocolVersion, const SequenceNumber& _replayStart,
        const SequenceNumber& _commandPoint,
        const SequenceSet& _sentIncomplete,
        const SequenceNumber& _expected,
        const SequenceNumber& _received,
        const SequenceSet& _unknownCompleted,
        const SequenceSet& _receivedIncomplete) : 
        replayStart(_replayStart),
        commandPoint(_commandPoint),
        sentIncomplete(_sentIncomplete),
        expected(_expected),
        received(_received),
        unknownCompleted(_unknownCompleted),
        receivedIncomplete(_receivedIncomplete),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
        flags |= (1 << 12);
        flags |= (1 << 13);
        flags |= (1 << 14);
    }
    ClusterConnectionSessionStateBody(ProtocolVersion=ProtocolVersion())  : flags(0) {}
    
    QPID_COMMON_EXTERN void setReplayStart(const SequenceNumber& _replayStart);
    QPID_COMMON_EXTERN SequenceNumber getReplayStart() const;
    QPID_COMMON_EXTERN bool hasReplayStart() const;
    QPID_COMMON_EXTERN void clearReplayStartFlag();
    QPID_COMMON_EXTERN void setCommandPoint(const SequenceNumber& _commandPoint);
    QPID_COMMON_EXTERN SequenceNumber getCommandPoint() const;
    QPID_COMMON_EXTERN bool hasCommandPoint() const;
    QPID_COMMON_EXTERN void clearCommandPointFlag();
    QPID_COMMON_EXTERN void setSentIncomplete(const SequenceSet& _sentIncomplete);
    QPID_COMMON_EXTERN const SequenceSet& getSentIncomplete() const;
    QPID_COMMON_EXTERN bool hasSentIncomplete() const;
    QPID_COMMON_EXTERN void clearSentIncompleteFlag();
    QPID_COMMON_EXTERN void setExpected(const SequenceNumber& _expected);
    QPID_COMMON_EXTERN SequenceNumber getExpected() const;
    QPID_COMMON_EXTERN bool hasExpected() const;
    QPID_COMMON_EXTERN void clearExpectedFlag();
    QPID_COMMON_EXTERN void setReceived(const SequenceNumber& _received);
    QPID_COMMON_EXTERN SequenceNumber getReceived() const;
    QPID_COMMON_EXTERN bool hasReceived() const;
    QPID_COMMON_EXTERN void clearReceivedFlag();
    QPID_COMMON_EXTERN void setUnknownCompleted(const SequenceSet& _unknownCompleted);
    QPID_COMMON_EXTERN const SequenceSet& getUnknownCompleted() const;
    QPID_COMMON_EXTERN bool hasUnknownCompleted() const;
    QPID_COMMON_EXTERN void clearUnknownCompletedFlag();
    QPID_COMMON_EXTERN void setReceivedIncomplete(const SequenceSet& _receivedIncomplete);
    QPID_COMMON_EXTERN const SequenceSet& getReceivedIncomplete() const;
    QPID_COMMON_EXTERN bool hasReceivedIncomplete() const;
    QPID_COMMON_EXTERN void clearReceivedIncompleteFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.sessionState(getReplayStart(), getCommandPoint(), getSentIncomplete(), getExpected(), getReceived(), getUnknownCompleted(), getReceivedIncomplete());
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
}; /* class ClusterConnectionSessionStateBody */

}}
#endif  /*!QPID_FRAMING_CLUSTERCONNECTIONSESSIONSTATEBODY_H*/
