#ifndef QPID_FRAMING_CLUSTERCONNECTIONDELIVERYRECORDBODY_H
#define QPID_FRAMING_CLUSTERCONNECTIONDELIVERYRECORDBODY_H
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

class ClusterConnectionDeliveryRecordBody : public ModelMethod {
    string queue;
    SequenceNumber position;
    string tag;
    SequenceNumber id;
    uint32_t credit;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x81;
    static const MethodId METHOD_ID = 0x11;
    ClusterConnectionDeliveryRecordBody(
        ProtocolVersion, const string& _queue,
        const SequenceNumber& _position,
        const string& _tag,
        const SequenceNumber& _id,
        bool _acquired,
        bool _accepted,
        bool _cancelled,
        bool _completed,
        bool _ended,
        bool _windowing,
        bool _enqueued,
        uint32_t _credit) : 
        queue(_queue),
        position(_position),
        tag(_tag),
        id(_id),
        credit(_credit),
        flags(0){
        setAcquired(_acquired);
        setAccepted(_accepted);
        setCancelled(_cancelled);
        setCompleted(_completed);
        setEnded(_ended);
        setWindowing(_windowing);
        setEnqueued(_enqueued);
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
        flags |= (1 << 3);
    }
    ClusterConnectionDeliveryRecordBody(ProtocolVersion=ProtocolVersion())  : credit(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setQueue(const string& _queue);
    QPID_COMMON_EXTERN const string& getQueue() const;
    QPID_COMMON_EXTERN bool hasQueue() const;
    QPID_COMMON_EXTERN void clearQueueFlag();
    QPID_COMMON_EXTERN void setPosition(const SequenceNumber& _position);
    QPID_COMMON_EXTERN SequenceNumber getPosition() const;
    QPID_COMMON_EXTERN bool hasPosition() const;
    QPID_COMMON_EXTERN void clearPositionFlag();
    QPID_COMMON_EXTERN void setTag(const string& _tag);
    QPID_COMMON_EXTERN const string& getTag() const;
    QPID_COMMON_EXTERN bool hasTag() const;
    QPID_COMMON_EXTERN void clearTagFlag();
    QPID_COMMON_EXTERN void setId(const SequenceNumber& _id);
    QPID_COMMON_EXTERN SequenceNumber getId() const;
    QPID_COMMON_EXTERN bool hasId() const;
    QPID_COMMON_EXTERN void clearIdFlag();
    QPID_COMMON_EXTERN void setAcquired(bool _acquired);
    QPID_COMMON_EXTERN bool getAcquired() const;
    QPID_COMMON_EXTERN void setAccepted(bool _accepted);
    QPID_COMMON_EXTERN bool getAccepted() const;
    QPID_COMMON_EXTERN void setCancelled(bool _cancelled);
    QPID_COMMON_EXTERN bool getCancelled() const;
    QPID_COMMON_EXTERN void setCompleted(bool _completed);
    QPID_COMMON_EXTERN bool getCompleted() const;
    QPID_COMMON_EXTERN void setEnded(bool _ended);
    QPID_COMMON_EXTERN bool getEnded() const;
    QPID_COMMON_EXTERN void setWindowing(bool _windowing);
    QPID_COMMON_EXTERN bool getWindowing() const;
    QPID_COMMON_EXTERN void setEnqueued(bool _enqueued);
    QPID_COMMON_EXTERN bool getEnqueued() const;
    QPID_COMMON_EXTERN void setCredit(uint32_t _credit);
    QPID_COMMON_EXTERN uint32_t getCredit() const;
    QPID_COMMON_EXTERN bool hasCredit() const;
    QPID_COMMON_EXTERN void clearCreditFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.deliveryRecord(getQueue(), getPosition(), getTag(), getId(), getAcquired(), getAccepted(), getCancelled(), getCompleted(), getEnded(), getWindowing(), getEnqueued(), getCredit());
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
}; /* class ClusterConnectionDeliveryRecordBody */

}}
#endif  /*!QPID_FRAMING_CLUSTERCONNECTIONDELIVERYRECORDBODY_H*/
