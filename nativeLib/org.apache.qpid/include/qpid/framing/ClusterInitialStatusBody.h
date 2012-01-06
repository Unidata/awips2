#ifndef QPID_FRAMING_CLUSTERINITIALSTATUSBODY_H
#define QPID_FRAMING_CLUSTERINITIALSTATUSBODY_H
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

class ClusterInitialStatusBody : public ModelMethod {
    uint32_t version;
    Uuid clusterId;
    uint8_t storeState;
    Uuid shutdownId;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x80;
    static const MethodId METHOD_ID = 0x5;
    ClusterInitialStatusBody(
        ProtocolVersion, uint32_t _version,
        bool _active,
        const Uuid& _clusterId,
        uint8_t _storeState,
        const Uuid& _shutdownId) : 
        version(_version),
        clusterId(_clusterId),
        storeState(_storeState),
        shutdownId(_shutdownId),
        flags(0){
        setActive(_active);
        flags |= (1 << 8);
        flags |= (1 << 10);
        flags |= (1 << 11);
        flags |= (1 << 12);
    }
    ClusterInitialStatusBody(ProtocolVersion=ProtocolVersion())  : version(0), storeState(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setVersion(uint32_t _version);
    QPID_COMMON_EXTERN uint32_t getVersion() const;
    QPID_COMMON_EXTERN bool hasVersion() const;
    QPID_COMMON_EXTERN void clearVersionFlag();
    QPID_COMMON_EXTERN void setActive(bool _active);
    QPID_COMMON_EXTERN bool getActive() const;
    QPID_COMMON_EXTERN void setClusterId(const Uuid& _clusterId);
    QPID_COMMON_EXTERN const Uuid& getClusterId() const;
    QPID_COMMON_EXTERN bool hasClusterId() const;
    QPID_COMMON_EXTERN void clearClusterIdFlag();
    QPID_COMMON_EXTERN void setStoreState(uint8_t _storeState);
    QPID_COMMON_EXTERN uint8_t getStoreState() const;
    QPID_COMMON_EXTERN bool hasStoreState() const;
    QPID_COMMON_EXTERN void clearStoreStateFlag();
    QPID_COMMON_EXTERN void setShutdownId(const Uuid& _shutdownId);
    QPID_COMMON_EXTERN const Uuid& getShutdownId() const;
    QPID_COMMON_EXTERN bool hasShutdownId() const;
    QPID_COMMON_EXTERN void clearShutdownIdFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.initialStatus(getVersion(), getActive(), getClusterId(), getStoreState(), getShutdownId());
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
}; /* class ClusterInitialStatusBody */

}}
#endif  /*!QPID_FRAMING_CLUSTERINITIALSTATUSBODY_H*/
