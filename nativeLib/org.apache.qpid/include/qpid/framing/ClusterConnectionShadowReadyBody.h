#ifndef QPID_FRAMING_CLUSTERCONNECTIONSHADOWREADYBODY_H
#define QPID_FRAMING_CLUSTERCONNECTIONSHADOWREADYBODY_H
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

class ClusterConnectionShadowReadyBody : public ModelMethod {
    uint64_t memberId;
    uint64_t connectionId;
    string userName;
    string fragment;
    uint32_t sendMax;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x81;
    static const MethodId METHOD_ID = 0x20;
    ClusterConnectionShadowReadyBody(
        ProtocolVersion, uint64_t _memberId,
        uint64_t _connectionId,
        const string& _userName,
        const string& _fragment,
        uint32_t _sendMax) : 
        memberId(_memberId),
        connectionId(_connectionId),
        userName(_userName),
        fragment(_fragment),
        sendMax(_sendMax),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
        flags |= (1 << 12);
    }
    ClusterConnectionShadowReadyBody(ProtocolVersion=ProtocolVersion())  : memberId(0), connectionId(0), sendMax(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setMemberId(uint64_t _memberId);
    QPID_COMMON_EXTERN uint64_t getMemberId() const;
    QPID_COMMON_EXTERN bool hasMemberId() const;
    QPID_COMMON_EXTERN void clearMemberIdFlag();
    QPID_COMMON_EXTERN void setConnectionId(uint64_t _connectionId);
    QPID_COMMON_EXTERN uint64_t getConnectionId() const;
    QPID_COMMON_EXTERN bool hasConnectionId() const;
    QPID_COMMON_EXTERN void clearConnectionIdFlag();
    QPID_COMMON_EXTERN void setUserName(const string& _userName);
    QPID_COMMON_EXTERN const string& getUserName() const;
    QPID_COMMON_EXTERN bool hasUserName() const;
    QPID_COMMON_EXTERN void clearUserNameFlag();
    QPID_COMMON_EXTERN void setFragment(const string& _fragment);
    QPID_COMMON_EXTERN const string& getFragment() const;
    QPID_COMMON_EXTERN bool hasFragment() const;
    QPID_COMMON_EXTERN void clearFragmentFlag();
    QPID_COMMON_EXTERN void setSendMax(uint32_t _sendMax);
    QPID_COMMON_EXTERN uint32_t getSendMax() const;
    QPID_COMMON_EXTERN bool hasSendMax() const;
    QPID_COMMON_EXTERN void clearSendMaxFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.shadowReady(getMemberId(), getConnectionId(), getUserName(), getFragment(), getSendMax());
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
}; /* class ClusterConnectionShadowReadyBody */

}}
#endif  /*!QPID_FRAMING_CLUSTERCONNECTIONSHADOWREADYBODY_H*/
