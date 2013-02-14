#ifndef QPID_FRAMING_CLUSTERCONNECTIONANNOUNCEBODY_H
#define QPID_FRAMING_CLUSTERCONNECTIONANNOUNCEBODY_H
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

class ClusterConnectionAnnounceBody : public ModelMethod {
    string managementId;
    uint32_t ssf;
    string authid;
    string username;
    string initialFrames;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x81;
    static const MethodId METHOD_ID = 0x1;
    ClusterConnectionAnnounceBody(
        ProtocolVersion, const string& _managementId,
        uint32_t _ssf,
        const string& _authid,
        bool _nodict,
        const string& _username,
        const string& _initialFrames) : 
        managementId(_managementId),
        ssf(_ssf),
        authid(_authid),
        username(_username),
        initialFrames(_initialFrames),
        flags(0){
        setNodict(_nodict);
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 12);
        flags |= (1 << 13);
    }
    ClusterConnectionAnnounceBody(ProtocolVersion=ProtocolVersion())  : ssf(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setManagementId(const string& _managementId);
    QPID_COMMON_EXTERN const string& getManagementId() const;
    QPID_COMMON_EXTERN bool hasManagementId() const;
    QPID_COMMON_EXTERN void clearManagementIdFlag();
    QPID_COMMON_EXTERN void setSsf(uint32_t _ssf);
    QPID_COMMON_EXTERN uint32_t getSsf() const;
    QPID_COMMON_EXTERN bool hasSsf() const;
    QPID_COMMON_EXTERN void clearSsfFlag();
    QPID_COMMON_EXTERN void setAuthid(const string& _authid);
    QPID_COMMON_EXTERN const string& getAuthid() const;
    QPID_COMMON_EXTERN bool hasAuthid() const;
    QPID_COMMON_EXTERN void clearAuthidFlag();
    QPID_COMMON_EXTERN void setNodict(bool _nodict);
    QPID_COMMON_EXTERN bool getNodict() const;
    QPID_COMMON_EXTERN void setUsername(const string& _username);
    QPID_COMMON_EXTERN const string& getUsername() const;
    QPID_COMMON_EXTERN bool hasUsername() const;
    QPID_COMMON_EXTERN void clearUsernameFlag();
    QPID_COMMON_EXTERN void setInitialFrames(const string& _initialFrames);
    QPID_COMMON_EXTERN const string& getInitialFrames() const;
    QPID_COMMON_EXTERN bool hasInitialFrames() const;
    QPID_COMMON_EXTERN void clearInitialFramesFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.announce(getManagementId(), getSsf(), getAuthid(), getNodict(), getUsername(), getInitialFrames());
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
}; /* class ClusterConnectionAnnounceBody */

}}
#endif  /*!QPID_FRAMING_CLUSTERCONNECTIONANNOUNCEBODY_H*/
