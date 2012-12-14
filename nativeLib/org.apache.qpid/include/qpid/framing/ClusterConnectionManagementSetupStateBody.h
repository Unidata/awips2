#ifndef QPID_FRAMING_CLUSTERCONNECTIONMANAGEMENTSETUPSTATEBODY_H
#define QPID_FRAMING_CLUSTERCONNECTIONMANAGEMENTSETUPSTATEBODY_H
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

class ClusterConnectionManagementSetupStateBody : public ModelMethod {
    uint64_t objectNum;
    uint16_t bootSequence;
    Uuid brokerId;
    string vendor;
    string product;
    string instance;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x81;
    static const MethodId METHOD_ID = 0x36;
    ClusterConnectionManagementSetupStateBody(
        ProtocolVersion, uint64_t _objectNum,
        uint16_t _bootSequence,
        const Uuid& _brokerId,
        const string& _vendor,
        const string& _product,
        const string& _instance) : 
        objectNum(_objectNum),
        bootSequence(_bootSequence),
        brokerId(_brokerId),
        vendor(_vendor),
        product(_product),
        instance(_instance),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
        flags |= (1 << 12);
        flags |= (1 << 13);
    }
    ClusterConnectionManagementSetupStateBody(ProtocolVersion=ProtocolVersion())  : objectNum(0), bootSequence(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setObjectNum(uint64_t _objectNum);
    QPID_COMMON_EXTERN uint64_t getObjectNum() const;
    QPID_COMMON_EXTERN bool hasObjectNum() const;
    QPID_COMMON_EXTERN void clearObjectNumFlag();
    QPID_COMMON_EXTERN void setBootSequence(uint16_t _bootSequence);
    QPID_COMMON_EXTERN uint16_t getBootSequence() const;
    QPID_COMMON_EXTERN bool hasBootSequence() const;
    QPID_COMMON_EXTERN void clearBootSequenceFlag();
    QPID_COMMON_EXTERN void setBrokerId(const Uuid& _brokerId);
    QPID_COMMON_EXTERN const Uuid& getBrokerId() const;
    QPID_COMMON_EXTERN bool hasBrokerId() const;
    QPID_COMMON_EXTERN void clearBrokerIdFlag();
    QPID_COMMON_EXTERN void setVendor(const string& _vendor);
    QPID_COMMON_EXTERN const string& getVendor() const;
    QPID_COMMON_EXTERN bool hasVendor() const;
    QPID_COMMON_EXTERN void clearVendorFlag();
    QPID_COMMON_EXTERN void setProduct(const string& _product);
    QPID_COMMON_EXTERN const string& getProduct() const;
    QPID_COMMON_EXTERN bool hasProduct() const;
    QPID_COMMON_EXTERN void clearProductFlag();
    QPID_COMMON_EXTERN void setInstance(const string& _instance);
    QPID_COMMON_EXTERN const string& getInstance() const;
    QPID_COMMON_EXTERN bool hasInstance() const;
    QPID_COMMON_EXTERN void clearInstanceFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.managementSetupState(getObjectNum(), getBootSequence(), getBrokerId(), getVendor(), getProduct(), getInstance());
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
}; /* class ClusterConnectionManagementSetupStateBody */

}}
#endif  /*!QPID_FRAMING_CLUSTERCONNECTIONMANAGEMENTSETUPSTATEBODY_H*/
