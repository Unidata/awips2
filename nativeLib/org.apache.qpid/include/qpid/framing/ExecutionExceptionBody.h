#ifndef QPID_FRAMING_EXECUTIONEXCEPTIONBODY_H
#define QPID_FRAMING_EXECUTIONEXCEPTIONBODY_H
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

class ExecutionExceptionBody : public ModelMethod {
    uint16_t errorCode;
    SequenceNumber commandId;
    uint8_t classCode;
    uint8_t commandCode;
    uint8_t fieldIndex;
    string description;
    FieldTable errorInfo;
    uint16_t flags;
public:
    static const ClassId CLASS_ID = 0x3;
    static const MethodId METHOD_ID = 0x3;
    ExecutionExceptionBody(
        ProtocolVersion, uint16_t _errorCode,
        const SequenceNumber& _commandId,
        uint8_t _classCode,
        uint8_t _commandCode,
        uint8_t _fieldIndex,
        const string& _description,
        const FieldTable& _errorInfo) : 
        errorCode(_errorCode),
        commandId(_commandId),
        classCode(_classCode),
        commandCode(_commandCode),
        fieldIndex(_fieldIndex),
        description(_description),
        errorInfo(_errorInfo),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
        flags |= (1 << 12);
        flags |= (1 << 13);
        flags |= (1 << 14);
    }
    ExecutionExceptionBody(ProtocolVersion=ProtocolVersion())  : errorCode(0), classCode(0), commandCode(0), fieldIndex(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setErrorCode(uint16_t _errorCode);
    QPID_COMMON_EXTERN uint16_t getErrorCode() const;
    QPID_COMMON_EXTERN bool hasErrorCode() const;
    QPID_COMMON_EXTERN void clearErrorCodeFlag();
    QPID_COMMON_EXTERN void setCommandId(const SequenceNumber& _commandId);
    QPID_COMMON_EXTERN SequenceNumber getCommandId() const;
    QPID_COMMON_EXTERN bool hasCommandId() const;
    QPID_COMMON_EXTERN void clearCommandIdFlag();
    QPID_COMMON_EXTERN void setClassCode(uint8_t _classCode);
    QPID_COMMON_EXTERN uint8_t getClassCode() const;
    QPID_COMMON_EXTERN bool hasClassCode() const;
    QPID_COMMON_EXTERN void clearClassCodeFlag();
    QPID_COMMON_EXTERN void setCommandCode(uint8_t _commandCode);
    QPID_COMMON_EXTERN uint8_t getCommandCode() const;
    QPID_COMMON_EXTERN bool hasCommandCode() const;
    QPID_COMMON_EXTERN void clearCommandCodeFlag();
    QPID_COMMON_EXTERN void setFieldIndex(uint8_t _fieldIndex);
    QPID_COMMON_EXTERN uint8_t getFieldIndex() const;
    QPID_COMMON_EXTERN bool hasFieldIndex() const;
    QPID_COMMON_EXTERN void clearFieldIndexFlag();
    QPID_COMMON_EXTERN void setDescription(const string& _description);
    QPID_COMMON_EXTERN const string& getDescription() const;
    QPID_COMMON_EXTERN bool hasDescription() const;
    QPID_COMMON_EXTERN void clearDescriptionFlag();
    QPID_COMMON_EXTERN void setErrorInfo(const FieldTable& _errorInfo);
    QPID_COMMON_EXTERN const FieldTable& getErrorInfo() const;
    QPID_COMMON_EXTERN FieldTable& getErrorInfo();
    QPID_COMMON_EXTERN bool hasErrorInfo() const;
    QPID_COMMON_EXTERN void clearErrorInfoFlag();
    typedef void ResultType;

    template <class T> ResultType invoke(T& invocable) const {
        return invocable.exception(getErrorCode(), getCommandId(), getClassCode(), getCommandCode(), getFieldIndex(), getDescription(), getErrorInfo());
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
}; /* class ExecutionExceptionBody */

}}
#endif  /*!QPID_FRAMING_EXECUTIONEXCEPTIONBODY_H*/
