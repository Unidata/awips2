#ifndef _AMQMethodBody_
#define _AMQMethodBody_

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
#include "qpid/framing/amqp_types.h"
#include "qpid/framing/AMQBody.h"
#include "qpid/framing/ProtocolVersion.h"
#include "qpid/CommonImportExport.h"

#include <boost/shared_ptr.hpp>
#include <ostream>
#include <assert.h>

namespace qpid {
namespace framing {

class Buffer;
class AMQP_ServerOperations;
class MethodBodyConstVisitor;

class AMQMethodBody : public AMQBody {
  public:
    AMQMethodBody() {}
    QPID_COMMON_EXTERN virtual ~AMQMethodBody();

    virtual void accept(MethodBodyConstVisitor&) const = 0;
    
    virtual MethodId amqpMethodId() const = 0;
    virtual ClassId  amqpClassId() const = 0;
    virtual bool isContentBearing() const = 0;
    virtual bool resultExpected() const = 0;    
    virtual bool responseExpected() const = 0;    

    template <class T> bool isA() const {
        return amqpClassId()==T::CLASS_ID && amqpMethodId()==T::METHOD_ID;
    }

    virtual uint32_t encodedSize() const = 0;
    virtual uint8_t type() const { return METHOD_BODY; }

    virtual bool isSync() const { return false; /*only ModelMethods can have the sync flag set*/ }
    virtual void setSync(bool) const { /*only ModelMethods can have the sync flag set*/ }

    AMQMethodBody* getMethod() { return this; }
    const AMQMethodBody* getMethod() const { return this; }
    void accept(AMQBodyConstVisitor& v) const { v.visit(*this); }
};


}} // namespace qpid::framing


#endif
