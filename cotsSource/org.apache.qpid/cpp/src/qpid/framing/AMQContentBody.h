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
#include "qpid/framing/Buffer.h"
#include "qpid/CommonImportExport.h"

#ifndef _AMQContentBody_
#define _AMQContentBody_

namespace qpid {
namespace framing {

class AMQContentBody :  public AMQBody
{
    string data;

public:
    QPID_COMMON_EXTERN AMQContentBody();
    QPID_COMMON_EXTERN AMQContentBody(const string& data);
    inline virtual ~AMQContentBody(){}
    QPID_COMMON_EXTERN inline uint8_t type() const { return CONTENT_BODY; };
    QPID_COMMON_EXTERN inline const string& getData() const { return data; }
    QPID_COMMON_EXTERN inline string& getData() { return data; }
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN void encode(Buffer& buffer) const;
    QPID_COMMON_EXTERN void decode(Buffer& buffer, uint32_t size);
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
    QPID_COMMON_EXTERN void accept(AMQBodyConstVisitor& v) const { v.visit(*this); }
    QPID_COMMON_EXTERN boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }
};

}
}


#endif
