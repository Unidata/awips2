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

#ifndef _AMQHeartbeatBody_
#define _AMQHeartbeatBody_

namespace qpid {
namespace framing {

class AMQHeartbeatBody :  public AMQBody
{
public:
    QPID_COMMON_EXTERN virtual ~AMQHeartbeatBody();
    inline uint32_t encodedSize() const { return 0; }
    inline uint8_t type() const { return HEARTBEAT_BODY; }
    inline void encode(Buffer& ) const {}
    inline void decode(Buffer& , uint32_t /*size*/) {}
    QPID_COMMON_EXTERN virtual void print(std::ostream& out) const;
    void accept(AMQBodyConstVisitor& v) const { v.visit(*this); }
    boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }
};

}
}

#endif
