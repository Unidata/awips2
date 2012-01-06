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
#include "qpid/framing/Buffer.h"

#ifndef _HeaderProperties_
#define _HeaderProperties_

namespace qpid {
namespace framing {

    class HeaderProperties
    {
	
    public:
	inline virtual ~HeaderProperties(){}
	virtual uint8_t classId() const = 0;
	virtual uint32_t encodedSize() const = 0;
	virtual void encode(Buffer& buffer) const = 0;
	virtual void decode(Buffer& buffer, uint32_t size) = 0;
    };
}
}


#endif
