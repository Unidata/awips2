#ifndef _broker_Persistable_h
#define _broker_Persistable_h

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
#include "qpid/RefCounted.h"

namespace qpid {
namespace broker {

/**
 * Base class for all persistable objects
 */
class Persistable : public RefCounted
{
public:
    /**
     * Allows the store to attach its own identifier to this object
     */
    virtual void setPersistenceId(uint64_t id) const = 0;
    /**
     * Returns any identifier the store may have attached to this
     * object
     */
    virtual uint64_t getPersistenceId() const = 0;
    /**
     * Encodes the persistable state of this object into the supplied
     * buffer
     */
    virtual void encode(framing::Buffer& buffer) const = 0;
    /**
     * @returns the size of the buffer needed to encode this object
     */
    virtual uint32_t encodedSize() const = 0;

    virtual ~Persistable() {};
};

}}


#endif
