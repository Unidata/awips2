#ifndef _broker_RecoverableMessage_h
#define _broker_RecoverableMessage_h

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

#include <boost/shared_ptr.hpp>
#include "qpid/framing/amqp_types.h"
#include "qpid/framing/Buffer.h"

namespace qpid {
namespace broker {

/**
 * The interface through which messages are reloaded on recovery.
 */
class RecoverableMessage
{
public:
    typedef boost::shared_ptr<RecoverableMessage> shared_ptr;
    virtual void setPersistenceId(uint64_t id) = 0;
    virtual void setRedelivered() = 0;
    /**
     * Used by store to determine whether to load content on recovery
     * or let message load its own content as and when it requires it.
     * 
     * @returns true if the content of the message should be loaded
     */
    virtual bool loadContent(uint64_t available) = 0;
    /**
     * Loads the content held in the supplied buffer (may do checking
     * of length as necessary)
     */
    virtual void decodeContent(framing::Buffer& buffer) = 0;
    virtual ~RecoverableMessage() {};
};

}}


#endif
