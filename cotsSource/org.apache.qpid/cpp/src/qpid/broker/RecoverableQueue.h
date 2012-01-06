#ifndef _broker_RecoverableQueue_h
#define _broker_RecoverableQueue_h

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

#include "qpid/broker/RecoverableMessage.h"
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace broker {

class ExternalQueueStore;

/**
 * The interface through which messages are added back to queues on
 * recovery.
 */
class RecoverableQueue
{
public:
    typedef boost::shared_ptr<RecoverableQueue> shared_ptr;

    virtual void setPersistenceId(uint64_t id) = 0;
    virtual uint64_t getPersistenceId() const = 0;
    /**
     * Used during recovery to add stored messages back to the queue
     */
    virtual void recover(RecoverableMessage::shared_ptr msg) = 0;
    virtual ~RecoverableQueue() {};

    virtual const std::string& getName() const = 0;
    virtual void setExternalQueueStore(ExternalQueueStore* inst) = 0;
	virtual ExternalQueueStore* getExternalQueueStore() const = 0;

};

}}


#endif
