#ifndef _broker_RecoverableTransaction_h
#define _broker_RecoverableTransaction_h

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

#include "qpid/broker/RecoverableMessage.h"
#include "qpid/broker/RecoverableQueue.h"

namespace qpid {
namespace broker {

/**
 * The interface through which prepared 2pc transactions are
 * recovered.
 */
class RecoverableTransaction
{
public:
    typedef boost::shared_ptr<RecoverableTransaction> shared_ptr;
    virtual void enqueue(RecoverableQueue::shared_ptr queue, RecoverableMessage::shared_ptr message) = 0;
    virtual void dequeue(RecoverableQueue::shared_ptr queue, RecoverableMessage::shared_ptr message) = 0;
    virtual ~RecoverableTransaction() {};
};

}}


#endif
