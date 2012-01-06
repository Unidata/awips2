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
#ifndef _Consumer_
#define _Consumer_

#include "qpid/broker/Message.h"
#include "qpid/broker/QueuedMessage.h"
#include "qpid/broker/OwnershipToken.h"

namespace qpid {
namespace broker {

class Queue;

class Consumer {
    const bool acquires;
  public:
    typedef boost::shared_ptr<Consumer> shared_ptr;            
    
    framing::SequenceNumber position;
    
    Consumer(bool preAcquires = true) : acquires(preAcquires) {}
    bool preAcquires() const { return acquires; }
    virtual bool deliver(QueuedMessage& msg) = 0;
    virtual void notify() = 0;
    virtual bool filter(boost::intrusive_ptr<Message>) { return true; }
    virtual bool accept(boost::intrusive_ptr<Message>) { return true; }
    virtual OwnershipToken* getSession() = 0;
    virtual ~Consumer(){}
};

}}


#endif
