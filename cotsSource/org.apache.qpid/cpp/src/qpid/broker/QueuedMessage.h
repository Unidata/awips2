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
#ifndef _QueuedMessage_
#define _QueuedMessage_

#include "qpid/broker/Message.h"

namespace qpid {
namespace broker {

class Queue;

struct QueuedMessage
{
    boost::intrusive_ptr<Message> payload;
    framing::SequenceNumber position;
    Queue* queue;

    QueuedMessage() : queue(0) {}
    QueuedMessage(Queue* q, boost::intrusive_ptr<Message> msg, framing::SequenceNumber sn) : 
        payload(msg), position(sn), queue(q) {}
    QueuedMessage(Queue* q) : queue(q) {}
    
};
    inline bool operator<(const QueuedMessage& a, const QueuedMessage& b) { return a.position < b.position; } 

}}


#endif
