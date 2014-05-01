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
#include "qpid/broker/RecoveredDequeue.h"

using boost::intrusive_ptr;
using namespace qpid::broker;

RecoveredDequeue::RecoveredDequeue(Queue::shared_ptr _queue, intrusive_ptr<Message> _msg) : queue(_queue), msg(_msg)
{
    queue->recoverPrepared(msg);
}

bool RecoveredDequeue::prepare(TransactionContext*) throw()
{
    //should never be called; transaction has already prepared if an enqueue is recovered
    return false;
}

void RecoveredDequeue::commit() throw()
{
    queue->enqueueAborted(msg);
}

void RecoveredDequeue::rollback() throw()
{
    msg->enqueueComplete();
    queue->process(msg);
}

