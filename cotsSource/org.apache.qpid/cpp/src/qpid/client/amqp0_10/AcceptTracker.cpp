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
#include "AcceptTracker.h"

namespace qpid {
namespace client {
namespace amqp0_10 {

void AcceptTracker::State::accept()
{
    unconfirmed.add(unaccepted);
    unaccepted.clear();
}

void AcceptTracker::State::release()
{
    unaccepted.clear();
}

uint32_t AcceptTracker::State::acceptsPending()
{
    return unconfirmed.size();
}

void AcceptTracker::State::completed(qpid::framing::SequenceSet& set)
{
    unconfirmed.remove(set);
}

void AcceptTracker::delivered(const std::string& destination, const qpid::framing::SequenceNumber& id)
{
    aggregateState.unaccepted.add(id);
    destinationState[destination].unaccepted.add(id);
}

void AcceptTracker::accept(qpid::client::AsyncSession& session)
{
    for (StateMap::iterator i = destinationState.begin(); i != destinationState.end(); ++i) {
        i->second.accept();
    }
    Record record;
    record.status = session.messageAccept(aggregateState.unaccepted);
    record.accepted = aggregateState.unaccepted;
    pending.push_back(record);
    aggregateState.accept();
}

void AcceptTracker::release(qpid::client::AsyncSession& session)
{
    session.messageRelease(aggregateState.unaccepted);
    for (StateMap::iterator i = destinationState.begin(); i != destinationState.end(); ++i) {
        i->second.release();
    }
    aggregateState.release();
}

uint32_t AcceptTracker::acceptsPending()
{
    checkPending();
    return aggregateState.acceptsPending();
}

uint32_t AcceptTracker::acceptsPending(const std::string& destination)
{
    checkPending();
    return destinationState[destination].acceptsPending();
}

void AcceptTracker::reset()
{
    destinationState.clear();
    aggregateState.unaccepted.clear();
    aggregateState.unconfirmed.clear();
    pending.clear();
}

void AcceptTracker::checkPending()
{
    while (!pending.empty() && pending.front().status.isComplete()) {        
        completed(pending.front().accepted);
        pending.pop_front();
    }
}

void AcceptTracker::completed(qpid::framing::SequenceSet& set)
{
    for (StateMap::iterator i = destinationState.begin(); i != destinationState.end(); ++i) {
        i->second.completed(set);
    }
    aggregateState.completed(set);    
}

}}} // namespace qpid::client::amqp0_10
