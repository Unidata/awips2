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
#include "qpid/client/MessageReplayTracker.h"
#include <boost/bind.hpp>

namespace qpid {
namespace client {

MessageReplayTracker::MessageReplayTracker(uint f) : flushInterval(f), count(0) {}

void MessageReplayTracker::send(const Message& message, const std::string& destination)
{
    buffer.push_back(ReplayRecord(message, destination));    
    buffer.back().send(*this);
    if (flushInterval && ++count >= flushInterval) {
        checkCompletion();
        if (!buffer.empty()) session.flush();
    }
}
void MessageReplayTracker::init(AsyncSession s)
{
    session = s;
}

void MessageReplayTracker::replay(AsyncSession s)
{
    session = s;
    std::for_each(buffer.begin(), buffer.end(), boost::bind(&ReplayRecord::send, _1, boost::ref(*this)));
    session.flush();
    count = 0;
}

void MessageReplayTracker::setFlushInterval(uint f)
{
    flushInterval = f;
}

uint MessageReplayTracker::getFlushInterval()
{
    return flushInterval;
}

void MessageReplayTracker::checkCompletion()
{
    buffer.remove_if(boost::bind(&ReplayRecord::isComplete, _1));    
}

MessageReplayTracker::ReplayRecord::ReplayRecord(const Message& m, const std::string& d) : message(m), destination(d) {}

void MessageReplayTracker::ReplayRecord::send(MessageReplayTracker& tracker)
{
    status = tracker.session.messageTransfer(arg::destination=destination, arg::content=message);
}

bool MessageReplayTracker::ReplayRecord::isComplete()
{
    return status.isComplete();
}

}} // namespace qpid::client
