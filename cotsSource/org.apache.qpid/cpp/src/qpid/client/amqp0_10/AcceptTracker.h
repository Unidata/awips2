#ifndef QPID_CLIENT_AMQP0_10_ACCEPTTRACKER_H
#define QPID_CLIENT_AMQP0_10_ACCEPTTRACKER_H

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
#include "qpid/client/AsyncSession.h"
#include "qpid/client/Completion.h"
#include "qpid/framing/SequenceNumber.h"
#include "qpid/framing/SequenceSet.h"
#include <deque>
#include <map>

namespace qpid {
namespace client {
namespace amqp0_10 {

/**
 * Tracks the set of messages requiring acceptance, and those for
 * which an accept has been issued but is yet to be confirmed
 * complete.
 */
class AcceptTracker
{
  public:
    void delivered(const std::string& destination, const qpid::framing::SequenceNumber& id);
    void accept(qpid::client::AsyncSession&);
    void release(qpid::client::AsyncSession&);
    uint32_t acceptsPending();
    uint32_t acceptsPending(const std::string& destination);
    void reset();
  private:
    struct State 
    {
        /**
         * ids of messages that have been delivered but not yet
         * accepted
         */
        qpid::framing::SequenceSet unaccepted;
        /**
         * ids of messages for which an accpet has been issued but not
         * yet confirmed as completed
         */
        qpid::framing::SequenceSet unconfirmed;

        void accept();
        void release();
        uint32_t acceptsPending();
        void completed(qpid::framing::SequenceSet&);
    };
    typedef std::map<std::string, State> StateMap;
    struct Record
    {
        qpid::client::Completion status;
        qpid::framing::SequenceSet accepted;
    };
    typedef std::deque<Record> Records;

    State aggregateState;
    StateMap destinationState;
    Records pending;

    void checkPending();
    void completed(qpid::framing::SequenceSet&);
};
}}} // namespace qpid::client::amqp0_10

#endif  /*!QPID_CLIENT_AMQP0_10_ACCEPTTRACKER_H*/
