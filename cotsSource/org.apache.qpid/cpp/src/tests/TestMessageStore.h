#ifndef _tests_TestMessageStore_h
#define _tests_TestMessageStore_h

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
#include "qpid/broker/NullMessageStore.h"
#include <vector>

using namespace qpid;
using namespace qpid::broker;
using namespace qpid::framing;

namespace qpid {
namespace tests {

typedef std::pair<string, boost::intrusive_ptr<PersistableMessage> > msg_queue_pair;

class TestMessageStore : public NullMessageStore
{
  public:
    std::vector<boost::intrusive_ptr<PersistableMessage> > dequeued;
    std::vector<msg_queue_pair> enqueued;

    void dequeue(TransactionContext*,
                 const boost::intrusive_ptr<PersistableMessage>& msg,
                 const PersistableQueue& /*queue*/)
    {
        dequeued.push_back(msg);
    }

    void enqueue(TransactionContext*,
                 const boost::intrusive_ptr<PersistableMessage>& msg,
                 const PersistableQueue& queue)
    {
        msg->enqueueComplete();
        enqueued.push_back(msg_queue_pair(queue.getName(), msg));
    }

    TestMessageStore() : NullMessageStore() {}
    ~TestMessageStore(){}
};

}} // namespace qpid::tests

#endif
