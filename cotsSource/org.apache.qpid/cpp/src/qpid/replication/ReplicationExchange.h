#ifndef QPID_REPLICATION_REPLICATIONEXCHANGE_H
#define QPID_REPLICATION_REPLICATIONEXCHANGE_H

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
#include "qpid/broker/Exchange.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/SequenceNumber.h"

namespace qpid {
namespace replication {

/**
 * A custom exchange plugin that processes incoming messages
 * representing enqueue or dequeue events for particular queues and
 * carries out the corresponding action to replicate that on the local
 * broker.
 */
class ReplicationExchange : public qpid::broker::Exchange
{
  public:
    static const std::string typeName;

    ReplicationExchange(const std::string& name, bool durable, 
                        const qpid::framing::FieldTable& args,
                        qpid::broker::QueueRegistry& queues,
                        qpid::management::Manageable* parent = 0,
                        qpid::broker::Broker* broker = 0);

    std::string getType() const;

    void route(qpid::broker::Deliverable& msg, const std::string& routingKey, const qpid::framing::FieldTable* args);
        
    bool bind(qpid::broker::Queue::shared_ptr queue, const std::string& routingKey, const qpid::framing::FieldTable* args);
    bool unbind(qpid::broker::Queue::shared_ptr queue, const std::string& routingKey, const qpid::framing::FieldTable* args);
    bool isBound(qpid::broker::Queue::shared_ptr queue, const std::string* const routingKey, const qpid::framing::FieldTable* const args);
  private:
    qpid::broker::QueueRegistry& queues;
    qpid::framing::SequenceNumber sequence;
    bool init;

    bool isDuplicate(const qpid::framing::FieldTable* args);
    void handleEnqueueEvent(const qpid::framing::FieldTable* args, qpid::broker::Deliverable& msg);
    void handleDequeueEvent(const qpid::framing::FieldTable* args, qpid::broker::Deliverable& msg);
    void encode(framing::Buffer& buffer) const;
};
}} // namespace qpid::replication

#endif  /*!QPID_REPLICATION_REPLICATIONEXCHANGE_H*/
