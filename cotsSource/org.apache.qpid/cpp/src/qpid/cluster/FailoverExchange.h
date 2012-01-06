#ifndef QPID_CLUSTER_FAILOVEREXCHANGE_H
#define QPID_CLUSTER_FAILOVEREXCHANGE_H

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
#include "qpid/broker/DeliverableMessage.h"
#include "qpid/Url.h"

#include <vector>
#include <set>

namespace qpid {
namespace cluster {

/**
 * Failover exchange provides failover host list, as specified in AMQP 0-10.
 */
class FailoverExchange : public broker::Exchange
{
  public:
    static const std::string TYPE_NAME;

    FailoverExchange(management::Manageable* parent);
    
    void setUrls(const std::vector<Url>&);

    // Exchange overrides
    std::string getType() const;
    bool bind(broker::Queue::shared_ptr queue, const std::string& routingKey, const framing::FieldTable* args);
    bool unbind(broker::Queue::shared_ptr queue, const std::string& routingKey, const framing::FieldTable* args);
    bool isBound(broker::Queue::shared_ptr queue, const std::string* const routingKey, const framing::FieldTable* const args);
    void route(broker::Deliverable& msg, const std::string& routingKey, const framing::FieldTable* args);

  private:
    void sendUpdate(const broker::Queue::shared_ptr&);
    
    typedef sys::Mutex::ScopedLock Lock;
    typedef std::vector<Url> Urls;
    typedef std::set<broker::Queue::shared_ptr> Queues;

    sys::Mutex lock;
    Urls urls;
    Queues queues;
    
};
}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_FAILOVEREXCHANGE_H*/
