#ifndef QPID_REPLICATION_REPLICATINGEVENTLISTENER_H
#define QPID_REPLICATION_REPLICATINGEVENTLISTENER_H

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

#include "qpid/Plugin.h"
#include "qpid/Options.h"
#include "qpid/broker/Exchange.h"
#include "qpid/broker/Message.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/QueueEvents.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/SequenceNumber.h"

namespace qpid {
namespace replication {

/**
 * An event listener plugin that records queue events as messages on a
 * replication queue, from where they can be consumed (e.g. by an
 * inter-broker link to the corresponding QueueReplicationExchange
 * plugin.
 */
class ReplicatingEventListener : public Plugin
{
  public:
    Options* getOptions();
    void earlyInitialize(Plugin::Target& target);
    void initialize(Plugin::Target& target);
    void handle(qpid::broker::QueueEvents::Event);
  private:    
    struct PluginOptions : public Options
    {
        std::string queue;
        std::string exchange;
        std::string exchangeType;
        std::string name;
        bool createQueue;

        PluginOptions();
    };

    PluginOptions options;    
    qpid::broker::Queue::shared_ptr queue;
    qpid::broker::Exchange::shared_ptr exchange;

    void deliverDequeueMessage(const qpid::broker::QueuedMessage& enqueued);
    void deliverEnqueueMessage(const qpid::broker::QueuedMessage& enqueued);
    void route(boost::intrusive_ptr<qpid::broker::Message>);
    void shutdown();

    boost::intrusive_ptr<qpid::broker::Message> createMessage(const qpid::framing::FieldTable& headers);
    boost::intrusive_ptr<qpid::broker::Message> cloneMessage(qpid::broker::Queue& queue, 
                                                             boost::intrusive_ptr<qpid::broker::Message> original);
};

}} // namespace qpid::replication

#endif  /*!QPID_REPLICATION_REPLICATINGEVENTLISTENER_H*/
