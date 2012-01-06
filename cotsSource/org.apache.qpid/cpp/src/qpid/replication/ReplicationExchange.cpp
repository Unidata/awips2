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
#include "qpid/replication/ReplicationExchange.h"
#include "qpid/replication/constants.h"
#include "qpid/Plugin.h"
#include "qpid/broker/Broker.h"
#include "qpid/broker/ExchangeRegistry.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/log/Statement.h"
#include <boost/bind.hpp>

namespace qpid {
namespace replication {

using namespace qpid::broker;
using namespace qpid::framing;
using namespace qpid::replication::constants;

const std::string SEQUENCE_VALUE("qpid.replication-event.sequence");
ReplicationExchange::ReplicationExchange(const std::string& name, bool durable, 
                                         const FieldTable& _args,
                                         QueueRegistry& qr,
                                         Manageable* parent, Broker* broker) 
    : Exchange(name, durable, _args, parent, broker), queues(qr), sequence(args.getAsInt64(SEQUENCE_VALUE)), init(false)
{
    args.setInt64(SEQUENCE_VALUE, sequence);
    if (mgmtExchange != 0)
        mgmtExchange->set_type(typeName);
}

std::string ReplicationExchange::getType() const { return typeName; }            

void ReplicationExchange::route(Deliverable& msg, const std::string& /*routingKey*/, const FieldTable* args)
{
    if (mgmtExchange != 0) {
        mgmtExchange->inc_msgReceives();
        mgmtExchange->inc_byteReceives(msg.contentSize());
    }
    if (args) {
        int eventType = args->getAsInt(REPLICATION_EVENT_TYPE);
        if (eventType) {
            if (isDuplicate(args)) return;
            switch (eventType) {
              case ENQUEUE:
                handleEnqueueEvent(args, msg);
                return;
              case DEQUEUE:
                handleDequeueEvent(args, msg);
                return;
              default:
                throw IllegalArgumentException(QPID_MSG("Illegal value for " << REPLICATION_EVENT_TYPE << ": " << eventType));
            }
        }
    } else {
        QPID_LOG(warning, "Dropping unexpected message with no headers");
        if (mgmtExchange != 0) {
            mgmtExchange->inc_msgDrops();
            mgmtExchange->inc_byteDrops(msg.contentSize());
        }
    }
}

void ReplicationExchange::handleEnqueueEvent(const FieldTable* args, Deliverable& msg)
{
    std::string queueName = args->getAsString(REPLICATION_TARGET_QUEUE);
    Queue::shared_ptr queue = queues.find(queueName);
    if (queue) {

        SequenceNumber seqno1(args->getAsInt(QUEUE_MESSAGE_POSITION));

        // note that queue will ++ before enqueue.      
        if (queue->getPosition() > --seqno1) // test queue.pos < seqnumber
        {
            QPID_LOG(error, "Cannot enqueue replicated message. Destination Queue " << queueName << " ahead of source queue");
            mgmtExchange->inc_msgDrops();
            mgmtExchange->inc_byteDrops(msg.contentSize());
        } else {
            queue->setPosition(seqno1);  

            FieldTable& headers = msg.getMessage().getProperties<MessageProperties>()->getApplicationHeaders();
            headers.erase(REPLICATION_TARGET_QUEUE);
            headers.erase(REPLICATION_EVENT_SEQNO);
            headers.erase(REPLICATION_EVENT_TYPE);
            headers.erase(QUEUE_MESSAGE_POSITION);
            msg.deliverTo(queue);
            QPID_LOG(debug, "Enqueued replicated message onto " << queueName);
            if (mgmtExchange != 0) {
                mgmtExchange->inc_msgRoutes();
                mgmtExchange->inc_byteRoutes( msg.contentSize());
            }
        }
    } else {
        QPID_LOG(error, "Cannot enqueue replicated message. Queue " << queueName << " does not exist");
        if (mgmtExchange != 0) {
            mgmtExchange->inc_msgDrops();
            mgmtExchange->inc_byteDrops(msg.contentSize());
        }
    }
}

void ReplicationExchange::handleDequeueEvent(const FieldTable* args, Deliverable& msg)
{
    std::string queueName = args->getAsString(REPLICATION_TARGET_QUEUE);
    Queue::shared_ptr queue = queues.find(queueName);
    if (queue) {
        SequenceNumber position(args->getAsInt(DEQUEUED_MESSAGE_POSITION));        
        QueuedMessage dequeued;
        if (queue->acquireMessageAt(position, dequeued)) {
            queue->dequeue(0, dequeued);
            QPID_LOG(debug, "Processed replicated 'dequeue' event from " << queueName << " at position " << position);
            if (mgmtExchange != 0) {
                mgmtExchange->inc_msgRoutes();
                mgmtExchange->inc_byteRoutes(msg.contentSize());
            }
        } else {
            QPID_LOG(warning, "Could not acquire message " << position << " from " << queueName);
            if (mgmtExchange != 0) {
                mgmtExchange->inc_msgDrops();
                mgmtExchange->inc_byteDrops(msg.contentSize());
            }
        }
    } else {
        QPID_LOG(error, "Cannot process replicated 'dequeue' event. Queue " << queueName << " does not exist");
        if (mgmtExchange != 0) {
            mgmtExchange->inc_msgDrops();
            mgmtExchange->inc_byteDrops(msg.contentSize());
        }
    }
}

bool ReplicationExchange::isDuplicate(const FieldTable* args)
{
    if (!args->get(REPLICATION_EVENT_SEQNO)) return false;
    SequenceNumber seqno(args->getAsInt(REPLICATION_EVENT_SEQNO));
    if (!init) {
        init = true;
        sequence = seqno;
        return false;
    } else if (seqno > sequence) {
        if (seqno - sequence > 1) {
            QPID_LOG(error, "Gap in replication event sequence between: " << sequence << " and " << seqno);
        }
        sequence = seqno;
        return false;
    } else {
        QPID_LOG(info, "Duplicate detected: seqno=" << seqno << " (last seqno=" << sequence << ")");
        return true;
    }
}

bool ReplicationExchange::bind(Queue::shared_ptr /*queue*/, const std::string& /*routingKey*/, const FieldTable* /*args*/)
{
    throw NotImplementedException("Replication exchange does not support bind operation");
}

bool ReplicationExchange::unbind(Queue::shared_ptr /*queue*/, const std::string& /*routingKey*/, const FieldTable* /*args*/)
{
    throw NotImplementedException("Replication exchange does not support unbind operation");
}

bool ReplicationExchange::isBound(Queue::shared_ptr /*queue*/, const string* const /*routingKey*/, const FieldTable* const /*args*/)
{
    return false;
}

const std::string ReplicationExchange::typeName("replication");


void ReplicationExchange::encode(Buffer& buffer) const
{
    args.setInt64(std::string(SEQUENCE_VALUE), sequence);
    Exchange::encode(buffer);
}


struct ReplicationExchangePlugin : Plugin
{
    Broker* broker;

    ReplicationExchangePlugin();
    void earlyInitialize(Plugin::Target& target);
    void initialize(Plugin::Target& target);
    Exchange::shared_ptr create(const std::string& name, bool durable,
                                const framing::FieldTable& args, 
                                management::Manageable* parent, 
                                qpid::broker::Broker* broker);
};

ReplicationExchangePlugin::ReplicationExchangePlugin() : broker(0) {}

Exchange::shared_ptr ReplicationExchangePlugin::create(const std::string& name, bool durable,
                                                       const framing::FieldTable& args, 
                                                       management::Manageable* parent, qpid::broker::Broker* broker)
{
    Exchange::shared_ptr e(new ReplicationExchange(name, durable, args, broker->getQueues(), parent, broker));
    return e;
}


void ReplicationExchangePlugin::earlyInitialize(Plugin::Target& target)
{
    broker = dynamic_cast<broker::Broker*>(&target);
    if (broker) {
        ExchangeRegistry::FactoryFunction f = boost::bind(&ReplicationExchangePlugin::create, this, _1, _2, _3, _4, _5);
        broker->getExchanges().registerType(ReplicationExchange::typeName, f);
        QPID_LOG(info, "Registered replication exchange");
    }
}

void ReplicationExchangePlugin::initialize(Target&) {}

static ReplicationExchangePlugin exchangePlugin;

}} // namespace qpid::replication
