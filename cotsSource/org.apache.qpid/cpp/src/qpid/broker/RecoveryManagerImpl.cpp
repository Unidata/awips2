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
#include "qpid/broker/RecoveryManagerImpl.h"

#include "qpid/broker/Message.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/Link.h"
#include "qpid/broker/Bridge.h"
#include "qpid/broker/RecoveredEnqueue.h"
#include "qpid/broker/RecoveredDequeue.h"
#include "qpid/framing/reply_exceptions.h"

using boost::dynamic_pointer_cast;
using boost::intrusive_ptr;

namespace qpid {
namespace broker {

RecoveryManagerImpl::RecoveryManagerImpl(QueueRegistry& _queues, ExchangeRegistry& _exchanges, LinkRegistry& _links,
                                         DtxManager& _dtxMgr, uint64_t _stagingThreshold) 
    : queues(_queues), exchanges(_exchanges), links(_links), dtxMgr(_dtxMgr), stagingThreshold(_stagingThreshold) {}

RecoveryManagerImpl::~RecoveryManagerImpl() {}

class RecoverableMessageImpl : public RecoverableMessage
{
    intrusive_ptr<Message> msg;
    const uint64_t stagingThreshold;
public:
    RecoverableMessageImpl(const intrusive_ptr<Message>& _msg, uint64_t _stagingThreshold); 
    ~RecoverableMessageImpl() {};
    void setPersistenceId(uint64_t id);
    void setRedelivered();
    bool loadContent(uint64_t available);
    void decodeContent(framing::Buffer& buffer);
    void recover(Queue::shared_ptr queue);
    void enqueue(DtxBuffer::shared_ptr buffer, Queue::shared_ptr queue);
    void dequeue(DtxBuffer::shared_ptr buffer, Queue::shared_ptr queue);
};

class RecoverableQueueImpl : public RecoverableQueue
{
    Queue::shared_ptr queue;
public:
    RecoverableQueueImpl(const boost::shared_ptr<Queue>& _queue) : queue(_queue) {}
    ~RecoverableQueueImpl() {};
    void setPersistenceId(uint64_t id);    
	uint64_t getPersistenceId() const;
    const std::string& getName() const;
    void setExternalQueueStore(ExternalQueueStore* inst);
    ExternalQueueStore* getExternalQueueStore() const;
    void recover(RecoverableMessage::shared_ptr msg);
    void enqueue(DtxBuffer::shared_ptr buffer, RecoverableMessage::shared_ptr msg);
    void dequeue(DtxBuffer::shared_ptr buffer, RecoverableMessage::shared_ptr msg);
};

class RecoverableExchangeImpl : public RecoverableExchange
{
    Exchange::shared_ptr exchange;
    QueueRegistry& queues;
public:
    RecoverableExchangeImpl(Exchange::shared_ptr _exchange, QueueRegistry& _queues) : exchange(_exchange), queues(_queues) {}
    void setPersistenceId(uint64_t id);
    void bind(const std::string& queue, const std::string& routingKey, qpid::framing::FieldTable& args);
};

class RecoverableConfigImpl : public RecoverableConfig
{
    Link::shared_ptr   link;
    Bridge::shared_ptr bridge;
public:
    RecoverableConfigImpl(Link::shared_ptr _link)     : link(_link)     {}
    RecoverableConfigImpl(Bridge::shared_ptr _bridge) : bridge(_bridge) {}
    void setPersistenceId(uint64_t id);
};

class RecoverableTransactionImpl : public RecoverableTransaction
{
    DtxBuffer::shared_ptr buffer;
public:
    RecoverableTransactionImpl(DtxBuffer::shared_ptr _buffer) : buffer(_buffer) {}
    void enqueue(RecoverableQueue::shared_ptr queue, RecoverableMessage::shared_ptr message);
    void dequeue(RecoverableQueue::shared_ptr queue, RecoverableMessage::shared_ptr message);
};

RecoverableExchange::shared_ptr RecoveryManagerImpl::recoverExchange(framing::Buffer& buffer)
{
    Exchange::shared_ptr e = Exchange::decode(exchanges, buffer);
    if (e) {
        return RecoverableExchange::shared_ptr(new RecoverableExchangeImpl(e, queues));
    } else {
        return RecoverableExchange::shared_ptr();
    }
}

RecoverableQueue::shared_ptr RecoveryManagerImpl::recoverQueue(framing::Buffer& buffer)
{
    Queue::shared_ptr queue = Queue::decode(queues, buffer, true);
    try {
        Exchange::shared_ptr exchange = exchanges.getDefault();
        if (exchange) {
            exchange->bind(queue, queue->getName(), 0);
            queue->bound(exchange->getName(), queue->getName(), framing::FieldTable());
        }
    } catch (const framing::NotFoundException& /*e*/) {
        //assume no default exchange has been declared
    }
    return RecoverableQueue::shared_ptr(new RecoverableQueueImpl(queue));
}

RecoverableMessage::shared_ptr RecoveryManagerImpl::recoverMessage(framing::Buffer& buffer)
{
    boost::intrusive_ptr<Message> message(new Message());
    message->decodeHeader(buffer);
    return RecoverableMessage::shared_ptr(new RecoverableMessageImpl(message, stagingThreshold));    
}

RecoverableTransaction::shared_ptr RecoveryManagerImpl::recoverTransaction(const std::string& xid, 
                                                                           std::auto_ptr<TPCTransactionContext> txn)
{
    DtxBuffer::shared_ptr buffer(new DtxBuffer());
    dtxMgr.recover(xid, txn, buffer);
    return RecoverableTransaction::shared_ptr(new RecoverableTransactionImpl(buffer));
}

RecoverableConfig::shared_ptr RecoveryManagerImpl::recoverConfig(framing::Buffer& buffer)
{
    string kind;

    buffer.getShortString (kind);
    if      (kind == "link")
        return RecoverableConfig::shared_ptr(new RecoverableConfigImpl(Link::decode (links, buffer)));
    else if (kind == "bridge")
        return RecoverableConfig::shared_ptr(new RecoverableConfigImpl(Bridge::decode (links, buffer)));

    return RecoverableConfig::shared_ptr(); // TODO: raise an exception instead
}

void RecoveryManagerImpl::recoveryComplete()
{
    //notify all queues and exchanges
    queues.eachQueue(boost::bind(&Queue::recoveryComplete, _1, boost::ref(exchanges)));
    exchanges.eachExchange(boost::bind(&Exchange::recoveryComplete, _1, boost::ref(exchanges)));
}

RecoverableMessageImpl:: RecoverableMessageImpl(const intrusive_ptr<Message>& _msg, uint64_t _stagingThreshold) : msg(_msg), stagingThreshold(_stagingThreshold) 
{
    if (!msg->isPersistent()) {
        msg->forcePersistent(); // set so that message will get dequeued from store.
    }
}

bool RecoverableMessageImpl::loadContent(uint64_t available)
{
    return !stagingThreshold || available < stagingThreshold;
}

void RecoverableMessageImpl::decodeContent(framing::Buffer& buffer)
{
    msg->decodeContent(buffer);
}

void RecoverableMessageImpl::recover(Queue::shared_ptr queue)
{
    queue->recover(msg);
}

void RecoverableMessageImpl::setPersistenceId(uint64_t id)
{
    msg->setPersistenceId(id);
}

void RecoverableMessageImpl::setRedelivered()
{
    msg->redeliver();
}

void RecoverableQueueImpl::recover(RecoverableMessage::shared_ptr msg)
{
    dynamic_pointer_cast<RecoverableMessageImpl>(msg)->recover(queue);
}

void RecoverableQueueImpl::setPersistenceId(uint64_t id)
{
    queue->setPersistenceId(id);
}
       
uint64_t RecoverableQueueImpl::getPersistenceId() const
{
	return queue->getPersistenceId();
}

const std::string& RecoverableQueueImpl::getName() const
{
    return queue->getName();
}
    
void RecoverableQueueImpl::setExternalQueueStore(ExternalQueueStore* inst)
{
    queue->setExternalQueueStore(inst);
}

ExternalQueueStore* RecoverableQueueImpl::getExternalQueueStore() const
{
	return queue->getExternalQueueStore();
}

void RecoverableExchangeImpl::setPersistenceId(uint64_t id)
{
    exchange->setPersistenceId(id);
}

void RecoverableConfigImpl::setPersistenceId(uint64_t id)
{
    if (link.get())
        link->setPersistenceId(id);
    else if (bridge.get())
        bridge->setPersistenceId(id);
}

void RecoverableExchangeImpl::bind(const string& queueName,
                                   const string& key,
                                   framing::FieldTable& args)
{
    Queue::shared_ptr queue = queues.find(queueName);
    exchange->bind(queue, key, &args);
    queue->bound(exchange->getName(), key, args);
}

void RecoverableMessageImpl::dequeue(DtxBuffer::shared_ptr buffer, Queue::shared_ptr queue)
{
    buffer->enlist(TxOp::shared_ptr(new RecoveredDequeue(queue, msg)));
}

void RecoverableMessageImpl::enqueue(DtxBuffer::shared_ptr buffer, Queue::shared_ptr queue)
{
    msg->enqueueComplete(); // recoved nmessage to enqueued in store already
    buffer->enlist(TxOp::shared_ptr(new RecoveredEnqueue(queue, msg)));
}

void RecoverableQueueImpl::dequeue(DtxBuffer::shared_ptr buffer, RecoverableMessage::shared_ptr message)
{
    dynamic_pointer_cast<RecoverableMessageImpl>(message)->dequeue(buffer, queue);
}

void RecoverableQueueImpl::enqueue(DtxBuffer::shared_ptr buffer, RecoverableMessage::shared_ptr message)
{
    dynamic_pointer_cast<RecoverableMessageImpl>(message)->enqueue(buffer, queue);
}

void RecoverableTransactionImpl::dequeue(RecoverableQueue::shared_ptr queue, RecoverableMessage::shared_ptr message)
{
    dynamic_pointer_cast<RecoverableQueueImpl>(queue)->dequeue(buffer, message);
}

void RecoverableTransactionImpl::enqueue(RecoverableQueue::shared_ptr queue, RecoverableMessage::shared_ptr message)
{
    dynamic_pointer_cast<RecoverableQueueImpl>(queue)->enqueue(buffer, message);
}

}}
