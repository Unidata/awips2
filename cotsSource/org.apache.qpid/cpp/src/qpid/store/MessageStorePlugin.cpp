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

#include "MessageStorePlugin.h"
#include "StorageProvider.h"
#include "StoreException.h"
#include "qpid/broker/Broker.h"
#include "qpid/Plugin.h"
#include "qpid/Options.h"
#include "qpid/DataDir.h"
#include "qpid/log/Statement.h"

/*
 * The MessageStore pointer given to the Broker points to static storage.
 * Thus, it cannot be deleted, especially by the broker. To prevent deletion,
 * this no-op deleter is used with the boost::shared_ptr. When the last
 * shared_ptr is destroyed, the deleter is called rather than delete().
 */
namespace {
  class NoopDeleter {
  public:
      NoopDeleter() {}
      void operator()(qpid::broker::MessageStore * /*p*/) {}
  };
}

namespace qpid {
namespace store {

static MessageStorePlugin static_instance_registers_plugin;


MessageStorePlugin::StoreOptions::StoreOptions(const std::string& name) :
    qpid::Options(name)
{
    addOptions()
        ("storage-provider", qpid::optValue(providerName, "PROVIDER"),
         "Name of the storage provider to use.")
        ;
}


void
MessageStorePlugin::earlyInitialize (qpid::Plugin::Target& target)
{
    qpid::broker::Broker* broker =
        dynamic_cast<qpid::broker::Broker*>(&target);
    if (0 == broker)
        return;        // Only listen to Broker targets

    // See if there are any storage provider plugins ready. If not, we can't
    // do a message store.
    qpid::Plugin::earlyInitAll(*this);

    if (providers.empty()) {
        QPID_LOG(warning,
                 "Message store plugin: No storage providers available.");
        provider = providers.end();
        return;
    }
    if (!options.providerName.empty()) {
        // If specific one was chosen, locate it in loaded set of providers.
        provider = providers.find(options.providerName);
        if (provider == providers.end())
            throw Exception("Message store plugin: storage provider '" +
                            options.providerName +
                            "' does not exist.");
    }
    else {
        // No specific provider chosen; if there's only one, use it. Else
        // report the need to pick one.
        if (providers.size() > 1) {
            provider = providers.end();
            throw Exception("Message store plugin: multiple provider plugins "
                            "loaded; must either load only one or select one "
                            "using --storage-provider");
        }
        provider = providers.begin();
    }

    provider->second->activate(*this);
    NoopDeleter d;
    boost::shared_ptr<qpid::broker::MessageStore> sp(this, d);
    broker->setStore(sp);
    target.addFinalizer(boost::bind(&MessageStorePlugin::finalizeMe, this));
}

void
MessageStorePlugin::initialize(qpid::Plugin::Target& target)
{
    qpid::broker::Broker* broker =
        dynamic_cast<qpid::broker::Broker*>(&target);
    if (0 == broker)
        return;        // Only listen to Broker targets

    // Pass along the initialize step to the provider that's activated.
    if (provider != providers.end()) {
        provider->second->initialize(*this);
    }
    //    qpid::Plugin::initializeAll(*this);
}

void
MessageStorePlugin::finalizeMe()
{
    finalize();              // Call finalizers on any Provider plugins
}

void
MessageStorePlugin::providerAvailable(const std::string name,
                                      StorageProvider *be)
{
    ProviderMap::value_type newSp(name, be);
    std::pair<ProviderMap::iterator, bool> inserted = providers.insert(newSp);
    if (inserted.second == false)
        QPID_LOG(warning, "Storage provider " << name << " duplicate; ignored.");
}

void
MessageStorePlugin::truncateInit(const bool /*saveStoreContent*/)
{
    QPID_LOG(info, "Store: truncateInit");
}


/**
 * Record the existence of a durable queue
 */
void
MessageStorePlugin::create(broker::PersistableQueue& queue,
                           const framing::FieldTable& args)
{
    if (queue.getName().size() == 0)
    {
        QPID_LOG(error,
                 "Cannot create store for empty (null) queue name - "
                 "ignoring and attempting to continue.");
        return;
    }
    if (queue.getPersistenceId()) {
        THROW_STORE_EXCEPTION("Queue already created: " + queue.getName());
    }
    provider->second->create(queue, args);
}

/**
 * Destroy a durable queue
 */
void
MessageStorePlugin::destroy(broker::PersistableQueue& queue)
{
    provider->second->destroy(queue);
}

/**
 * Record the existence of a durable exchange
 */
void
MessageStorePlugin::create(const broker::PersistableExchange& exchange,
                           const framing::FieldTable& args)
{
    if (exchange.getPersistenceId()) {
        THROW_STORE_EXCEPTION("Exchange already created: " + exchange.getName());
    }
    provider->second->create(exchange, args);
}

/**
 * Destroy a durable exchange
 */
void
MessageStorePlugin::destroy(const broker::PersistableExchange& exchange)
{
    provider->second->destroy(exchange);
}

/**
 * Record a binding
 */
void
MessageStorePlugin::bind(const broker::PersistableExchange& exchange,
                         const broker::PersistableQueue& queue,
                         const std::string& key,
                         const framing::FieldTable& args)
{
    provider->second->bind(exchange, queue, key, args);
}

/**
 * Forget a binding
 */
void
MessageStorePlugin::unbind(const broker::PersistableExchange& exchange,
                           const broker::PersistableQueue& queue,
                           const std::string& key,
                           const framing::FieldTable& args)
{
    provider->second->unbind(exchange, queue, key, args);
}

/**
 * Record generic durable configuration
 */
void
MessageStorePlugin::create(const broker::PersistableConfig& config)
{
    if (config.getPersistenceId()) {
        THROW_STORE_EXCEPTION("Config item already created: " +
                              config.getName());
    }
    provider->second->create(config);
}

/**
 * Destroy generic durable configuration
 */
void
MessageStorePlugin::destroy(const broker::PersistableConfig& config)
{
    provider->second->destroy(config);
}

/**
 * Stores a message before it has been enqueued
 * (enqueueing automatically stores the message so this is
 * only required if storage is required prior to that
 * point).
 */
void
MessageStorePlugin::stage(const boost::intrusive_ptr<broker::PersistableMessage>& msg)
{
    if (msg->getPersistenceId() == 0 && !msg->isContentReleased()) {
        provider->second->stage(msg);
    }
}

/**
 * Destroys a previously staged message. This only needs
 * to be called if the message is never enqueued. (Once
 * enqueued, deletion will be automatic when the message
 * is dequeued from all queues it was enqueued onto).
 */
void
MessageStorePlugin::destroy(broker::PersistableMessage& msg)
{
    if (msg.getPersistenceId())
        provider->second->destroy(msg);
}

/**
 * Appends content to a previously staged message
 */
void
MessageStorePlugin::appendContent
  (const boost::intrusive_ptr<const broker::PersistableMessage>& msg,
   const std::string& data)
{
    if (msg->getPersistenceId())
        provider->second->appendContent(msg, data);
    else
        THROW_STORE_EXCEPTION("Cannot append content. Message not known to store!");
}

/**
 * Loads (a section) of content data for the specified
 * message (previously stored through a call to stage or
 * enqueue) into data. The offset refers to the content
 * only (i.e. an offset of 0 implies that the start of the
 * content should be loaded, not the headers or related
 * meta-data).
 */
void
MessageStorePlugin::loadContent(const broker::PersistableQueue& queue,
                                const boost::intrusive_ptr<const broker::PersistableMessage>& msg,
                                std::string& data,
                                uint64_t offset,
                                uint32_t length)
{
    if (msg->getPersistenceId())
        provider->second->loadContent(queue, msg, data, offset, length);
    else
        THROW_STORE_EXCEPTION("Cannot load content. Message not known to store!");
}

/**
 * Enqueues a message, storing the message if it has not
 * been previously stored and recording that the given
 * message is on the given queue.
 *
 * Note: The operation is asynchronous so the return of this function does
 * not mean the operation is complete.
 */
void
MessageStorePlugin::enqueue(broker::TransactionContext* ctxt,
                            const boost::intrusive_ptr<broker::PersistableMessage>& msg,
                            const broker::PersistableQueue& queue)
{
    if (queue.getPersistenceId() == 0) {
        THROW_STORE_EXCEPTION("Queue not created: " + queue.getName());
    }
    provider->second->enqueue(ctxt, msg, queue);
}

/**
 * Dequeues a message, recording that the given message is
 * no longer on the given queue and deleting the message
 * if it is no longer on any other queue.
 *
 * Note: The operation is asynchronous so the return of this function does
 * not mean the operation is complete.
 */
void
MessageStorePlugin::dequeue(broker::TransactionContext* ctxt,
                            const boost::intrusive_ptr<broker::PersistableMessage>& msg,
                            const broker::PersistableQueue& queue)
{
    provider->second->dequeue(ctxt, msg, queue);
}

/**
 * Flushes all async messages to disk for the specified queue
 *
 * Note: The operation is asynchronous so the return of this function does
 * not mean the operation is complete.
 */
void
MessageStorePlugin::flush(const broker::PersistableQueue& queue)
{
    provider->second->flush(queue);
}

/**
 * Returns the number of outstanding AIO's for a given queue
 *
 * If 0, than all the enqueue / dequeues have been stored
 * to disk.
 */
uint32_t
MessageStorePlugin::outstandingQueueAIO(const broker::PersistableQueue& queue)
{
    return provider->second->outstandingQueueAIO(queue);
}

std::auto_ptr<broker::TransactionContext>
MessageStorePlugin::begin()
{
    return provider->second->begin();
}

std::auto_ptr<broker::TPCTransactionContext>
MessageStorePlugin::begin(const std::string& xid)
{
    return provider->second->begin(xid);
}

void
MessageStorePlugin::prepare(broker::TPCTransactionContext& ctxt)
{
    provider->second->prepare(ctxt);
}

void
MessageStorePlugin::commit(broker::TransactionContext& ctxt)
{
    provider->second->commit(ctxt);
}

void
MessageStorePlugin::abort(broker::TransactionContext& ctxt)
{
    provider->second->abort(ctxt);
}

void
MessageStorePlugin::collectPreparedXids(std::set<std::string>& xids)
{
    provider->second->collectPreparedXids(xids);
}

/**
 * Request recovery of queue and message state; inherited from Recoverable
 */
void
MessageStorePlugin::recover(broker::RecoveryManager& recoverer)
{
    ExchangeMap exchanges;
    QueueMap queues;
    MessageMap messages;
    MessageQueueMap messageQueueMap;

    provider->second->recoverConfigs(recoverer);
    provider->second->recoverExchanges(recoverer, exchanges);
    provider->second->recoverQueues(recoverer, queues);
    provider->second->recoverBindings(recoverer, exchanges, queues);
    provider->second->recoverMessages(recoverer, messages, messageQueueMap);
    // Enqueue msgs where needed.
    for (MessageQueueMap::const_iterator i = messageQueueMap.begin();
         i != messageQueueMap.end();
         ++i) {
        // Locate the message corresponding to the current message Id
        MessageMap::const_iterator iMsg = messages.find(i->first);
        if (iMsg == messages.end()) {
            std::ostringstream oss;
            oss << "No matching message trying to re-enqueue message "
                << i->first;
            THROW_STORE_EXCEPTION(oss.str());
        }
        broker::RecoverableMessage::shared_ptr msg = iMsg->second;
        // Now for each queue referenced in the queue map, locate it
        // and re-enqueue the message.
        for (std::vector<uint64_t>::const_iterator j = i->second.begin();
             j != i->second.end();
             ++j) {
            // Locate the queue corresponding to the current queue Id
            QueueMap::const_iterator iQ = queues.find(*j);
            if (iQ == queues.end()) {
                std::ostringstream oss;
                oss << "No matching queue trying to re-enqueue message "
                    << " on queue Id " << *j;
                THROW_STORE_EXCEPTION(oss.str());
            }
            iQ->second->recover(msg);
        }
    }

    // recoverTransactions() and apply correctly while re-enqueuing
}

}} // namespace qpid::store
