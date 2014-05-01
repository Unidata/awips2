#ifndef QPID_STORE_MESSAGESTOREPLUGIN_H
#define QPID_STORE_MESSAGESTOREPLUGIN_H

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
#include "qpid/broker/Broker.h"
#include "qpid/broker/MessageStore.h"
#include "qpid/broker/PersistableExchange.h"
#include "qpid/broker/PersistableMessage.h"
#include "qpid/broker/PersistableQueue.h"
#include "qpid/management/Manageable.h"

#include <string>

using namespace qpid;

namespace qpid {
namespace store {

class StorageProvider;

/**
 * @class MessageStorePlugin
 *
 * MessageStorePlugin is the front end of the persistent message store
 * plugin. It is responsible for coordinating recovery, initialization,
 * transactions (both local and distributed), flow-to-disk loading and
 * unloading and persisting broker state (queues, bindings etc.).
 * Actual storage operations are carried out by a message store storage
 * provider that implements the qpid::store::StorageProvider interface.
 */
class MessageStorePlugin :
    public qpid::Plugin,
    public qpid::broker::MessageStore,        // Frontend classes
    public qpid::Plugin::Target               // Provider target
    // @TODO Need a mgmt story for this. Maybe allow r/o access to provider store info?    public qpid::management::Manageable
{
  public:
    MessageStorePlugin() {}

    /**
     * @name Methods inherited from qpid::Plugin
     */
    //@{
    virtual Options* getOptions() { return &options; }
    virtual void earlyInitialize (Plugin::Target& target);
    virtual void initialize(Plugin::Target& target);
    //@}

    /// Finalizer; calls Target::finalize() to run finalizers on
    /// StorageProviders.
    void finalizeMe();

    /**
     * Called by StorageProvider instances during the earlyInitialize sequence.
     * Each StorageProvider must supply a unique name by which it is known and a
     * pointer to itself.
     */
    virtual void providerAvailable(const std::string name, StorageProvider *be);

    /**
     * @name Methods inherited from qpid::broker::MessageStore
     */
    //@{
    /**
     * If called before recovery, will discard the database and reinitialize
     * using an empty store. This is used when cluster nodes recover and
     * must get their content from a cluster sync rather than directly from
     * the store.
     *
     * @param saveStoreContent    If true, the store's contents should be
     *                            saved to a backup location before
     *                            reinitializing the store content.
     */
    virtual void truncateInit(const bool saveStoreContent = false);

    /**
     * Record the existence of a durable queue
     */
    virtual void create(broker::PersistableQueue& queue,
                        const framing::FieldTable& args);
    /**
     * Destroy a durable queue
     */
    virtual void destroy(broker::PersistableQueue& queue);

    /**
     * Record the existence of a durable exchange
     */
    virtual void create(const broker::PersistableExchange& exchange,
                        const framing::FieldTable& args);
    /**
     * Destroy a durable exchange
     */
    virtual void destroy(const broker::PersistableExchange& exchange);

    /**
     * Record a binding
     */
    virtual void bind(const broker::PersistableExchange& exchange,
                      const broker::PersistableQueue& queue,
                      const std::string& key,
                      const framing::FieldTable& args);

    /**
     * Forget a binding
     */
    virtual void unbind(const broker::PersistableExchange& exchange,
                        const broker::PersistableQueue& queue,
                        const std::string& key,
                        const framing::FieldTable& args);

    /**
     * Record generic durable configuration
     */
    virtual void create(const broker::PersistableConfig& config);

    /**
     * Destroy generic durable configuration
     */
    virtual void destroy(const broker::PersistableConfig& config);

    /**
     * Stores a message before it has been enqueued
     * (enqueueing automatically stores the message so this is
     * only required if storage is required prior to that
     * point). If the message has not yet been stored it will
     * store the headers as well as any content passed in. A
     * persistence id will be set on the message which can be
     * used to load the content or to append to it.
     */
    virtual void stage(const boost::intrusive_ptr<broker::PersistableMessage>& msg);

    /**
     * Destroys a previously staged message. This only needs
     * to be called if the message is never enqueued. (Once
     * enqueued, deletion will be automatic when the message
     * is dequeued from all queues it was enqueued onto).
     */
    virtual void destroy(broker::PersistableMessage& msg);

    /**
     * Appends content to a previously staged message
     */
    virtual void appendContent(const boost::intrusive_ptr<const broker::PersistableMessage>& msg,
                               const std::string& data);

    /**
     * Loads (a section) of content data for the specified
     * message (previously stored through a call to stage or
     * enqueue) into data. The offset refers to the content
     * only (i.e. an offset of 0 implies that the start of the
     * content should be loaded, not the headers or related
     * meta-data).
     */
    virtual void loadContent(const broker::PersistableQueue& queue,
                             const boost::intrusive_ptr<const broker::PersistableMessage>& msg,
                             std::string& data,
                             uint64_t offset,
                             uint32_t length);

    /**
     * Enqueues a message, storing the message if it has not
     * been previously stored and recording that the given
     * message is on the given queue.
     *
     * Note: The operation is asynchronous so the return of this function does
     * not mean the operation is complete.
     *
     * @param msg the message to enqueue
     * @param queue the name of the queue onto which it is to be enqueued
     * @param xid (a pointer to) an identifier of the
     * distributed transaction in which the operation takes
     * place or null for 'local' transactions
     */
    virtual void enqueue(broker::TransactionContext* ctxt,
                         const boost::intrusive_ptr<broker::PersistableMessage>& msg,
                         const broker::PersistableQueue& queue);

    /**
     * Dequeues a message, recording that the given message is
     * no longer on the given queue and deleting the message
     * if it is no longer on any other queue.
     *
     *
     * Note: The operation is asynchronous so the return of this function does
     * not mean the operation is complete.
     *
     * @param msg the message to dequeue
     * @param queue the name of the queue from which it is to be dequeued
     * @param xid (a pointer to) an identifier of the
     * distributed transaction in which the operation takes
     * place or null for 'local' transactions
     */
    virtual void dequeue(broker::TransactionContext* ctxt,
                         const boost::intrusive_ptr<broker::PersistableMessage>& msg,
                         const broker::PersistableQueue& queue);

    /**
     * Flushes all async messages to disk for the specified queue
     *
     *
     * Note: The operation is asynchronous so the return of this function does
     * not mean the operation is complete.
     *
     * @param queue the name of the queue from which it is to be dequeued
     */
    virtual void flush(const broker::PersistableQueue& queue);

    /**
     * Returns the number of outstanding AIO's for a given queue
     *
     * If 0, than all the enqueue / dequeues have been stored
     * to disk
     *
     * @param queue the name of the queue to check for outstanding AIO
     */
    virtual uint32_t outstandingQueueAIO(const broker::PersistableQueue& queue);
    //@}

    /**
     * @name Methods inherited from qpid::broker::TransactionalStore
     */
    //@{
    std::auto_ptr<broker::TransactionContext> begin();

    std::auto_ptr<broker::TPCTransactionContext> begin(const std::string& xid);

    void prepare(broker::TPCTransactionContext& ctxt);

    void commit(broker::TransactionContext& ctxt);

    void abort(broker::TransactionContext& ctxt);

    void collectPreparedXids(std::set<std::string>& xids);
    //@}

    /**
     * Request recovery of queue and message state; inherited from Recoverable
     */
    virtual void recover(broker::RecoveryManager& recoverer);

    //    inline management::Manageable::status_t ManagementMethod (uint32_t, management::Args&, std::string&)
    //        { return management::Manageable::STATUS_OK; }

  protected:

    struct StoreOptions : public qpid::Options {
        StoreOptions(const std::string& name="Store Options");
        std::string providerName;
    };
    StoreOptions options;

    typedef std::map<const std::string, StorageProvider*> ProviderMap;
    ProviderMap providers;
    ProviderMap::const_iterator provider;

}; // class MessageStoreImpl

} // namespace msgstore
} // namespace mrg

#endif /* QPID_SERIALIZER_H */
