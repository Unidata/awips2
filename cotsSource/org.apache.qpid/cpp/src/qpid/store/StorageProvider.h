#ifndef QPID_STORE_STORAGEPROVIDER_H
#define QPID_STORE_STORAGEPROVIDER_H

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

#include <map>
#include <stdexcept>
#include <vector>
#include "qpid/Exception.h"
#include "qpid/Plugin.h"
#include "qpid/Options.h"
#include "qpid/broker/MessageStore.h"

using qpid::broker::PersistableConfig;
using qpid::broker::PersistableExchange;
using qpid::broker::PersistableMessage;
using qpid::broker::PersistableQueue;

namespace qpid {
namespace store {

typedef std::map<uint64_t, qpid::broker::RecoverableExchange::shared_ptr>
    ExchangeMap;
typedef std::map<uint64_t, qpid::broker::RecoverableQueue::shared_ptr>
    QueueMap;
typedef std::map<uint64_t, qpid::broker::RecoverableMessage::shared_ptr>
    MessageMap;
// Msg Id -> vector of queue Ids where message is queued
typedef std::map<uint64_t, std::vector<uint64_t> > MessageQueueMap;

class MessageStorePlugin;

/**
 * @class StorageProvider
 *
 * StorageProvider defines the interface for the storage provider plugin to the
 * Qpid broker persistence store plugin.
 *
 * @TODO Should StorageProvider also inherit from MessageStore? If so, then
 *       maybe remove Recoverable from MessageStore's inheritance and move it
 *       to MessageStorePlugin? In any event, somehow the discardInit() feature
 *       needs to get added here.
 */
class StorageProvider : public qpid::Plugin, public qpid::broker::MessageStore
{
public:

    class Exception : public qpid::Exception
    {
    public:
        virtual ~Exception() throw() {}
        virtual const char *what() const throw() = 0;
    };

    /**
     * @name Methods inherited from qpid::Plugin
     */
    //@{
    /**
     * Return a pointer to the provider's options. The options will be
     * updated during option parsing by the host program; therefore, the
     * referenced Options object must remain valid past this function's return.
     * 
     * @return An options group or 0 for no options. Default returns 0.
     * Plugin retains ownership of return value.
     */
    virtual qpid::Options* getOptions() = 0;

    /**
     * Initialize Plugin functionality on a Target, called before
     * initializing the target.
     *
     * StorageProviders should respond only to Targets of class
     * qpid::store::MessageStorePlugin and ignore all others.
     *
     * When called, the provider should invoke the method
     * qpid::store::MessageStorePlugin::providerAvailable() to alert the
     * message store of StorageProvider's availability.
     *
     * Called before the target itself is initialized.
     */
    virtual void earlyInitialize (Plugin::Target& target) = 0;

    /**
     * Initialize StorageProvider functionality. Called after initializing
     * the target.
     * 
     * StorageProviders should respond only to Targets of class
     * qpid::store::MessageStorePlugin and ignore all others.
     *
     * Called after the target is fully initialized.
     */
    virtual void initialize(Plugin::Target& target) = 0;
    //@}

    /**
     * Receive notification that this provider is the one that will actively
     * handle storage for the target. If the provider is to be used, this
     * method will be called after earlyInitialize() and before any
     * recovery operations (recovery, in turn, precedes call to initialize()).
     * Thus, it is wise to not actually do any database ops from within
     * earlyInitialize() - they can wait until activate() is called because
     * at that point it is certain the database will be needed.
     */
    virtual void activate(MessageStorePlugin &store) = 0;

    /**
     * @name Methods inherited from qpid::broker::MessageStore
     */
    //@{
    /**
     * If called after init() but before recovery, will discard the database
     * and reinitialize using an empty store dir. If @a pushDownStoreFiles
     * is true, the content of the store dir will be moved to a backup dir
     * inside the store dir. This is used when cluster nodes recover and must
     * get thier content from a cluster sync rather than directly fromt the
     * store.
     *
     * @param pushDownStoreFiles If true, will move content of the store dir
     *                           into a subdir, leaving the store dir
     *                           otherwise empty.
     */
    virtual void truncateInit(const bool pushDownStoreFiles = false) = 0;

    /**
     * Record the existence of a durable queue
     */
    virtual void create(PersistableQueue& queue,
                        const qpid::framing::FieldTable& args) = 0;
    /**
     * Destroy a durable queue
     */
    virtual void destroy(PersistableQueue& queue) = 0;

    /**
     * Record the existence of a durable exchange
     */
    virtual void create(const PersistableExchange& exchange,
                        const qpid::framing::FieldTable& args) = 0;
    /**
     * Destroy a durable exchange
     */
    virtual void destroy(const PersistableExchange& exchange) = 0;

    /**
     * Record a binding
     */
    virtual void bind(const PersistableExchange& exchange,
                      const PersistableQueue& queue,
                      const std::string& key,
                      const qpid::framing::FieldTable& args) = 0;

    /**
     * Forget a binding
     */
    virtual void unbind(const PersistableExchange& exchange,
                        const PersistableQueue& queue,
                        const std::string& key,
                        const qpid::framing::FieldTable& args) = 0;

    /**
     * Record generic durable configuration
     */
    virtual void create(const PersistableConfig& config) = 0;

    /**
     * Destroy generic durable configuration
     */
    virtual void destroy(const PersistableConfig& config) = 0;

    /**
     * Stores a messages before it has been enqueued
     * (enqueueing automatically stores the message so this is
     * only required if storage is required prior to that
     * point). If the message has not yet been stored it will
     * store the headers as well as any content passed in. A
     * persistence id will be set on the message which can be
     * used to load the content or to append to it.
     */
    virtual void stage(const boost::intrusive_ptr<PersistableMessage>& msg) = 0;

    /**
     * Destroys a previously staged message. This only needs
     * to be called if the message is never enqueued. (Once
     * enqueued, deletion will be automatic when the message
     * is dequeued from all queues it was enqueued onto).
     */
    virtual void destroy(PersistableMessage& msg) = 0;

    /**
     * Appends content to a previously staged message
     */
    virtual void appendContent(const boost::intrusive_ptr<const PersistableMessage>& msg,
                               const std::string& data) = 0;

    /**
     * Loads (a section) of content data for the specified
     * message (previously stored through a call to stage or
     * enqueue) into data. The offset refers to the content
     * only (i.e. an offset of 0 implies that the start of the
     * content should be loaded, not the headers or related
     * meta-data).
     */
    virtual void loadContent(const PersistableQueue& queue,
                             const boost::intrusive_ptr<const PersistableMessage>& msg,
                             std::string& data,
                             uint64_t offset,
                             uint32_t length) = 0;

    /**
     * Enqueues a message, storing the message if it has not
     * been previously stored and recording that the given
     * message is on the given queue.
     *
     * Note: that this is async so the return of the function does
     * not mean the opperation is complete.
     *
     * @param msg the message to enqueue
     * @param queue the name of the queue onto which it is to be enqueued
     * @param xid (a pointer to) an identifier of the
     * distributed transaction in which the operation takes
     * place or null for 'local' transactions
     */
    virtual void enqueue(qpid::broker::TransactionContext* ctxt,
                         const boost::intrusive_ptr<PersistableMessage>& msg,
                         const PersistableQueue& queue) = 0;

    /**
     * Dequeues a message, recording that the given message is
     * no longer on the given queue and deleting the message
     * if it is no longer on any other queue.
     *
     * Note: that this is async so the return of the function does
     * not mean the opperation is complete.
     *
     * @param msg the message to dequeue
     * @param queue the name of the queue from which it is to be dequeued
     * @param xid (a pointer to) an identifier of the
     * distributed transaction in which the operation takes
     * place or null for 'local' transactions
     */
    virtual void dequeue(qpid::broker::TransactionContext* ctxt,
                         const boost::intrusive_ptr<PersistableMessage>& msg,
                         const PersistableQueue& queue) = 0;

    /**
     * Flushes all async messages to disk for the specified queue
     *
     * Note: that this is async so the return of the function does
     * not mean the opperation is complete.
     *
     * @param queue the name of the queue from which it is to be dequeued
     */
    virtual void flush(const qpid::broker::PersistableQueue& queue) = 0;

    /**
     * Returns the number of outstanding AIO's for a given queue
     *
     * If 0, than all the enqueue / dequeues have been stored
     * to disk
     *
     * @param queue the name of the queue to check for outstanding AIO
     */
    virtual uint32_t outstandingQueueAIO(const PersistableQueue& queue) = 0;
    //@}

    /**
     * @TODO This should probably not be here - it's only here because
     * MessageStore inherits from Recoverable... maybe move that derivation.
     *
     * As it is now, we don't use this. Separate recover methods are
     * declared below for individual types, which also set up maps of
     * messages, queues, transactions for the main store plugin to handle
     * properly.
     *
     * Request recovery of queue and message state.
     */
    virtual void recover(qpid::broker::RecoveryManager& /*recoverer*/) {}

    /**
     * @name Methods that do the recovery of the various objects that
     * were saved.
     */
    //@{

    /**
     * Recover bindings.
     */
    virtual void recoverConfigs(qpid::broker::RecoveryManager& recoverer) = 0;
    virtual void recoverExchanges(qpid::broker::RecoveryManager& recoverer,
                                  ExchangeMap& exchangeMap) = 0;
    virtual void recoverQueues(qpid::broker::RecoveryManager& recoverer,
                               QueueMap& queueMap) = 0;
    virtual void recoverBindings(qpid::broker::RecoveryManager& recoverer,
                                 const ExchangeMap& exchangeMap,
                                 const QueueMap& queueMap) = 0;
    virtual void recoverMessages(qpid::broker::RecoveryManager& recoverer,
                                 MessageMap& messageMap,
                                 MessageQueueMap& messageQueueMap) = 0;
    //@}
};

}} // namespace qpid::store

#endif /* QPID_STORE_STORAGEPROVIDER_H */
