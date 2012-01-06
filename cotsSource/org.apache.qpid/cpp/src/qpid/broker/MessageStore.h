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
#ifndef _MessageStore_
#define _MessageStore_

#include "qpid/broker/PersistableExchange.h"
#include "qpid/broker/PersistableMessage.h"
#include "qpid/broker/PersistableQueue.h"
#include "qpid/broker/PersistableConfig.h"
#include "qpid/broker/RecoveryManager.h"
#include "qpid/broker/TransactionalStore.h"
#include "qpid/framing/FieldTable.h"

#include <qpid/Options.h>

#include <boost/shared_ptr.hpp>
#include <boost/intrusive_ptr.hpp>

namespace qpid {
namespace broker {

/**
 * An abstraction of the persistent storage for messages. (In
 * all methods, any pointers/references to queues or messages
 * are valid only for the duration of the call).
 */
class MessageStore : public TransactionalStore, public Recoverable {
  public:

    /**
     * If called after initialization but before recovery, will discard the database
     * and reinitialize using an empty store dir. If the parameter pushDownStoreFiles
     * is true, the content of the store dir will be moved to a backup dir inside the
     * store dir. This is used when cluster nodes recover and must get thier content
     * from a cluster sync rather than directly fromt the store.
     *
     * @param pushDownStoreFiles If true, will move content of the store dir into a
     *                           subdir, leaving the store dir otherwise empty.
     */
    virtual void truncateInit(const bool pushDownStoreFiles = false) = 0;

    /**
     * Record the existence of a durable queue
     */
    virtual void create(PersistableQueue& queue,
                        const framing::FieldTable& args) = 0;
    /**
     * Destroy a durable queue
     */
    virtual void destroy(PersistableQueue& queue) = 0;

    /**
     * Record the existence of a durable exchange
     */
    virtual void create(const PersistableExchange& exchange,
                        const framing::FieldTable& args) = 0;
    /**
     * Destroy a durable exchange
     */
    virtual void destroy(const PersistableExchange& exchange) = 0;

    /**
     * Record a binding
     */
    virtual void bind(const PersistableExchange& exchange, const PersistableQueue& queue,
                      const std::string& key, const framing::FieldTable& args) = 0;

    /**
     * Forget a binding
     */
    virtual void unbind(const PersistableExchange& exchange, const PersistableQueue& queue,
                        const std::string& key, const framing::FieldTable& args) = 0;

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
    virtual void loadContent(const qpid::broker::PersistableQueue& queue,
                             const boost::intrusive_ptr<const PersistableMessage>& msg,
                             std::string& data, uint64_t offset, uint32_t length) = 0;

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
    virtual void enqueue(TransactionContext* ctxt,
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
    virtual void dequeue(TransactionContext* ctxt,
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
    virtual void flush(const qpid::broker::PersistableQueue& queue)=0;

    /**
     * Returns the number of outstanding AIO's for a given queue
     *
     * If 0, than all the enqueue / dequeues have been stored
     * to disk
     *
     * @param queue the name of the queue to check for outstanding AIO
     */
    virtual uint32_t outstandingQueueAIO(const PersistableQueue& queue) = 0;


    virtual ~MessageStore(){}
};

}
}


#endif
