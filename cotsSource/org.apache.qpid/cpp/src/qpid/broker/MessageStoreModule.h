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
#ifndef _MessageStoreModule_
#define _MessageStoreModule_

#include "qpid/broker/MessageStore.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/RecoveryManager.h"

#include <boost/intrusive_ptr.hpp>
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace broker {

/**
 * A null implementation of the MessageStore interface
 */
class MessageStoreModule : public MessageStore
{
    boost::shared_ptr<MessageStore> store;
  public:
    MessageStoreModule(boost::shared_ptr<MessageStore>& store);

    bool init(const Options* options);
    void truncateInit(const bool pushDownStoreFiles = false);
    std::auto_ptr<TransactionContext> begin();
    std::auto_ptr<TPCTransactionContext> begin(const std::string& xid);
    void prepare(TPCTransactionContext& txn);
    void commit(TransactionContext& txn);
    void abort(TransactionContext& txn);
    void collectPreparedXids(std::set<std::string>& xids);

    void create(PersistableQueue& queue, const framing::FieldTable& args);
    void destroy(PersistableQueue& queue);
    void create(const PersistableExchange& exchange, const framing::FieldTable& args);
    void destroy(const PersistableExchange& exchange);
    void bind(const PersistableExchange& exchange, const PersistableQueue& queue,
              const std::string& key, const framing::FieldTable& args);
    void unbind(const PersistableExchange& exchange, const PersistableQueue& queue,
                const std::string& key, const framing::FieldTable& args);
    void create(const PersistableConfig& config);
    void destroy(const PersistableConfig& config);
    void recover(RecoveryManager& queues);
    void stage(const boost::intrusive_ptr<PersistableMessage>& msg);
    void destroy(PersistableMessage& msg);
    void appendContent(const boost::intrusive_ptr<const PersistableMessage>& msg, const std::string& data);
    void loadContent(const qpid::broker::PersistableQueue& queue,
                     const boost::intrusive_ptr<const PersistableMessage>& msg, std::string& data,
                     uint64_t offset, uint32_t length);

    void enqueue(TransactionContext* ctxt,
                 const boost::intrusive_ptr<PersistableMessage>& msg,
                 const PersistableQueue& queue);
    void dequeue(TransactionContext* ctxt,
                 const boost::intrusive_ptr<PersistableMessage>& msg,
                 const PersistableQueue& queue);
    uint32_t outstandingQueueAIO(const PersistableQueue& queue);
    void flush(const qpid::broker::PersistableQueue& queue);
    bool isNull() const;

    ~MessageStoreModule();
};

}
}


#endif
