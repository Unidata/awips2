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
#ifndef _NullMessageStore_
#define _NullMessageStore_

#include <set>
#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/MessageStore.h"
#include "qpid/broker/Queue.h"

#include <boost/intrusive_ptr.hpp>

namespace qpid {
namespace broker {

/**
 * A null implementation of the MessageStore interface
 */
class NullMessageStore : public MessageStore
{
    std::set<std::string> prepared;
    uint64_t nextPersistenceId;
  public:
    QPID_BROKER_EXTERN NullMessageStore();

    QPID_BROKER_EXTERN virtual bool init(const Options* options);
    QPID_BROKER_EXTERN virtual void truncateInit(const bool pushDownStoreFiles = false);
    QPID_BROKER_EXTERN virtual std::auto_ptr<TransactionContext> begin();
    QPID_BROKER_EXTERN virtual std::auto_ptr<TPCTransactionContext> begin(const std::string& xid);
    QPID_BROKER_EXTERN virtual void prepare(TPCTransactionContext& txn);
    QPID_BROKER_EXTERN virtual void commit(TransactionContext& txn);
    QPID_BROKER_EXTERN virtual void abort(TransactionContext& txn);
    QPID_BROKER_EXTERN virtual void collectPreparedXids(std::set<std::string>& xids);

    QPID_BROKER_EXTERN virtual void create(PersistableQueue& queue,
                                           const framing::FieldTable& args);
    QPID_BROKER_EXTERN virtual void destroy(PersistableQueue& queue);
    QPID_BROKER_EXTERN virtual void create(const PersistableExchange& exchange,
                                           const framing::FieldTable& args);
    QPID_BROKER_EXTERN virtual void destroy(const PersistableExchange& exchange);

    QPID_BROKER_EXTERN virtual void bind(const PersistableExchange& exchange,
                                         const PersistableQueue& queue,
                                         const std::string& key,
                                         const framing::FieldTable& args);
    QPID_BROKER_EXTERN virtual void unbind(const PersistableExchange& exchange,
                                           const PersistableQueue& queue,
                                           const std::string& key,
                                           const framing::FieldTable& args);
    QPID_BROKER_EXTERN virtual void create(const PersistableConfig& config);
    QPID_BROKER_EXTERN virtual void destroy(const PersistableConfig& config);
    QPID_BROKER_EXTERN virtual void recover(RecoveryManager& queues);
    QPID_BROKER_EXTERN virtual void stage(const boost::intrusive_ptr<PersistableMessage>& msg);
    QPID_BROKER_EXTERN virtual void destroy(PersistableMessage& msg);
    QPID_BROKER_EXTERN virtual void appendContent(const boost::intrusive_ptr<const PersistableMessage>& msg,
                                                  const std::string& data);
    QPID_BROKER_EXTERN virtual void loadContent(const qpid::broker::PersistableQueue& queue,
                                                const boost::intrusive_ptr<const PersistableMessage>& msg,
                                                std::string& data,
                                                uint64_t offset,
                                                uint32_t length);
    QPID_BROKER_EXTERN virtual void enqueue(TransactionContext* ctxt,
                                            const boost::intrusive_ptr<PersistableMessage>& msg,
                                            const PersistableQueue& queue);
    QPID_BROKER_EXTERN virtual void dequeue(TransactionContext* ctxt,
                                            const boost::intrusive_ptr<PersistableMessage>& msg,
                                            const PersistableQueue& queue);
    QPID_BROKER_EXTERN virtual uint32_t outstandingQueueAIO(const PersistableQueue& queue);
    QPID_BROKER_EXTERN virtual void flush(const qpid::broker::PersistableQueue& queue);
    ~NullMessageStore(){}

    QPID_BROKER_EXTERN virtual bool isNull() const;
    static bool isNullStore(const MessageStore*);
};

}
}


#endif
