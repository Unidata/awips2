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

#include "qpid/broker/NullMessageStore.h"
#include "qpid/broker/MessageStoreModule.h"
#include "qpid/broker/RecoveryManager.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/reply_exceptions.h"

#include <iostream>

using boost::intrusive_ptr;

namespace qpid{
namespace broker{

const std::string nullxid = "";

class SimpleDummyCtxt : public TransactionContext {};

class DummyCtxt : public TPCTransactionContext
{
    const std::string xid;
public:
    DummyCtxt(const std::string& _xid) : xid(_xid) {}
    static std::string getXid(TransactionContext& ctxt)
    {
        DummyCtxt* c(dynamic_cast<DummyCtxt*>(&ctxt));
        return c ? c->xid : nullxid;
    }
};

NullMessageStore::NullMessageStore() : nextPersistenceId(1) {
    QPID_LOG(info, "No message store configured, persistence is disabled.");
}

bool NullMessageStore::init(const Options* /*options*/) {return true;}

void NullMessageStore::truncateInit(const bool /*pushDownStoreFiles*/) {}

void NullMessageStore::create(PersistableQueue& queue, const framing::FieldTable& /*args*/)
{
    queue.setPersistenceId(nextPersistenceId++);
}

void NullMessageStore::destroy(PersistableQueue&) {}

void NullMessageStore::create(const PersistableExchange& exchange, const framing::FieldTable& /*args*/)
{
    exchange.setPersistenceId(nextPersistenceId++);
}

void NullMessageStore::destroy(const PersistableExchange& ) {}

void NullMessageStore::bind(const PersistableExchange&, const PersistableQueue&, const std::string&, const framing::FieldTable&){}

void NullMessageStore::unbind(const PersistableExchange&, const PersistableQueue&, const std::string&, const framing::FieldTable&){}

void NullMessageStore::create(const PersistableConfig& config)
{
    config.setPersistenceId(nextPersistenceId++);
}

void NullMessageStore::destroy(const PersistableConfig&) {}

void NullMessageStore::recover(RecoveryManager&) {}

void NullMessageStore::stage(const intrusive_ptr<PersistableMessage>&) {}

void NullMessageStore::destroy(PersistableMessage&) {}

void NullMessageStore::appendContent(const intrusive_ptr<const PersistableMessage>&, const string&) {}

void NullMessageStore::loadContent(const qpid::broker::PersistableQueue&,
                                   const intrusive_ptr<const PersistableMessage>&,
                                   string&, uint64_t, uint32_t)
{
    throw qpid::framing::InternalErrorException("Can't load content; persistence not enabled");
}

void NullMessageStore::enqueue(TransactionContext*,
                               const intrusive_ptr<PersistableMessage>& msg,
                               const PersistableQueue&)
{
    msg->enqueueComplete();
}

void NullMessageStore::dequeue(TransactionContext*,
                               const intrusive_ptr<PersistableMessage>& msg,
                               const PersistableQueue&)
{
    msg->dequeueComplete();
}

void NullMessageStore::flush(const qpid::broker::PersistableQueue&) {}

uint32_t NullMessageStore::outstandingQueueAIO(const PersistableQueue& ) {
    return 0;
}

std::auto_ptr<TransactionContext> NullMessageStore::begin()
{
    return std::auto_ptr<TransactionContext>(new SimpleDummyCtxt());
}

std::auto_ptr<TPCTransactionContext> NullMessageStore::begin(const std::string& xid)
{
    return std::auto_ptr<TPCTransactionContext>(new DummyCtxt(xid));
}

void NullMessageStore::prepare(TPCTransactionContext& ctxt)
{
    prepared.insert(DummyCtxt::getXid(ctxt));
}

void NullMessageStore::commit(TransactionContext& ctxt)
{
    prepared.erase(DummyCtxt::getXid(ctxt));
}

void NullMessageStore::abort(TransactionContext& ctxt)
{
    prepared.erase(DummyCtxt::getXid(ctxt));
}

void NullMessageStore::collectPreparedXids(std::set<string>& out)
{
    out.insert(prepared.begin(), prepared.end());
}

bool NullMessageStore::isNull() const
{
    return true;
}

bool NullMessageStore::isNullStore(const MessageStore* store)
{
    const MessageStoreModule* wrapper = dynamic_cast<const MessageStoreModule*>(store);
    if (wrapper) {
        return wrapper->isNull();
    } else {
        const NullMessageStore* test = dynamic_cast<const NullMessageStore*>(store);
        return test && test->isNull();
    }
}

}}  // namespace qpid::broker
