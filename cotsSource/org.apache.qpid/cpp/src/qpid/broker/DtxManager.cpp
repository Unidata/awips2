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
#include "qpid/broker/DtxManager.h"
#include "qpid/broker/DtxTimeout.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/Timer.h"
#include "qpid/ptr_map.h"

#include <boost/format.hpp>
#include <iostream>

using boost::intrusive_ptr;
using qpid::sys::Mutex;
using qpid::ptr_map_ptr;
using namespace qpid::broker;
using namespace qpid::framing;

DtxManager::DtxManager(qpid::sys::Timer& t) : store(0), timer(t) {}

DtxManager::~DtxManager() {}

void DtxManager::start(const std::string& xid, DtxBuffer::shared_ptr ops)
{
    createWork(xid)->add(ops);
}

void DtxManager::join(const std::string& xid, DtxBuffer::shared_ptr ops)
{
    getWork(xid)->add(ops);
}

void DtxManager::recover(const std::string& xid, std::auto_ptr<TPCTransactionContext> txn, DtxBuffer::shared_ptr ops)
{
    createWork(xid)->recover(txn, ops);
}

bool DtxManager::prepare(const std::string& xid) 
{ 
    QPID_LOG(debug, "preparing: " << xid);
    try {
        return getWork(xid)->prepare();
    } catch (DtxTimeoutException& e) {
        remove(xid);
        throw e;
    }
}

bool DtxManager::commit(const std::string& xid, bool onePhase) 
{ 
    QPID_LOG(debug, "committing: " << xid);
    try {
        bool result = getWork(xid)->commit(onePhase);
        remove(xid);
        return result;
    } catch (DtxTimeoutException& e) {
        remove(xid);
        throw e;
    }
}

void DtxManager::rollback(const std::string& xid) 
{ 
    QPID_LOG(debug, "rolling back: " << xid);
    try {
        getWork(xid)->rollback();
        remove(xid);
    } catch (DtxTimeoutException& e) {
        remove(xid);
        throw e;
    }
}

DtxWorkRecord* DtxManager::getWork(const std::string& xid)
{
    Mutex::ScopedLock locker(lock); 
    WorkMap::iterator i = work.find(xid);
    if (i == work.end()) {
        throw NotFoundException(QPID_MSG("Unrecognised xid " << xid));
    }
    return ptr_map_ptr(i);
}

void DtxManager::remove(const std::string& xid)
{
    Mutex::ScopedLock locker(lock); 
    WorkMap::iterator i = work.find(xid);
    if (i == work.end()) {
        throw NotFoundException(QPID_MSG("Unrecognised xid " << xid));
    } else {
        work.erase(i);
    }
}

DtxWorkRecord* DtxManager::createWork(std::string xid)
{
    Mutex::ScopedLock locker(lock); 
    WorkMap::iterator i = work.find(xid);
    if (i != work.end()) {
        throw NotAllowedException(QPID_MSG("Xid " << xid << " is already known (use 'join' to add work to an existing xid)"));
    } else {
      return ptr_map_ptr(work.insert(xid, new DtxWorkRecord(xid, store)).first);
    }
}

void DtxManager::setTimeout(const std::string& xid, uint32_t secs)
{
    DtxWorkRecord* record = getWork(xid);
    intrusive_ptr<DtxTimeout> timeout = record->getTimeout();
    if (timeout.get()) {
        if (timeout->timeout == secs) return;//no need to do anything further if timeout hasn't changed
        timeout->cancel();
    }
    timeout = intrusive_ptr<DtxTimeout>(new DtxTimeout(secs, *this, xid));
    record->setTimeout(timeout);
    timer.add(timeout);
}

uint32_t DtxManager::getTimeout(const std::string& xid)
{
    intrusive_ptr<DtxTimeout> timeout = getWork(xid)->getTimeout();
    return !timeout ? 0 : timeout->timeout;
}

void DtxManager::timedout(const std::string& xid)
{
    Mutex::ScopedLock locker(lock); 
    WorkMap::iterator i = work.find(xid);
    if (i == work.end()) {
        QPID_LOG(warning, "Transaction timeout failed: no record for xid");
    } else {
        ptr_map_ptr(i)->timedout();
        //TODO: do we want to have a timed task to cleanup, or can we rely on an explicit completion?
        //timer.add(intrusive_ptr<TimerTask>(new DtxCleanup(60*30/*30 mins*/, *this, xid)));
    }
}

DtxManager::DtxCleanup::DtxCleanup(uint32_t _timeout, DtxManager& _mgr, const std::string& _xid) 
    : TimerTask(qpid::sys::Duration(_timeout * qpid::sys::TIME_SEC)), mgr(_mgr), xid(_xid) {}

void DtxManager::DtxCleanup::fire()
{
    try {
        mgr.remove(xid);
    } catch (ConnectionException& /*e*/) {
        //assume it was explicitly cleaned up after a call to prepare, commit or rollback
    }
}

void DtxManager::setStore (TransactionalStore* _store)
{
    store = _store;
}
