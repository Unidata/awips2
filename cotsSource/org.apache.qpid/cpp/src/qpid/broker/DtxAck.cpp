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
#include "qpid/broker/DtxAck.h"
#include "qpid/log/Statement.h"

using std::bind1st;
using std::bind2nd;
using std::mem_fun_ref;
using namespace qpid::broker;

DtxAck::DtxAck(const qpid::framing::SequenceSet& acked, DeliveryRecords& unacked)
{
    remove_copy_if(unacked.begin(), unacked.end(), inserter(pending, pending.end()), 
                   not1(bind2nd(mem_fun_ref(&DeliveryRecord::coveredBy), &acked)));
}

bool DtxAck::prepare(TransactionContext* ctxt) throw()
{
    try{
        //record dequeue in the store
        for (DeliveryRecords::iterator i = pending.begin(); i != pending.end(); i++) {
            i->dequeue(ctxt);
        }
        return true;
    }catch(...){
        QPID_LOG(error, "Failed to prepare");
        return false;
    }
}

void DtxAck::commit() throw()
{
    try {
        for_each(pending.begin(), pending.end(), mem_fun_ref(&DeliveryRecord::committed));
        pending.clear();
    } catch (const std::exception& e) {
        QPID_LOG(error, "Failed to commit: " << e.what());
    } catch(...) {
        QPID_LOG(error, "Failed to commit (unknown error)");
    }

}

void DtxAck::rollback() throw()
{
    try {
        for_each(pending.begin(), pending.end(), mem_fun_ref(&DeliveryRecord::requeue));
        pending.clear();
    } catch (const std::exception& e) {
        QPID_LOG(error, "Failed to complete rollback: " << e.what());
    } catch(...) {
        QPID_LOG(error, "Failed to complete rollback (unknown error)");
    }

}
