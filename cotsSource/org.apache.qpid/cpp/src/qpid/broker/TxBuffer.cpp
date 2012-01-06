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
#include "qpid/broker/TxBuffer.h"
#include "qpid/log/Statement.h"

#include <boost/mem_fn.hpp>
#include <boost/bind.hpp>
using boost::mem_fn;
using namespace qpid::broker;

bool TxBuffer::prepare(TransactionContext* const ctxt)
{
    for(op_iterator i = ops.begin(); i < ops.end(); i++){
        if(!(*i)->prepare(ctxt)){
            return false;
        }
    }
    return true;
}

void TxBuffer::commit()
{
    std::for_each(ops.begin(), ops.end(), mem_fn(&TxOp::commit));
    ops.clear();
}

void TxBuffer::rollback()
{
    std::for_each(ops.begin(), ops.end(), mem_fn(&TxOp::rollback));
    ops.clear();
}

void TxBuffer::enlist(TxOp::shared_ptr op)
{
    ops.push_back(op);
}

bool TxBuffer::commitLocal(TransactionalStore* const store)
{
    if (!store) return false;
    try {
        std::auto_ptr<TransactionContext> ctxt = store->begin();
        if (prepare(ctxt.get())) {
            store->commit(*ctxt);
            commit();
            return true;
        } else {
            store->abort(*ctxt);
            rollback();
            return false;
        }
    } catch (std::exception& e) {
        QPID_LOG(error, "Commit failed with exception: " << e.what());
    } catch (...) {
        QPID_LOG(error, "Commit failed with unknown exception");
    }
    return false;
}

void TxBuffer::accept(TxOpConstVisitor& v) const {
    std::for_each(ops.begin(), ops.end(), boost::bind(&TxOp::accept, _1, boost::ref(v))); 
}
