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
#ifndef _TransactionalStore_
#define _TransactionalStore_

#include <memory>
#include <string>
#include <set>

namespace qpid {
namespace broker {

struct InvalidTransactionContextException : public std::exception {};

class TransactionContext {
public:
    virtual ~TransactionContext(){}
};

class TPCTransactionContext : public TransactionContext {
public:
    virtual ~TPCTransactionContext(){}
};

class TransactionalStore {
public:
    virtual std::auto_ptr<TransactionContext> begin() = 0;
    virtual std::auto_ptr<TPCTransactionContext> begin(const std::string& xid) = 0;
    virtual void prepare(TPCTransactionContext& txn) = 0;
    virtual void commit(TransactionContext& txn) = 0;
    virtual void abort(TransactionContext& txn) = 0;

    virtual void collectPreparedXids(std::set<std::string>& xids) = 0;
    
    virtual ~TransactionalStore(){}
};

}
}


#endif
