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
#ifndef _tests_TxMocks_h
#define _tests_TxMocks_h


#include "qpid/Exception.h"
#include "qpid/broker/TransactionalStore.h"
#include "qpid/broker/TxOp.h"
#include <iostream>
#include <vector>

using namespace qpid::broker;
using boost::static_pointer_cast;
using std::string;

namespace qpid {
namespace tests {

template <class T> void assertEqualVector(std::vector<T>& expected, std::vector<T>& actual){
    unsigned int i = 0;
    while(i < expected.size() && i < actual.size()){
        BOOST_CHECK_EQUAL(expected[i], actual[i]);
        i++;
    }
    if (i < expected.size()) {
        throw qpid::Exception(QPID_MSG("Missing " << expected[i]));
    } else if (i < actual.size()) {
        throw qpid::Exception(QPID_MSG("Extra " << actual[i]));
    }
    BOOST_CHECK_EQUAL(expected.size(), actual.size());
}

class TxOpConstants{
protected:
    const string PREPARE;
    const string COMMIT;
    const string ROLLBACK;

    TxOpConstants() : PREPARE("PREPARE"), COMMIT("COMMIT"), ROLLBACK("ROLLBACK") {}
};

class MockTxOp : public TxOp, public TxOpConstants{
    std::vector<string> expected;
    std::vector<string> actual;
    bool failOnPrepare;
    string debugName;
public:
    typedef boost::shared_ptr<MockTxOp> shared_ptr;

    MockTxOp() : failOnPrepare(false) {}
    MockTxOp(bool _failOnPrepare) : failOnPrepare(_failOnPrepare) {}

    void setDebugName(string name){
        debugName = name;
    }

    void printExpected(){
        std::cout << std::endl << "MockTxOp[" << debugName << "] expects: ";
        for (std::vector<string>::iterator i = expected.begin(); i < expected.end(); i++) {
            if(i != expected.begin()) std::cout << ", ";
            std::cout << *i;
        }
        std::cout << std::endl;
    }

    void printActual(){
        std::cout << std::endl << "MockTxOp[" << debugName << "] actual: ";
        for (std::vector<string>::iterator i = actual.begin(); i < actual.end(); i++) {
            if(i != actual.begin()) std::cout << ", ";
            std::cout << *i;
        }
        std::cout << std::endl;
    }

    bool prepare(TransactionContext*) throw(){
        actual.push_back(PREPARE);
        return !failOnPrepare;
    }
    void commit()  throw(){
        actual.push_back(COMMIT);
    }
    void rollback()  throw(){
        if(!debugName.empty()) std::cout << std::endl << "MockTxOp[" << debugName << "]::rollback()" << std::endl;
        actual.push_back(ROLLBACK);
    }
    MockTxOp& expectPrepare(){
        expected.push_back(PREPARE);
        return *this;
    }
    MockTxOp& expectCommit(){
        expected.push_back(COMMIT);
        return *this;
    }
    MockTxOp& expectRollback(){
        expected.push_back(ROLLBACK);
        return *this;
    }
    void check(){
        assertEqualVector(expected, actual);
    }

    void accept(TxOpConstVisitor&) const {}

    ~MockTxOp(){}
};

class MockTransactionalStore : public TransactionalStore{
    const string BEGIN;
    const string BEGIN2PC;
    const string PREPARE;
    const string COMMIT;
    const string ABORT;
    std::vector<string> expected;
    std::vector<string> actual;

    enum states {OPEN = 1, PREPARED = 2, COMMITTED = 3, ABORTED = 4};
    int state;

    class TestTransactionContext : public TPCTransactionContext{
        MockTransactionalStore* store;
    public:
        TestTransactionContext(MockTransactionalStore* _store) : store(_store) {}
        void prepare(){
            if(!store->isOpen()) throw "txn already completed";
            store->state = PREPARED;
        }

        void commit(){
            if(!store->isOpen() && !store->isPrepared()) throw "txn already completed";
            store->state = COMMITTED;
        }

        void abort(){
            if(!store->isOpen() && !store->isPrepared()) throw "txn already completed";
            store->state = ABORTED;
        }
        ~TestTransactionContext(){}
    };

public:
    MockTransactionalStore() :
            BEGIN("BEGIN"), BEGIN2PC("BEGIN2PC"), PREPARE("PREPARE"), COMMIT("COMMIT"), ABORT("ABORT"),  state(OPEN){}

    void collectPreparedXids(std::set<std::string>&)
    {
        throw "Operation not supported";
    }

    std::auto_ptr<TPCTransactionContext> begin(const std::string&){
        actual.push_back(BEGIN2PC);
        std::auto_ptr<TPCTransactionContext> txn(new TestTransactionContext(this));
        return txn;
    }
    std::auto_ptr<TransactionContext> begin(){
        actual.push_back(BEGIN);
        std::auto_ptr<TransactionContext> txn(new TestTransactionContext(this));
        return txn;
    }
    void prepare(TPCTransactionContext& ctxt){
        actual.push_back(PREPARE);
        dynamic_cast<TestTransactionContext&>(ctxt).prepare();
    }
    void commit(TransactionContext& ctxt){
        actual.push_back(COMMIT);
        dynamic_cast<TestTransactionContext&>(ctxt).commit();
    }
    void abort(TransactionContext& ctxt){
        actual.push_back(ABORT);
        dynamic_cast<TestTransactionContext&>(ctxt).abort();
    }
    MockTransactionalStore& expectBegin(){
        expected.push_back(BEGIN);
        return *this;
    }
    MockTransactionalStore& expectBegin2PC(){
        expected.push_back(BEGIN2PC);
        return *this;
    }
    MockTransactionalStore& expectPrepare(){
        expected.push_back(PREPARE);
        return *this;
    }
    MockTransactionalStore& expectCommit(){
        expected.push_back(COMMIT);
        return *this;
    }
    MockTransactionalStore& expectAbort(){
        expected.push_back(ABORT);
        return *this;
    }
    void check(){
        assertEqualVector(expected, actual);
    }

    bool isPrepared(){
        return state == PREPARED;
    }

    bool isCommitted(){
        return state == COMMITTED;
    }

    bool isAborted(){
        return state == ABORTED;
    }

    bool isOpen() const{
        return state == OPEN;
    }
    ~MockTransactionalStore(){}
};

}} // namespace qpid::tests

#endif
