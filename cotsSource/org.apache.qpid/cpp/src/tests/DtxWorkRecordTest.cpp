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
#include "qpid/broker/DtxWorkRecord.h"
#include "unit_test.h"
#include <iostream>
#include <vector>
#include "TxMocks.h"

using namespace qpid::broker;
using boost::static_pointer_cast;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(DtxWorkRecordTestSuite)

QPID_AUTO_TEST_CASE(testOnePhaseCommit){
    MockTransactionalStore store;
    store.expectBegin().expectCommit();

    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare().expectCommit();
    MockTxOp::shared_ptr opB(new MockTxOp());
    opB->expectPrepare().expectCommit();

    DtxBuffer::shared_ptr bufferA(new DtxBuffer());
    bufferA->enlist(static_pointer_cast<TxOp>(opA));
    bufferA->markEnded();
    DtxBuffer::shared_ptr bufferB(new DtxBuffer());
    bufferB->enlist(static_pointer_cast<TxOp>(opB));
    bufferB->markEnded();

    DtxWorkRecord work("my-xid", &store);
    work.add(bufferA);
    work.add(bufferB);

    work.commit(true);

    store.check();
    BOOST_CHECK(store.isCommitted());
    opA->check();
    opB->check();
}

QPID_AUTO_TEST_CASE(testFailOnOnePhaseCommit){
    MockTransactionalStore store;
    store.expectBegin().expectAbort();

    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare().expectRollback();
    MockTxOp::shared_ptr opB(new MockTxOp(true));
    opB->expectPrepare().expectRollback();
    MockTxOp::shared_ptr opC(new MockTxOp());
    opC->expectRollback();

    DtxBuffer::shared_ptr bufferA(new DtxBuffer());
    bufferA->enlist(static_pointer_cast<TxOp>(opA));
    bufferA->markEnded();
    DtxBuffer::shared_ptr bufferB(new DtxBuffer());
    bufferB->enlist(static_pointer_cast<TxOp>(opB));
    bufferB->markEnded();
    DtxBuffer::shared_ptr bufferC(new DtxBuffer());
    bufferC->enlist(static_pointer_cast<TxOp>(opC));
    bufferC->markEnded();

    DtxWorkRecord work("my-xid", &store);
    work.add(bufferA);
    work.add(bufferB);
    work.add(bufferC);

    work.commit(true);

    BOOST_CHECK(store.isAborted());
    store.check();

    opA->check();
    opB->check();
    opC->check();
}

QPID_AUTO_TEST_CASE(testTwoPhaseCommit){
    MockTransactionalStore store;
    store.expectBegin2PC().expectPrepare().expectCommit();

    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare().expectCommit();
    MockTxOp::shared_ptr opB(new MockTxOp());
    opB->expectPrepare().expectCommit();

    DtxBuffer::shared_ptr bufferA(new DtxBuffer());
    bufferA->enlist(static_pointer_cast<TxOp>(opA));
    bufferA->markEnded();
    DtxBuffer::shared_ptr bufferB(new DtxBuffer());
    bufferB->enlist(static_pointer_cast<TxOp>(opB));
    bufferB->markEnded();

    DtxWorkRecord work("my-xid", &store);
    work.add(bufferA);
    work.add(bufferB);

    BOOST_CHECK(work.prepare());
    BOOST_CHECK(store.isPrepared());
    work.commit(false);
    store.check();
    BOOST_CHECK(store.isCommitted());
    opA->check();
    opB->check();
}

QPID_AUTO_TEST_CASE(testFailOnTwoPhaseCommit){
    MockTransactionalStore store;
    store.expectBegin2PC().expectAbort();

    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare().expectRollback();
    MockTxOp::shared_ptr opB(new MockTxOp(true));
    opB->expectPrepare().expectRollback();
    MockTxOp::shared_ptr opC(new MockTxOp());
    opC->expectRollback();

    DtxBuffer::shared_ptr bufferA(new DtxBuffer());
    bufferA->enlist(static_pointer_cast<TxOp>(opA));
    bufferA->markEnded();
    DtxBuffer::shared_ptr bufferB(new DtxBuffer());
    bufferB->enlist(static_pointer_cast<TxOp>(opB));
    bufferB->markEnded();
    DtxBuffer::shared_ptr bufferC(new DtxBuffer());
    bufferC->enlist(static_pointer_cast<TxOp>(opC));
    bufferC->markEnded();

    DtxWorkRecord work("my-xid", &store);
    work.add(bufferA);
    work.add(bufferB);
    work.add(bufferC);

    BOOST_CHECK(!work.prepare());
    BOOST_CHECK(store.isAborted());
    store.check();
    opA->check();
    opB->check();
    opC->check();
}

QPID_AUTO_TEST_CASE(testRollback){
    MockTransactionalStore store;
    store.expectBegin2PC().expectPrepare().expectAbort();

    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare().expectRollback();
    MockTxOp::shared_ptr opB(new MockTxOp());
    opB->expectPrepare().expectRollback();

    DtxBuffer::shared_ptr bufferA(new DtxBuffer());
    bufferA->enlist(static_pointer_cast<TxOp>(opA));
    bufferA->markEnded();
    DtxBuffer::shared_ptr bufferB(new DtxBuffer());
    bufferB->enlist(static_pointer_cast<TxOp>(opB));
    bufferB->markEnded();

    DtxWorkRecord work("my-xid", &store);
    work.add(bufferA);
    work.add(bufferB);

    BOOST_CHECK(work.prepare());
    BOOST_CHECK(store.isPrepared());
    work.rollback();
    store.check();
    BOOST_CHECK(store.isAborted());
    opA->check();
    opB->check();
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
