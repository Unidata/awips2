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
#include "unit_test.h"
#include <iostream>
#include <vector>
#include "TxMocks.h"

using namespace qpid::broker;
using boost::static_pointer_cast;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(TxBufferTestSuite)

QPID_AUTO_TEST_CASE(testCommitLocal)
{
    MockTransactionalStore store;
    store.expectBegin().expectCommit();

    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare().expectCommit();
    MockTxOp::shared_ptr opB(new MockTxOp());
    opB->expectPrepare().expectPrepare().expectCommit().expectCommit();//opB enlisted twice to test relative order
    MockTxOp::shared_ptr opC(new MockTxOp());
    opC->expectPrepare().expectCommit();

    TxBuffer buffer;
    buffer.enlist(static_pointer_cast<TxOp>(opA));
    buffer.enlist(static_pointer_cast<TxOp>(opB));
    buffer.enlist(static_pointer_cast<TxOp>(opB));//opB enlisted twice
    buffer.enlist(static_pointer_cast<TxOp>(opC));

    BOOST_CHECK(buffer.commitLocal(&store));
    store.check();
    BOOST_CHECK(store.isCommitted());
    opA->check();
    opB->check();
    opC->check();
}

QPID_AUTO_TEST_CASE(testFailOnCommitLocal)
{
    MockTransactionalStore store;
    store.expectBegin().expectAbort();

    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare().expectRollback();
    MockTxOp::shared_ptr opB(new MockTxOp(true));
    opB->expectPrepare().expectRollback();
    MockTxOp::shared_ptr opC(new MockTxOp());//will never get prepare as b will fail
    opC->expectRollback();

    TxBuffer buffer;
    buffer.enlist(static_pointer_cast<TxOp>(opA));
    buffer.enlist(static_pointer_cast<TxOp>(opB));
    buffer.enlist(static_pointer_cast<TxOp>(opC));

    BOOST_CHECK(!buffer.commitLocal(&store));
    BOOST_CHECK(store.isAborted());
    store.check();
    opA->check();
    opB->check();
    opC->check();
}

QPID_AUTO_TEST_CASE(testPrepare)
{
    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare();
    MockTxOp::shared_ptr opB(new MockTxOp());
    opB->expectPrepare();
    MockTxOp::shared_ptr opC(new MockTxOp());
    opC->expectPrepare();

    TxBuffer buffer;
    buffer.enlist(static_pointer_cast<TxOp>(opA));
    buffer.enlist(static_pointer_cast<TxOp>(opB));
    buffer.enlist(static_pointer_cast<TxOp>(opC));

    BOOST_CHECK(buffer.prepare(0));
    opA->check();
    opB->check();
    opC->check();
}

QPID_AUTO_TEST_CASE(testFailOnPrepare)
{
    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectPrepare();
    MockTxOp::shared_ptr opB(new MockTxOp(true));
    opB->expectPrepare();
    MockTxOp::shared_ptr opC(new MockTxOp());//will never get prepare as b will fail

    TxBuffer buffer;
    buffer.enlist(static_pointer_cast<TxOp>(opA));
    buffer.enlist(static_pointer_cast<TxOp>(opB));
    buffer.enlist(static_pointer_cast<TxOp>(opC));

    BOOST_CHECK(!buffer.prepare(0));
    opA->check();
    opB->check();
    opC->check();
}

QPID_AUTO_TEST_CASE(testRollback)
{
    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectRollback();
    MockTxOp::shared_ptr opB(new MockTxOp(true));
    opB->expectRollback();
    MockTxOp::shared_ptr opC(new MockTxOp());
    opC->expectRollback();

    TxBuffer buffer;
    buffer.enlist(static_pointer_cast<TxOp>(opA));
    buffer.enlist(static_pointer_cast<TxOp>(opB));
    buffer.enlist(static_pointer_cast<TxOp>(opC));

    buffer.rollback();
    opA->check();
    opB->check();
    opC->check();
}

QPID_AUTO_TEST_CASE(testBufferIsClearedAfterRollback)
{
    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectRollback();
    MockTxOp::shared_ptr opB(new MockTxOp());
    opB->expectRollback();

    TxBuffer buffer;
    buffer.enlist(static_pointer_cast<TxOp>(opA));
    buffer.enlist(static_pointer_cast<TxOp>(opB));

    buffer.rollback();
    buffer.commit();//second call should not reach ops
    opA->check();
    opB->check();
}

QPID_AUTO_TEST_CASE(testBufferIsClearedAfterCommit)
{
    MockTxOp::shared_ptr opA(new MockTxOp());
    opA->expectCommit();
    MockTxOp::shared_ptr opB(new MockTxOp());
    opB->expectCommit();

    TxBuffer buffer;
    buffer.enlist(static_pointer_cast<TxOp>(opA));
    buffer.enlist(static_pointer_cast<TxOp>(opB));

    buffer.commit();
    buffer.rollback();//second call should not reach ops
    opA->check();
    opB->check();
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
