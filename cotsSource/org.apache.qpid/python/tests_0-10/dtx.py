#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
#   http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
from qpid.client import Client, Closed
from qpid.queue import Empty
from qpid.datatypes import Message, RangedSet
from qpid.session import SessionException
from qpid.testlib import TestBase010
from qpid.compat import set
from struct import pack, unpack
from time import sleep

class DtxTests(TestBase010):
    """
    Tests for the amqp dtx related classes.

    Tests of the form test_simple_xxx test the basic transactional
    behaviour. The approach here is to 'swap' a message from one queue
    to another by consuming and re-publishing in the same
    transaction. That transaction is then completed in different ways
    and the appropriate result verified.

    The other tests enforce more specific rules and behaviour on a
    per-method or per-field basis.        
    """

    XA_RBROLLBACK = 1
    XA_RBTIMEOUT = 2
    XA_OK = 0
    tx_counter = 0

    def reset_channel(self):
        self.session.close()
        self.session = self.conn.session("dtx-session", 1)

    def test_simple_commit(self):
        """        
        Test basic one-phase commit behaviour.     
        """
        guard = self.keepQueuesAlive(["queue-a", "queue-b"])
        session = self.session
        tx = self.xid("my-xid")
        self.txswap(tx, "commit")

        #neither queue should have any messages accessible
        self.assertMessageCount(0, "queue-a")
        self.assertMessageCount(0, "queue-b")

        #commit
        self.assertEqual(self.XA_OK, session.dtx_commit(xid=tx, one_phase=True).status)

        #should close and reopen session to ensure no unacked messages are held
        self.reset_channel()

        #check result
        self.assertMessageCount(0, "queue-a")
        self.assertMessageCount(1, "queue-b")
        self.assertMessageId("commit", "queue-b")

    def test_simple_prepare_commit(self):
        """        
        Test basic two-phase commit behaviour.     
        """
        guard = self.keepQueuesAlive(["queue-a", "queue-b"])
        session = self.session
        tx = self.xid("my-xid")
        self.txswap(tx, "prepare-commit")

        #prepare
        self.assertEqual(self.XA_OK, session.dtx_prepare(xid=tx).status)

        #neither queue should have any messages accessible
        self.assertMessageCount(0, "queue-a")
        self.assertMessageCount(0, "queue-b")

        #commit
        self.assertEqual(self.XA_OK, session.dtx_commit(xid=tx, one_phase=False).status)

        self.reset_channel()

        #check result
        self.assertMessageCount(0, "queue-a")
        self.assertMessageCount(1, "queue-b")
        self.assertMessageId("prepare-commit", "queue-b")


    def test_simple_rollback(self):
        """        
        Test basic rollback behaviour.     
        """
        guard = self.keepQueuesAlive(["queue-a", "queue-b"])
        session = self.session
        tx = self.xid("my-xid")
        self.txswap(tx, "rollback")

        #neither queue should have any messages accessible
        self.assertMessageCount(0, "queue-a")
        self.assertMessageCount(0, "queue-b")

        #rollback
        self.assertEqual(self.XA_OK, session.dtx_rollback(xid=tx).status)

        self.reset_channel()

        #check result
        self.assertMessageCount(1, "queue-a")
        self.assertMessageCount(0, "queue-b")
        self.assertMessageId("rollback", "queue-a")

    def test_simple_prepare_rollback(self):
        """        
        Test basic rollback behaviour after the transaction has been prepared.     
        """
        guard = self.keepQueuesAlive(["queue-a", "queue-b"])
        session = self.session
        tx = self.xid("my-xid")
        self.txswap(tx, "prepare-rollback")

        #prepare
        self.assertEqual(self.XA_OK, session.dtx_prepare(xid=tx).status)

        #neither queue should have any messages accessible
        self.assertMessageCount(0, "queue-a")
        self.assertMessageCount(0, "queue-b")

        #rollback
        self.assertEqual(self.XA_OK, session.dtx_rollback(xid=tx).status)

        self.reset_channel()

        #check result
        self.assertMessageCount(1, "queue-a")
        self.assertMessageCount(0, "queue-b")
        self.assertMessageId("prepare-rollback", "queue-a")    

    def test_select_required(self):
        """
        check that an error is flagged if select is not issued before
        start or end        
        """
        session = self.session
        tx = self.xid("dummy")
        try:
            session.dtx_start(xid=tx)
            
            #if we get here we have failed, but need to do some cleanup:
            session.dtx_end(xid=tx)
            session.dtx_rollback(xid=tx)
            self.fail("Session not selected for use with dtx, expected exception!")
        except SessionException, e:
            self.assertEquals(503, e.args[0].error_code)

    def test_start_already_known(self):
        """
        Verify that an attempt to start an association with a
        transaction that is already known is not allowed (unless the
        join flag is set).
        """
        #create two sessions on different connection & select them for use with dtx:
        session1 = self.session
        session1.dtx_select()

        other = self.connect()
        session2 = other.session("other", 0)
        session2.dtx_select()

        #create a xid
        tx = self.xid("dummy")
        #start work on one session under that xid:
        session1.dtx_start(xid=tx)
        #then start on the other without the join set
        failed = False
        try:
            session2.dtx_start(xid=tx)
        except SessionException, e:
            failed = True
            error = e

        #cleanup:
        if not failed:
            session2.dtx_end(xid=tx)
            other.close()
        session1.dtx_end(xid=tx)
        session1.dtx_rollback(xid=tx)
        
        #verification:
        if failed: self.assertEquals(530, error.args[0].error_code)
        else: self.fail("Xid already known, expected exception!")                    

    def test_forget_xid_on_completion(self):
        """
        Verify that a xid is 'forgotten' - and can therefore be used
        again - once it is completed.
        """
        #do some transactional work & complete the transaction
        self.test_simple_commit()
        # session has been reset, so reselect for use with dtx
        self.session.dtx_select()        
        
        #start association for the same xid as the previously completed txn
        tx = self.xid("my-xid")
        self.session.dtx_start(xid=tx)
        self.session.dtx_end(xid=tx)
        self.session.dtx_rollback(xid=tx)

    def test_start_join_and_resume(self):
        """
        Ensure the correct error is signalled when both the join and
        resume flags are set on starting an association between a
        session and a transcation.
        """
        session = self.session
        session.dtx_select()
        tx = self.xid("dummy")
        try:
            session.dtx_start(xid=tx, join=True, resume=True)
            #failed, but need some cleanup:
            session.dtx_end(xid=tx)
            session.dtx_rollback(xid=tx)
            self.fail("Join and resume both set, expected exception!")
        except SessionException, e:
            self.assertEquals(503, e.args[0].error_code)

    def test_start_join(self):
        """        
        Verify 'join' behaviour, where a session is associated with a
        transaction that is already associated with another session.        
        """
        guard = self.keepQueuesAlive(["one", "two"])
        #create two sessions & select them for use with dtx:
        session1 = self.session
        session1.dtx_select()

        session2 = self.conn.session("second", 2)
        session2.dtx_select()

        #setup
        session1.queue_declare(queue="one", auto_delete=True)
        session1.queue_declare(queue="two", auto_delete=True)
        session1.message_transfer(self.createMessage(session1, "one", "a", "DtxMessage"))
        session1.message_transfer(self.createMessage(session1, "two", "b", "DtxMessage"))

        #create a xid
        tx = self.xid("dummy")
        #start work on one session under that xid:
        session1.dtx_start(xid=tx)
        #then start on the other with the join flag set
        session2.dtx_start(xid=tx, join=True)

        #do work through each session
        self.swap(session1, "one", "two")#swap 'a' from 'one' to 'two'
        self.swap(session2, "two", "one")#swap 'b' from 'two' to 'one'

        #mark end on both sessions
        session1.dtx_end(xid=tx)
        session2.dtx_end(xid=tx)
        
        #commit and check
        session1.dtx_commit(xid=tx, one_phase=True)
        self.assertMessageCount(1, "one")
        self.assertMessageCount(1, "two")
        self.assertMessageId("a", "two")
        self.assertMessageId("b", "one")
        

    def test_suspend_resume(self):
        """
        Test suspension and resumption of an association
        """
        session = self.session
        session.dtx_select()

        #setup
        session.queue_declare(queue="one", exclusive=True, auto_delete=True)
        session.queue_declare(queue="two", exclusive=True, auto_delete=True)
        session.message_transfer(self.createMessage(session, "one", "a", "DtxMessage"))
        session.message_transfer(self.createMessage(session, "two", "b", "DtxMessage"))

        tx = self.xid("dummy")

        session.dtx_start(xid=tx)
        self.swap(session, "one", "two")#swap 'a' from 'one' to 'two'
        session.dtx_end(xid=tx, suspend=True)

        session.dtx_start(xid=tx, resume=True)
        self.swap(session, "two", "one")#swap 'b' from 'two' to 'one'
        session.dtx_end(xid=tx)
        
        #commit and check
        session.dtx_commit(xid=tx, one_phase=True)
        self.assertMessageCount(1, "one")
        self.assertMessageCount(1, "two")
        self.assertMessageId("a", "two")
        self.assertMessageId("b", "one")

    def test_suspend_start_end_resume(self):        
        """
        Test suspension and resumption of an association with work
        done on another transaction when the first transaction is
        suspended
        """
        session = self.session
        session.dtx_select()

        #setup
        session.queue_declare(queue="one", exclusive=True, auto_delete=True)
        session.queue_declare(queue="two", exclusive=True, auto_delete=True)
        session.message_transfer(self.createMessage(session, "one", "a", "DtxMessage"))
        session.message_transfer(self.createMessage(session, "two", "b", "DtxMessage"))

        tx = self.xid("dummy")

        session.dtx_start(xid=tx)
        self.swap(session, "one", "two")#swap 'a' from 'one' to 'two'
        session.dtx_end(xid=tx, suspend=True)

        session.dtx_start(xid=tx, resume=True)
        self.swap(session, "two", "one")#swap 'b' from 'two' to 'one'
        session.dtx_end(xid=tx)
        
        #commit and check
        session.dtx_commit(xid=tx, one_phase=True)
        self.assertMessageCount(1, "one")
        self.assertMessageCount(1, "two")
        self.assertMessageId("a", "two")
        self.assertMessageId("b", "one")

    def test_end_suspend_and_fail(self):
        """        
        Verify that the correct error is signalled if the suspend and
        fail flag are both set when disassociating a transaction from
        the session        
        """
        session = self.session
        session.dtx_select()
        tx = self.xid("suspend_and_fail")
        session.dtx_start(xid=tx)
        try:
            session.dtx_end(xid=tx, suspend=True, fail=True)
            self.fail("Suspend and fail both set, expected exception!")
        except SessionException, e:
            self.assertEquals(503, e.args[0].error_code)

        #cleanup    
        other = self.connect()
        session = other.session("cleanup", 1)
        session.dtx_rollback(xid=tx)
        session.close()
        other.close()
    

    def test_end_unknown_xid(self):
        """        
        Verifies that the correct exception is thrown when an attempt
        is made to end the association for a xid not previously
        associated with the session
        """
        session = self.session
        session.dtx_select()
        tx = self.xid("unknown-xid")
        try:
            session.dtx_end(xid=tx)
            self.fail("Attempted to end association with unknown xid, expected exception!")
        except SessionException, e:
            self.assertEquals(409, e.args[0].error_code)

    def test_end(self):
        """
        Verify that the association is terminated by end and subsequent
        operations are non-transactional        
        """
        guard = self.keepQueuesAlive(["tx-queue"])
        session = self.conn.session("alternate", 1)
        session.queue_declare(queue="tx-queue", exclusive=True, auto_delete=True)

        #publish a message under a transaction
        session.dtx_select()
        tx = self.xid("dummy")
        session.dtx_start(xid=tx)
        session.message_transfer(self.createMessage(session, "tx-queue", "one", "DtxMessage"))
        session.dtx_end(xid=tx)

        #now that association with txn is ended, publish another message
        session.message_transfer(self.createMessage(session, "tx-queue", "two", "DtxMessage"))

        #check the second message is available, but not the first
        self.assertMessageCount(1, "tx-queue")
        self.subscribe(session, queue="tx-queue", destination="results")
        msg = session.incoming("results").get(timeout=1)
        self.assertEqual("two", self.getMessageProperty(msg, 'correlation_id'))
        session.message_cancel(destination="results")
        #ack the message then close the session
        session.message_accept(RangedSet(msg.id))
        session.close()

        session = self.session        
        #commit the transaction and check that the first message (and
        #only the first message) is then delivered
        session.dtx_commit(xid=tx, one_phase=True)
        self.assertMessageCount(1, "tx-queue")
        self.assertMessageId("one", "tx-queue")

    def test_invalid_commit_one_phase_true(self):
        """
        Test that a commit with one_phase = True is rejected if the
        transaction in question has already been prepared.        
        """
        other = self.connect()
        tester = other.session("tester", 1)
        tester.queue_declare(queue="dummy", exclusive=True, auto_delete=True)
        tester.dtx_select()
        tx = self.xid("dummy")
        tester.dtx_start(xid=tx)
        tester.message_transfer(self.createMessage(tester, "dummy", "dummy", "whatever"))
        tester.dtx_end(xid=tx)
        tester.dtx_prepare(xid=tx)
        failed = False
        try:
            tester.dtx_commit(xid=tx, one_phase=True)
        except SessionException, e:
            failed = True
            error = e

        if failed:
            self.session.dtx_rollback(xid=tx)
            self.assertEquals(409, error.args[0].error_code)
        else:
            tester.close()
            other.close()
            self.fail("Invalid use of one_phase=True, expected exception!")

    def test_invalid_commit_one_phase_false(self):
        """
        Test that a commit with one_phase = False is rejected if the
        transaction in question has not yet been prepared.        
        """
        other = self.connect()
        tester = other.session("tester", 1)
        tester.queue_declare(queue="dummy", exclusive=True, auto_delete=True)
        tester.dtx_select()
        tx = self.xid("dummy")
        tester.dtx_start(xid=tx)
        tester.message_transfer(self.createMessage(tester, "dummy", "dummy", "whatever"))
        tester.dtx_end(xid=tx)
        failed = False
        try:
            tester.dtx_commit(xid=tx, one_phase=False)
        except SessionException, e:
            failed = True
            error = e

        if failed:
            self.session.dtx_rollback(xid=tx)
            self.assertEquals(409, error.args[0].error_code)
        else:
            tester.close()
            other.close()
            self.fail("Invalid use of one_phase=False, expected exception!")

    def test_invalid_commit_not_ended(self):
        """
        Test that a commit fails if the xid is still associated with a session.        
        """
        other = self.connect()
        tester = other.session("tester", 1)
        self.session.queue_declare(queue="dummy", exclusive=True, auto_delete=True)
        self.session.dtx_select()
        tx = self.xid("dummy")
        self.session.dtx_start(xid=tx)
        self.session.message_transfer(self.createMessage(tester, "dummy", "dummy", "whatever"))

        failed = False
        try:
            tester.dtx_commit(xid=tx, one_phase=False)
        except SessionException, e:
            failed = True
            error = e

        if failed:
            self.session.dtx_end(xid=tx)
            self.session.dtx_rollback(xid=tx)
            self.assertEquals(409, error.args[0].error_code)
        else:
            tester.close()
            other.close()
            self.fail("Commit should fail as xid is still associated!")

    def test_invalid_rollback_not_ended(self):
        """
        Test that a rollback fails if the xid is still associated with a session.        
        """
        other = self.connect()
        tester = other.session("tester", 1)
        self.session.queue_declare(queue="dummy", exclusive=True, auto_delete=True)
        self.session.dtx_select()
        tx = self.xid("dummy")
        self.session.dtx_start(xid=tx)
        self.session.message_transfer(self.createMessage(tester, "dummy", "dummy", "whatever"))

        failed = False
        try:
            tester.dtx_rollback(xid=tx)
        except SessionException, e:
            failed = True
            error = e

        if failed:
            self.session.dtx_end(xid=tx)
            self.session.dtx_rollback(xid=tx)
            self.assertEquals(409, error.args[0].error_code)
        else:
            tester.close()
            other.close()
            self.fail("Rollback should fail as xid is still associated!")


    def test_invalid_prepare_not_ended(self):
        """
        Test that a prepare fails if the xid is still associated with a session.        
        """
        other = self.connect()
        tester = other.session("tester", 1)
        self.session.queue_declare(queue="dummy", exclusive=True, auto_delete=True)
        self.session.dtx_select()
        tx = self.xid("dummy")
        self.session.dtx_start(xid=tx)
        self.session.message_transfer(self.createMessage(tester, "dummy", "dummy", "whatever"))

        failed = False
        try:
            tester.dtx_prepare(xid=tx)
        except SessionException, e:
            failed = True
            error = e

        if failed:
            self.session.dtx_end(xid=tx)
            self.session.dtx_rollback(xid=tx)
            self.assertEquals(409, error.args[0].error_code)
        else:
            tester.close()
            other.close()
            self.fail("Rollback should fail as xid is still associated!")

    def test_implicit_end(self):
        """
        Test that an association is implicitly ended when the session
        is closed (whether by exception or explicit client request)
        and the transaction in question is marked as rollback only.
        """
        session1 = self.session
        session2 = self.conn.session("other", 2)

        #setup:
        session2.queue_declare(queue="dummy", exclusive=True, auto_delete=True)
        session2.message_transfer(self.createMessage(session2, "dummy", "a", "whatever"))
        tx = self.xid("dummy")

        session2.dtx_select()
        session2.dtx_start(xid=tx)
        session2.message_subscribe(queue="dummy", destination="dummy")
        session2.message_flow(destination="dummy", unit=session2.credit_unit.message, value=1)
        session2.message_flow(destination="dummy", unit=session2.credit_unit.byte, value=0xFFFFFFFFL)
        msg = session2.incoming("dummy").get(timeout=1)
        session2.message_accept(RangedSet(msg.id))
        session2.message_cancel(destination="dummy")
        session2.message_transfer(self.createMessage(session2, "dummy", "b", "whatever"))
        session2.close()

        self.assertEqual(self.XA_RBROLLBACK, session1.dtx_prepare(xid=tx).status)
        session1.dtx_rollback(xid=tx)

    def test_get_timeout(self):
        """        
        Check that get-timeout returns the correct value, (and that a
        transaction with a timeout can complete normally)        
        """
        session = self.session
        tx = self.xid("dummy")

        session.dtx_select()
        session.dtx_start(xid=tx)
        self.assertEqual(0, session.dtx_get_timeout(xid=tx).timeout)
        session.dtx_set_timeout(xid=tx, timeout=60)
        self.assertEqual(60, session.dtx_get_timeout(xid=tx).timeout)
        self.assertEqual(self.XA_OK, session.dtx_end(xid=tx).status)
        self.assertEqual(self.XA_OK, session.dtx_rollback(xid=tx).status)        
        
    def test_set_timeout(self):
        """        
        Test the timeout of a transaction results in the expected
        behaviour        
        """

        guard = self.keepQueuesAlive(["queue-a", "queue-b"])
        #open new session to allow self.session to be used in checking the queue
        session = self.conn.session("worker", 1)
        #setup:
        tx = self.xid("dummy")
        session.queue_declare(queue="queue-a", auto_delete=True)
        session.queue_declare(queue="queue-b", auto_delete=True)
        session.message_transfer(self.createMessage(session, "queue-a", "timeout", "DtxMessage"))

        session.dtx_select()
        session.dtx_start(xid=tx)
        self.swap(session, "queue-a", "queue-b")
        session.dtx_set_timeout(xid=tx, timeout=2)
        sleep(3)
        #check that the work has been rolled back already
        self.assertMessageCount(1, "queue-a")
        self.assertMessageCount(0, "queue-b")
        self.assertMessageId("timeout", "queue-a")
        #check the correct codes are returned when we try to complete the txn
        self.assertEqual(self.XA_RBTIMEOUT, session.dtx_end(xid=tx).status)
        self.assertEqual(self.XA_RBTIMEOUT, session.dtx_rollback(xid=tx).status)        



    def test_recover(self):
        """
        Test basic recover behaviour
        """
        session = self.session

        session.dtx_select()
        session.queue_declare(queue="dummy", exclusive=True, auto_delete=True)

        prepared = []
        for i in range(1, 10):
            tx = self.xid("tx%s" % (i))
            session.dtx_start(xid=tx)
            session.message_transfer(self.createMessage(session, "dummy", "message%s" % (i), "message%s" % (i)))
            session.dtx_end(xid=tx)
            if i in [2, 5, 6, 8]:
                session.dtx_prepare(xid=tx)
                prepared.append(tx)
            else:    
                session.dtx_rollback(xid=tx)

        xids = session.dtx_recover().in_doubt
        
        #rollback the prepared transactions returned by recover
        for x in xids:
            session.dtx_rollback(xid=x)            

        #validate against the expected list of prepared transactions
        actual = set([x.global_id for x in xids]) #TODO: come up with nicer way to test these
        expected = set([x.global_id for x in prepared])
        intersection = actual.intersection(expected)
        
        if intersection != expected:
            missing = expected.difference(actual)
            extra = actual.difference(expected)
            self.fail("Recovered xids not as expected. missing: %s; extra: %s" % (missing, extra))

    def test_bad_resume(self):
        """
        Test that a resume on a session not selected for use with dtx fails
        """
        session = self.session
        try:
            session.dtx_start(resume=True)
        except SessionException, e:
            self.assertEquals(503, e.args[0].error_code)

    def test_prepare_unknown(self):
        session = self.session
        try:
            session.dtx_prepare(xid=self.xid("unknown"))
        except SessionException, e:
            self.assertEquals(404, e.args[0].error_code)

    def test_commit_unknown(self):
        session = self.session
        try:
            session.dtx_commit(xid=self.xid("unknown"))
        except SessionException, e:
            self.assertEquals(404, e.args[0].error_code)

    def test_rollback_unknown(self):
        session = self.session
        try:
            session.dtx_rollback(xid=self.xid("unknown"))
        except SessionException, e:
            self.assertEquals(404, e.args[0].error_code)

    def test_get_timeout_unknown(self):
        session = self.session
        try:
            session.dtx_get_timeout(xid=self.xid("unknown"))
        except SessionException, e:
            self.assertEquals(404, e.args[0].error_code)

    def xid(self, txid):
        DtxTests.tx_counter += 1
        branchqual = "v%s" % DtxTests.tx_counter
        return self.session.xid(format=0, global_id=txid, branch_id=branchqual)

    def txswap(self, tx, id):
        session = self.session
        #declare two queues:
        session.queue_declare(queue="queue-a", auto_delete=True)
        session.queue_declare(queue="queue-b", auto_delete=True)

        #put message with specified id on one queue:
        dp=session.delivery_properties(routing_key="queue-a")
        mp=session.message_properties(correlation_id=id)
        session.message_transfer(message=Message(dp, mp, "DtxMessage"))

        #start the transaction:
        session.dtx_select()        
        self.assertEqual(self.XA_OK, self.session.dtx_start(xid=tx).status)

        #'swap' the message from one queue to the other, under that transaction:
        self.swap(self.session, "queue-a", "queue-b")

        #mark the end of the transactional work:
        self.assertEqual(self.XA_OK, self.session.dtx_end(xid=tx).status)

    def swap(self, session, src, dest):
        #consume from src:
        session.message_subscribe(destination="temp-swap", queue=src)
        session.message_flow(destination="temp-swap", unit=session.credit_unit.message, value=1)
        session.message_flow(destination="temp-swap", unit=session.credit_unit.byte, value=0xFFFFFFFFL)
        msg = session.incoming("temp-swap").get(timeout=1)
        session.message_cancel(destination="temp-swap")
        session.message_accept(RangedSet(msg.id))
        #todo: also complete at this point?

        #re-publish to dest:
        dp=session.delivery_properties(routing_key=dest)
        mp=session.message_properties(correlation_id=self.getMessageProperty(msg, 'correlation_id'))
        session.message_transfer(message=Message(dp, mp, msg.body))

    def assertMessageCount(self, expected, queue):
        self.assertEqual(expected, self.session.queue_query(queue=queue).message_count)

    def assertMessageId(self, expected, queue):
        self.session.message_subscribe(queue=queue, destination="results")
        self.session.message_flow(destination="results", unit=self.session.credit_unit.message, value=1)
        self.session.message_flow(destination="results", unit=self.session.credit_unit.byte, value=0xFFFFFFFFL)
        self.assertEqual(expected, self.getMessageProperty(self.session.incoming("results").get(timeout=1), 'correlation_id'))
        self.session.message_cancel(destination="results")

    def getMessageProperty(self, msg, prop):
        for h in msg.headers:
            if hasattr(h, prop): return getattr(h, prop)
        return None            

    def keepQueuesAlive(self, names):
        session = self.conn.session("nasty", 99)
        for n in names:
            session.queue_declare(queue=n, auto_delete=True)
            session.message_subscribe(destination=n, queue=n)
        return session
        
    def createMessage(self, session, key, id, body):
        dp=session.delivery_properties(routing_key=key)
        mp=session.message_properties(correlation_id=id)
        session.message_transfer(message=Message(dp, mp, body))
