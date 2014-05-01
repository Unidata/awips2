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
import traceback
from qpid.queue import Empty
from qpid.datatypes import Message
from qpid.testlib import TestBase010
from qpid.session import SessionException

class AlternateExchangeTests(TestBase010):
    """
    Tests for the new mechanism for message returns introduced in 0-10
    and available in 0-9 for preview
    """

    def test_unroutable(self):
        """
        Test that unroutable messages are delivered to the alternate-exchange if specified
        """
        session = self.session
        #create an exchange with an alternate defined
        session.exchange_declare(exchange="secondary", type="fanout")
        session.exchange_declare(exchange="primary", type="direct", alternate_exchange="secondary")

        #declare, bind (to the alternate exchange) and consume from a queue for 'returned' messages
        session.queue_declare(queue="returns", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="returns", exchange="secondary")
        session.message_subscribe(destination="a", queue="returns")
        session.message_flow(destination="a", unit=session.credit_unit.message, value=0xFFFFFFFFL)
        session.message_flow(destination="a", unit=session.credit_unit.byte, value=0xFFFFFFFFL)
        returned = session.incoming("a")

        #declare, bind (to the primary exchange) and consume from a queue for 'processed' messages
        session.queue_declare(queue="processed", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="processed", exchange="primary", binding_key="my-key")
        session.message_subscribe(destination="b", queue="processed")
        session.message_flow(destination="b", unit=session.credit_unit.message, value=0xFFFFFFFFL)
        session.message_flow(destination="b", unit=session.credit_unit.byte, value=0xFFFFFFFFL)
        processed = session.incoming("b")

        #publish to the primary exchange
        #...one message that makes it to the 'processed' queue:
        dp=self.session.delivery_properties(routing_key="my-key")
        session.message_transfer(destination="primary", message=Message(dp, "Good"))
        #...and one that does not:
        dp=self.session.delivery_properties(routing_key="unused-key")
        session.message_transfer(destination="primary", message=Message(dp, "Bad"))

        #delete the exchanges
        session.exchange_delete(exchange="primary")
        session.exchange_delete(exchange="secondary")

        #verify behaviour
        self.assertEqual("Good", processed.get(timeout=1).body)
        self.assertEqual("Bad", returned.get(timeout=1).body)
        self.assertEmpty(processed)
        self.assertEmpty(returned)

    def test_queue_delete(self):
        """
        Test that messages in a queue being deleted are delivered to the alternate-exchange if specified
        """
        session = self.session
        #set up a 'dead letter queue':
        session.exchange_declare(exchange="dlq", type="fanout")
        session.queue_declare(queue="deleted", exclusive=True, auto_delete=True)
        session.exchange_bind(exchange="dlq", queue="deleted")
        session.message_subscribe(destination="dlq", queue="deleted")
        session.message_flow(destination="dlq", unit=session.credit_unit.message, value=0xFFFFFFFFL)
        session.message_flow(destination="dlq", unit=session.credit_unit.byte, value=0xFFFFFFFFL)
        dlq = session.incoming("dlq")

        #create a queue using the dlq as its alternate exchange:
        session.queue_declare(queue="delete-me", alternate_exchange="dlq")
        #send it some messages:
        dp=self.session.delivery_properties(routing_key="delete-me")
        session.message_transfer(message=Message(dp, "One"))
        session.message_transfer(message=Message(dp, "Two"))
        session.message_transfer(message=Message(dp, "Three"))
        #delete it:
        session.queue_delete(queue="delete-me")
        #delete the dlq exchange:
        session.exchange_delete(exchange="dlq")

        #check the messages were delivered to the dlq:
        self.assertEqual("One", dlq.get(timeout=1).body)
        self.assertEqual("Two", dlq.get(timeout=1).body)
        self.assertEqual("Three", dlq.get(timeout=1).body)
        self.assertEmpty(dlq)

    def test_delete_while_used_by_queue(self):
        """
        Ensure an exchange still in use as an alternate-exchange for a
        queue can't be deleted
        """
        session = self.session
        session.exchange_declare(exchange="alternate", type="fanout")

        session2 = self.conn.session("alternate", 2)
        session2.queue_declare(queue="q", alternate_exchange="alternate")
        try:
            session2.exchange_delete(exchange="alternate")
            self.fail("Expected deletion of in-use alternate-exchange to fail")
        except SessionException, e:
            session = self.session
            session.queue_delete(queue="q")
            session.exchange_delete(exchange="alternate")
            self.assertEquals(530, e.args[0].error_code)            


    def test_delete_while_used_by_exchange(self):
        """
        Ensure an exchange still in use as an alternate-exchange for 
        another exchange can't be deleted
        """
        session = self.session
        session.exchange_declare(exchange="alternate", type="fanout")

        session = self.conn.session("alternate", 2)
        session.exchange_declare(exchange="e", type="fanout", alternate_exchange="alternate")
        try:
            session.exchange_delete(exchange="alternate")
            self.fail("Expected deletion of in-use alternate-exchange to fail")
        except SessionException, e:
            session = self.session
            session.exchange_delete(exchange="e")
            session.exchange_delete(exchange="alternate")
            self.assertEquals(530, e.args[0].error_code)


    def test_modify_existing_exchange_alternate(self):
        """
        Ensure that attempting to modify an exhange to change
        the alternate throws an exception
        """
        session = self.session
        session.exchange_declare(exchange="alt1", type="direct")
        session.exchange_declare(exchange="alt2", type="direct")
        session.exchange_declare(exchange="onealternate", type="fanout", alternate_exchange="alt1")
        try:
            # attempt to change the alternate on an already existing exchange
            session.exchange_declare(exchange="onealternate", type="fanout", alternate_exchange="alt2")
            self.fail("Expected changing an alternate on an existing exchange to fail")
        except SessionException, e:
            self.assertEquals(530, e.args[0].error_code)
        session = self.conn.session("alternate", 2)
        session.exchange_delete(exchange="onealternate")
        session.exchange_delete(exchange="alt2")
        session.exchange_delete(exchange="alt1")


    def test_add_alternate_to_exchange(self):
        """
        Ensure that attempting to modify an exhange by adding
        an alternate throws an exception
        """
        session = self.session
        session.exchange_declare(exchange="alt1", type="direct")
        session.exchange_declare(exchange="noalternate", type="fanout")
        try:
            # attempt to add an alternate on an already existing exchange
            session.exchange_declare(exchange="noalternate", type="fanout", alternate_exchange="alt1")
            self.fail("Expected adding an alternate on an existing exchange to fail")
        except SessionException, e:
            self.assertEquals(530, e.args[0].error_code)
        session = self.conn.session("alternate", 2)
        session.exchange_delete(exchange="noalternate")
        session.exchange_delete(exchange="alt1")


    def test_del_alternate_to_exchange(self):
        """
        Ensure that attempting to modify an exhange by declaring
        it again without an alternate does nothing
        """
        session = self.session
        session.exchange_declare(exchange="alt1", type="direct")
        session.exchange_declare(exchange="onealternate", type="fanout", alternate_exchange="alt1")
        # attempt to re-declare without an alternate - silently ignore
        session.exchange_declare(exchange="onealternate", type="fanout" )
        session.exchange_delete(exchange="onealternate")
        session.exchange_delete(exchange="alt1")


    def assertEmpty(self, queue):
        try:
            msg = queue.get(timeout=1) 
            self.fail("Queue not empty: " + msg)
        except Empty: None
