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
from qpid.client import Closed
from qpid.queue import Empty
from qpid.testlib import TestBase010
from qpid.datatypes import Message, RangedSet

class BrokerTests(TestBase010):
    """Tests for basic Broker functionality"""

    def test_ack_and_no_ack(self):
        """
        First, this test tries to receive a message with a no-ack
        consumer. Second, this test tries to explicitly receive and
        acknowledge a message with an acknowledging consumer.
        """
        session = self.session
        session.queue_declare(queue = "myqueue", exclusive=True, auto_delete=True)

        # No ack consumer
        ctag = "tag1"
        session.message_subscribe(queue = "myqueue", destination = ctag)
        session.message_flow(destination=ctag, unit=session.credit_unit.message, value=0xFFFFFFFFL)
        session.message_flow(destination=ctag, unit=session.credit_unit.byte, value=0xFFFFFFFFL)
        body = "test no-ack"
        session.message_transfer(message=Message(session.delivery_properties(routing_key="myqueue"), body))
        msg = session.incoming(ctag).get(timeout = 5)
        self.assert_(msg.body == body)

        # Acknowledging consumer
        session.queue_declare(queue = "otherqueue", exclusive=True, auto_delete=True)
        ctag = "tag2"
        session.message_subscribe(queue = "otherqueue", destination = ctag, accept_mode = 1)
        session.message_flow(destination=ctag, unit=session.credit_unit.message, value=0xFFFFFFFFL)
        session.message_flow(destination=ctag, unit=session.credit_unit.byte, value=0xFFFFFFFFL)
        body = "test ack"
        session.message_transfer(message=Message(session.delivery_properties(routing_key="otherqueue"), body))
        msg = session.incoming(ctag).get(timeout = 5)
        session.message_accept(RangedSet(msg.id))
        self.assert_(msg.body == body)
        
    def test_simple_delivery_immediate(self):
        """
        Test simple message delivery where consume is issued before publish
        """
        session = self.session
        session.queue_declare(queue="test-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="test-queue", exchange="amq.fanout")
        consumer_tag = "tag1"
        session.message_subscribe(queue="test-queue", destination=consumer_tag)
        session.message_flow(unit = session.credit_unit.message, value = 0xFFFFFFFFL, destination = consumer_tag)
        session.message_flow(unit = session.credit_unit.byte, value = 0xFFFFFFFFL, destination = consumer_tag)
        queue = session.incoming(consumer_tag)

        body = "Immediate Delivery"
        session.message_transfer("amq.fanout", None, None, Message(body))
        msg = queue.get(timeout=5)
        self.assert_(msg.body == body)

    def test_simple_delivery_queued(self):
        """
        Test basic message delivery where publish is issued before consume
        (i.e. requires queueing of the message)
        """
        session = self.session
        session.queue_declare(queue="test-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="test-queue", exchange="amq.fanout")
        body = "Queued Delivery"
        session.message_transfer("amq.fanout", None, None, Message(body))

        consumer_tag = "tag1"
        session.message_subscribe(queue="test-queue", destination=consumer_tag)
        session.message_flow(unit = session.credit_unit.message, value = 0xFFFFFFFFL, destination = consumer_tag)
        session.message_flow(unit = session.credit_unit.byte, value = 0xFFFFFFFFL, destination = consumer_tag)
        queue = session.incoming(consumer_tag)
        msg = queue.get(timeout=5)
        self.assert_(msg.body == body)
