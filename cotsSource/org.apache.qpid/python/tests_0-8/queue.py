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
from qpid.content import Content
from qpid.testlib import TestBase

class QueueTests(TestBase):
    """Tests for 'methods' on the amqp queue 'class'"""

    def test_purge(self):
        """
        Test that the purge method removes messages from the queue
        """
        channel = self.channel
        #setup, declare a queue and add some messages to it:
        channel.exchange_declare(exchange="test-exchange", type="direct")
        channel.queue_declare(queue="test-queue", exclusive=True)
        channel.queue_bind(queue="test-queue", exchange="test-exchange", routing_key="key")
        channel.basic_publish(exchange="test-exchange", routing_key="key", content=Content("one"))
        channel.basic_publish(exchange="test-exchange", routing_key="key", content=Content("two"))
        channel.basic_publish(exchange="test-exchange", routing_key="key", content=Content("three"))

        #check that the queue now reports 3 messages:
        reply = channel.queue_declare(queue="test-queue")
        self.assertEqual(3, reply.message_count)

        #now do the purge, then test that three messages are purged and the count drops to 0
        reply = channel.queue_purge(queue="test-queue");
        self.assertEqual(3, reply.message_count)        
        reply = channel.queue_declare(queue="test-queue")
        self.assertEqual(0, reply.message_count)

        #send a further message and consume it, ensuring that the other messages are really gone
        channel.basic_publish(exchange="test-exchange", routing_key="key", content=Content("four"))
        reply = channel.basic_consume(queue="test-queue", no_ack=True)
        queue = self.client.queue(reply.consumer_tag)
        msg = queue.get(timeout=1)
        self.assertEqual("four", msg.content.body)

        #check error conditions (use new channels): 
        channel = self.client.channel(2)
        channel.channel_open()
        try:
            #queue specified but doesn't exist:
            channel.queue_purge(queue="invalid-queue")
            self.fail("Expected failure when purging non-existent queue")
        except Closed, e:
            self.assertChannelException(404, e.args[0])

        channel = self.client.channel(3)
        channel.channel_open()
        try:
            #queue not specified and none previously declared for channel:
            channel.queue_purge()
            self.fail("Expected failure when purging unspecified queue")
        except Closed, e:
            self.assertConnectionException(530, e.args[0])

        #cleanup    
        other = self.connect()
        channel = other.channel(1)
        channel.channel_open()
        channel.exchange_delete(exchange="test-exchange")

    def test_declare_exclusive(self):
        """
        Test that the exclusive field is honoured in queue.declare
        """
        # TestBase.setUp has already opened channel(1)
        c1 = self.channel
        # Here we open a second separate connection:
        other = self.connect()
        c2 = other.channel(1)
        c2.channel_open()

        #declare an exclusive queue:
        c1.queue_declare(queue="exclusive-queue", exclusive="True")
        try:
            #other connection should not be allowed to declare this:
            c2.queue_declare(queue="exclusive-queue", exclusive="True")
            self.fail("Expected second exclusive queue_declare to raise a channel exception")
        except Closed, e:
            self.assertChannelException(405, e.args[0])


    def test_declare_passive(self):
        """
        Test that the passive field is honoured in queue.declare
        """
        channel = self.channel
        #declare an exclusive queue:
        channel.queue_declare(queue="passive-queue-1", exclusive="True")
        channel.queue_declare(queue="passive-queue-1", passive="True")
        try:
            #other connection should not be allowed to declare this:
            channel.queue_declare(queue="passive-queue-2", passive="True")
            self.fail("Expected passive declaration of non-existant queue to raise a channel exception")
        except Closed, e:
            self.assertChannelException(404, e.args[0])


    def test_bind(self):
        """
        Test various permutations of the queue.bind method
        """
        channel = self.channel
        channel.queue_declare(queue="queue-1", exclusive="True")

        #straightforward case, both exchange & queue exist so no errors expected:
        channel.queue_bind(queue="queue-1", exchange="amq.direct", routing_key="key1")

        #bind the default queue for the channel (i.e. last one declared):
        channel.queue_bind(exchange="amq.direct", routing_key="key2")

        #use the queue name where neither routing key nor queue are specified:
        channel.queue_bind(exchange="amq.direct")

        #try and bind to non-existant exchange
        try:
            channel.queue_bind(queue="queue-1", exchange="an-invalid-exchange", routing_key="key1")
            self.fail("Expected bind to non-existant exchange to fail")
        except Closed, e:
            self.assertChannelException(404, e.args[0])

        #need to reopen a channel:    
        channel = self.client.channel(2)
        channel.channel_open()

        #try and bind non-existant queue:
        try:
            channel.queue_bind(queue="queue-2", exchange="amq.direct", routing_key="key1")
            self.fail("Expected bind of non-existant queue to fail")
        except Closed, e:
            self.assertChannelException(404, e.args[0])


    def test_delete_simple(self):
        """
        Test basic queue deletion
        """
        channel = self.channel

        #straight-forward case:
        channel.queue_declare(queue="delete-me")
        channel.basic_publish(routing_key="delete-me", content=Content("a"))
        channel.basic_publish(routing_key="delete-me", content=Content("b"))
        channel.basic_publish(routing_key="delete-me", content=Content("c"))        
        reply = channel.queue_delete(queue="delete-me")
        self.assertEqual(3, reply.message_count)
        #check that it has gone be declaring passively
        try:
            channel.queue_declare(queue="delete-me", passive="True")
            self.fail("Queue has not been deleted")
        except Closed, e:
            self.assertChannelException(404, e.args[0])

        #check attempted deletion of non-existant queue is handled correctly:    
        channel = self.client.channel(2)
        channel.channel_open()
        try:
            channel.queue_delete(queue="i-dont-exist", if_empty="True")
            self.fail("Expected delete of non-existant queue to fail")
        except Closed, e:
            self.assertChannelException(404, e.args[0])

        

    def test_delete_ifempty(self):
        """
        Test that if_empty field of queue_delete is honoured
        """
        channel = self.channel

        #create a queue and add a message to it (use default binding):
        channel.queue_declare(queue="delete-me-2")
        channel.queue_declare(queue="delete-me-2", passive="True")
        channel.basic_publish(routing_key="delete-me-2", content=Content("message"))

        #try to delete, but only if empty:
        try:
            channel.queue_delete(queue="delete-me-2", if_empty="True")
            self.fail("Expected delete if_empty to fail for non-empty queue")
        except Closed, e:
            self.assertChannelException(406, e.args[0])

        #need new channel now:    
        channel = self.client.channel(2)
        channel.channel_open()

        #empty queue:
        reply = channel.basic_consume(queue="delete-me-2", no_ack=True)
        queue = self.client.queue(reply.consumer_tag)
        msg = queue.get(timeout=1)
        self.assertEqual("message", msg.content.body)
        channel.basic_cancel(consumer_tag=reply.consumer_tag)

        #retry deletion on empty queue:
        channel.queue_delete(queue="delete-me-2", if_empty="True")

        #check that it has gone by declaring passively:
        try:
            channel.queue_declare(queue="delete-me-2", passive="True")
            self.fail("Queue has not been deleted")
        except Closed, e:
            self.assertChannelException(404, e.args[0])
        
    def test_delete_ifunused(self):
        """
        Test that if_unused field of queue_delete is honoured
        """
        channel = self.channel

        #create a queue and register a consumer:
        channel.queue_declare(queue="delete-me-3")
        channel.queue_declare(queue="delete-me-3", passive="True")
        reply = channel.basic_consume(queue="delete-me-3", no_ack=True)

        #need new channel now:    
        channel2 = self.client.channel(2)
        channel2.channel_open()
        #try to delete, but only if empty:
        try:
            channel2.queue_delete(queue="delete-me-3", if_unused="True")
            self.fail("Expected delete if_unused to fail for queue with existing consumer")
        except Closed, e:
            self.assertChannelException(406, e.args[0])


        channel.basic_cancel(consumer_tag=reply.consumer_tag)    
        channel.queue_delete(queue="delete-me-3", if_unused="True")
        #check that it has gone by declaring passively:
        try:
            channel.queue_declare(queue="delete-me-3", passive="True")
            self.fail("Queue has not been deleted")
        except Closed, e:
            self.assertChannelException(404, e.args[0])


