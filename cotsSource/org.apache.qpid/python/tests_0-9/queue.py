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

    def test_unbind_direct(self):
        self.unbind_test(exchange="amq.direct", routing_key="key")

    def test_unbind_topic(self):
        self.unbind_test(exchange="amq.topic", routing_key="key")

    def test_unbind_fanout(self):
        self.unbind_test(exchange="amq.fanout")

    def test_unbind_headers(self):
        self.unbind_test(exchange="amq.match", args={ "x-match":"all", "a":"b"}, headers={"a":"b"})

    def unbind_test(self, exchange, routing_key="", args=None, headers={}):
        #bind two queues and consume from them
        channel = self.channel

        channel.queue_declare(queue="queue-1", exclusive="True")
        channel.queue_declare(queue="queue-2", exclusive="True")

        channel.basic_consume(queue="queue-1", consumer_tag="queue-1", no_ack=True)
        channel.basic_consume(queue="queue-2", consumer_tag="queue-2", no_ack=True)

        queue1 = self.client.queue("queue-1")
        queue2 = self.client.queue("queue-2")

        channel.queue_bind(exchange=exchange, queue="queue-1", routing_key=routing_key, arguments=args)
        channel.queue_bind(exchange=exchange, queue="queue-2", routing_key=routing_key, arguments=args)

        #send a message that will match both bindings
        channel.basic_publish(exchange=exchange, routing_key=routing_key,
                              content=Content("one", properties={"headers": headers}))

        #unbind first queue
        channel.queue_unbind(exchange=exchange, queue="queue-1", routing_key=routing_key, arguments=args)

        #send another message
        channel.basic_publish(exchange=exchange, routing_key=routing_key,
                              content=Content("two", properties={"headers": headers}))

        #check one queue has both messages and the other has only one
        self.assertEquals("one", queue1.get(timeout=1).content.body)
        try:
            msg = queue1.get(timeout=1)
            self.fail("Got extra message: %s" % msg.body)
        except Empty: pass

        self.assertEquals("one", queue2.get(timeout=1).content.body)
        self.assertEquals("two", queue2.get(timeout=1).content.body)
        try:
            msg = queue2.get(timeout=1)
            self.fail("Got extra message: " + msg)
        except Empty: pass

    def test_autodelete_shared(self):
        """
        Test auto-deletion (of non-exclusive queues)
        """
        channel = self.channel
        other = self.connect()
        channel2 = other.channel(1)
        channel2.channel_open()

        channel.queue_declare(queue="auto-delete-me", auto_delete=True)

        #consume from both channels
        reply = channel.basic_consume(queue="auto-delete-me", no_ack=True)
        channel2.basic_consume(queue="auto-delete-me", no_ack=True)

        #implicit cancel
        channel2.channel_close()

        #check it is still there
        channel.queue_declare(queue="auto-delete-me", passive=True)

        #explicit cancel => queue is now unused again:
        channel.basic_cancel(consumer_tag=reply.consumer_tag)

        #NOTE: this assumes there is no timeout in use

        #check that it has gone be declaring passively
        try:
            channel.queue_declare(queue="auto-delete-me", passive=True)
            self.fail("Expected queue to have been deleted")
        except Closed, e:
            self.assertChannelException(404, e.args[0])
