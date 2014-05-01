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

class TxTests(TestBase):
    """
    Tests for 'methods' on the amqp tx 'class'
    """

    def test_commit(self):
        """
        Test that commited publishes are delivered and commited acks are not re-delivered
        """
        channel = self.channel
        queue_a, queue_b, queue_c = self.perform_txn_work(channel, "tx-commit-a", "tx-commit-b", "tx-commit-c")
        channel.tx_commit()

        #check results
        for i in range(1, 5):
            msg = queue_c.get(timeout=1)
            self.assertEqual("TxMessage %d" % i, msg.content.body)

        msg = queue_b.get(timeout=1)
        self.assertEqual("TxMessage 6", msg.content.body)

        msg = queue_a.get(timeout=1)
        self.assertEqual("TxMessage 7", msg.content.body)

        for q in [queue_a, queue_b, queue_c]:
            try:
                extra = q.get(timeout=1)
                self.fail("Got unexpected message: " + extra.content.body)
            except Empty: None

        #cleanup
        channel.basic_ack(delivery_tag=0, multiple=True)
        channel.tx_commit()

    def test_auto_rollback(self):
        """
        Test that a channel closed with an open transaction is effectively rolled back
        """
        channel = self.channel
        queue_a, queue_b, queue_c = self.perform_txn_work(channel, "tx-autorollback-a", "tx-autorollback-b", "tx-autorollback-c")

        for q in [queue_a, queue_b, queue_c]:
            try:
                extra = q.get(timeout=1)
                self.fail("Got unexpected message: " + extra.content.body)
            except Empty: None

        channel.tx_rollback()

        #check results
        for i in range(1, 5):
            msg = queue_a.get(timeout=1)
            self.assertEqual("Message %d" % i, msg.content.body)

        msg = queue_b.get(timeout=1)
        self.assertEqual("Message 6", msg.content.body)

        msg = queue_c.get(timeout=1)
        self.assertEqual("Message 7", msg.content.body)

        for q in [queue_a, queue_b, queue_c]:
            try:
                extra = q.get(timeout=1)
                self.fail("Got unexpected message: " + extra.content.body)
            except Empty: None

        #cleanup
        channel.basic_ack(delivery_tag=0, multiple=True)
        channel.tx_commit()

    def test_rollback(self):
        """
        Test that rolled back publishes are not delivered and rolled back acks are re-delivered
        """
        channel = self.channel
        queue_a, queue_b, queue_c = self.perform_txn_work(channel, "tx-rollback-a", "tx-rollback-b", "tx-rollback-c")

        for q in [queue_a, queue_b, queue_c]:
            try:
                extra = q.get(timeout=1)
                self.fail("Got unexpected message: " + extra.content.body)
            except Empty: None

        channel.tx_rollback()

        #check results
        for i in range(1, 5):
            msg = queue_a.get(timeout=1)
            self.assertEqual("Message %d" % i, msg.content.body)

        msg = queue_b.get(timeout=1)
        self.assertEqual("Message 6", msg.content.body)

        msg = queue_c.get(timeout=1)
        self.assertEqual("Message 7", msg.content.body)

        for q in [queue_a, queue_b, queue_c]:
            try:
                extra = q.get(timeout=1)
                self.fail("Got unexpected message: " + extra.content.body)
            except Empty: None

        #cleanup
        channel.basic_ack(delivery_tag=0, multiple=True)
        channel.tx_commit()

    def perform_txn_work(self, channel, name_a, name_b, name_c):
        """
        Utility method that does some setup and some work under a transaction. Used for testing both
        commit and rollback
        """
        #setup:
        channel.queue_declare(queue=name_a, exclusive=True)
        channel.queue_declare(queue=name_b, exclusive=True)
        channel.queue_declare(queue=name_c, exclusive=True)

        key = "my_key_" + name_b
        topic = "my_topic_" + name_c 
    
        channel.queue_bind(queue=name_b, exchange="amq.direct", routing_key=key)
        channel.queue_bind(queue=name_c, exchange="amq.topic", routing_key=topic)

        for i in range(1, 5):
            channel.basic_publish(routing_key=name_a, content=Content("Message %d" % i))

        channel.basic_publish(routing_key=key, exchange="amq.direct", content=Content("Message 6"))
        channel.basic_publish(routing_key=topic, exchange="amq.topic", content=Content("Message 7"))

        channel.tx_select()

        #consume and ack messages
        sub_a = channel.basic_consume(queue=name_a, no_ack=False)
        queue_a = self.client.queue(sub_a.consumer_tag)
        for i in range(1, 5):
            msg = queue_a.get(timeout=1)
            self.assertEqual("Message %d" % i, msg.content.body)
        channel.basic_ack(delivery_tag=msg.delivery_tag, multiple=True)    

        sub_b = channel.basic_consume(queue=name_b, no_ack=False)
        queue_b = self.client.queue(sub_b.consumer_tag)
        msg = queue_b.get(timeout=1)
        self.assertEqual("Message 6", msg.content.body)
        channel.basic_ack(delivery_tag=msg.delivery_tag)    

        sub_c = channel.basic_consume(queue=name_c, no_ack=False)
        queue_c = self.client.queue(sub_c.consumer_tag)
        msg = queue_c.get(timeout=1)
        self.assertEqual("Message 7", msg.content.body)
        channel.basic_ack(delivery_tag=msg.delivery_tag)    

        #publish messages
        for i in range(1, 5):
            channel.basic_publish(routing_key=topic, exchange="amq.topic", content=Content("TxMessage %d" % i))

        channel.basic_publish(routing_key=key, exchange="amq.direct", content=Content("TxMessage 6"))
        channel.basic_publish(routing_key=name_a, content=Content("TxMessage 7"))

        return queue_a, queue_b, queue_c

    def test_commit_overlapping_acks(self):
        """
        Test that logically 'overlapping' acks do not cause errors on commit
        """
        channel = self.channel
        channel.queue_declare(queue="commit-overlapping", exclusive=True)
        for i in range(1, 10):
            channel.basic_publish(routing_key="commit-overlapping", content=Content("Message %d" % i))

        
        channel.tx_select()

        sub = channel.basic_consume(queue="commit-overlapping", no_ack=False)
        queue = self.client.queue(sub.consumer_tag)
        for i in range(1, 10):
            msg = queue.get(timeout=1)
            self.assertEqual("Message %d" % i, msg.content.body)
            if i in [3, 6, 10]:
                channel.basic_ack(delivery_tag=msg.delivery_tag)    
                
        channel.tx_commit()

        #check all have been acked:
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message: " + extra.content.body)
        except Empty: None
