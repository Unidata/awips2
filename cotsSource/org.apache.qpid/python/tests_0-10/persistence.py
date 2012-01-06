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
from qpid.datatypes import Message, RangedSet
#from qpid.testlib import testrunner, TestBase010
from qpid.testlib import TestBase010

class PersistenceTests(TestBase010):
    def test_delete_queue_after_publish(self):
        session = self.session
        session.auto_sync = False

        #create queue
        session.queue_declare(queue = "q", auto_delete=True, durable=True)

        #send message
        for i in range(1, 10):
            dp = session.delivery_properties(routing_key="q", delivery_mode=2)
            session.message_transfer(message=Message(dp, "my-message"))

        session.auto_sync = True
        #explicitly delete queue
        session.queue_delete(queue = "q")

    def test_ack_message_from_deleted_queue(self):
        session = self.session
        session.auto_sync = False

        #create queue
        session.queue_declare(queue = "q", auto_delete=True, durable=True)

        #send message
        dp = session.delivery_properties(routing_key="q", delivery_mode=2)
        session.message_transfer(message=Message(dp, "my-message"))

        #create consumer
        session.message_subscribe(queue = "q", destination = "a", accept_mode = 1, acquire_mode=0)
        session.message_flow(unit = session.credit_unit.byte, value = 0xFFFFFFFFL, destination = "a")
        session.message_flow(unit = session.credit_unit.message, value = 10, destination = "a")
        queue = session.incoming("a")

        #consume the message, cancel subscription (triggering auto-delete), then ack it
        msg = queue.get(timeout = 5)
        session.message_cancel(destination = "a")
        session.message_accept(RangedSet(msg.id))

    def test_queue_deletion(self):
        session = self.session
        session.queue_declare(queue = "durable-subscriber-queue", exclusive=True, durable=True)
        session.exchange_bind(exchange="amq.topic", queue="durable-subscriber-queue", binding_key="xyz")
        dp = session.delivery_properties(routing_key="xyz", delivery_mode=2)
        session.message_transfer(destination="amq.topic", message=Message(dp, "my-message"))
        session.queue_delete(queue = "durable-subscriber-queue")
