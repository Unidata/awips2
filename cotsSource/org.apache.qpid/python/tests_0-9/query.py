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

class QueryTests(TestBase):
    """Tests for various query methods introduced in 0-10 and available in 0-9 for preview"""

    def test_exchange_query(self):
        """
        Test that the exchange_query method works as expected
        """
        channel = self.channel
        #check returned type for the standard exchanges
        self.assertEqual("direct", channel.exchange_query(name="amq.direct").type)
        self.assertEqual("topic", channel.exchange_query(name="amq.topic").type)
        self.assertEqual("fanout", channel.exchange_query(name="amq.fanout").type)
        self.assertEqual("headers", channel.exchange_query(name="amq.match").type)
        self.assertEqual("direct", channel.exchange_query(name="").type)        
        #declare an exchange
        channel.exchange_declare(exchange="my-test-exchange", type= "direct", durable=False)
        #check that the result of a query is as expected
        response = channel.exchange_query(name="my-test-exchange")
        self.assertEqual("direct", response.type)
        self.assertEqual(False, response.durable)
        self.assertEqual(False, response.not_found)
        #delete the exchange
        channel.exchange_delete(exchange="my-test-exchange")
        #check that the query now reports not-found
        self.assertEqual(True, channel.exchange_query(name="my-test-exchange").not_found)

    def test_binding_query_direct(self):
        """
        Test that the binding_query method works as expected with the direct exchange
        """
        self.binding_query_with_key("amq.direct")

    def test_binding_query_topic(self):
        """
        Test that the binding_query method works as expected with the direct exchange
        """
        self.binding_query_with_key("amq.topic")

    def binding_query_with_key(self, exchange_name):
        channel = self.channel
        #setup: create two queues
        channel.queue_declare(queue="used-queue", exclusive=True)
        channel.queue_declare(queue="unused-queue", exclusive=True)
        
        channel.queue_bind(exchange=exchange_name, queue="used-queue", routing_key="used-key")

        # test detection of any binding to specific queue
        response = channel.binding_query(exchange=exchange_name, queue="used-queue")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.queue_not_matched)        

        # test detection of specific binding to any queue
        response = channel.binding_query(exchange=exchange_name, routing_key="used-key")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.key_not_matched)        

        # test detection of specific binding to specific queue
        response = channel.binding_query(exchange=exchange_name, queue="used-queue", routing_key="used-key")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.queue_not_matched)        
        self.assertEqual(False, response.key_not_matched)        

        # test unmatched queue, unspecified binding
        response = channel.binding_query(exchange=exchange_name, queue="unused-queue")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        

        # test unspecified queue, unmatched binding
        response = channel.binding_query(exchange=exchange_name, routing_key="unused-key")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.key_not_matched)        

        # test matched queue, unmatched binding
        response = channel.binding_query(exchange=exchange_name, queue="used-queue", routing_key="unused-key")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.queue_not_matched)        
        self.assertEqual(True, response.key_not_matched)        

        # test unmatched queue, matched binding
        response = channel.binding_query(exchange=exchange_name, queue="unused-queue", routing_key="used-key")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assertEqual(False, response.key_not_matched)        

        # test unmatched queue, unmatched binding
        response = channel.binding_query(exchange=exchange_name, queue="unused-queue", routing_key="unused-key")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assertEqual(True, response.key_not_matched)        

        #test exchange not found
        self.assertEqual(True, channel.binding_query(exchange="unknown-exchange").exchange_not_found)

        #test queue not found
        self.assertEqual(True, channel.binding_query(exchange=exchange_name, queue="unknown-queue").queue_not_found)


    def test_binding_query_fanout(self):
        """
        Test that the binding_query method works as expected with fanout exchange
        """
        channel = self.channel
        #setup
        channel.queue_declare(queue="used-queue", exclusive=True)
        channel.queue_declare(queue="unused-queue", exclusive=True)
        channel.queue_bind(exchange="amq.fanout", queue="used-queue")

        # test detection of any binding to specific queue
        response = channel.binding_query(exchange="amq.fanout", queue="used-queue")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.queue_not_matched)        

        # test unmatched queue, unspecified binding
        response = channel.binding_query(exchange="amq.fanout", queue="unused-queue")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        

        #test exchange not found
        self.assertEqual(True, channel.binding_query(exchange="unknown-exchange").exchange_not_found)

        #test queue not found
        self.assertEqual(True, channel.binding_query(exchange="amq.fanout", queue="unknown-queue").queue_not_found)

    def test_binding_query_header(self):
        """
        Test that the binding_query method works as expected with headers exchanges
        """
        channel = self.channel
        #setup
        channel.queue_declare(queue="used-queue", exclusive=True)
        channel.queue_declare(queue="unused-queue", exclusive=True)
        channel.queue_bind(exchange="amq.match", queue="used-queue", arguments={"x-match":"all", "a":"A"} )

        # test detection of any binding to specific queue
        response = channel.binding_query(exchange="amq.match", queue="used-queue")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.queue_not_matched)        

        # test detection of specific binding to any queue
        response = channel.binding_query(exchange="amq.match", arguments={"x-match":"all", "a":"A"})
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.args_not_matched)        

        # test detection of specific binding to specific queue
        response = channel.binding_query(exchange="amq.match", queue="used-queue", arguments={"x-match":"all", "a":"A"})
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.queue_not_matched)        
        self.assertEqual(False, response.args_not_matched)        

        # test unmatched queue, unspecified binding
        response = channel.binding_query(exchange="amq.match", queue="unused-queue")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        

        # test unspecified queue, unmatched binding
        response = channel.binding_query(exchange="amq.match", arguments={"x-match":"all", "b":"B"})
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.args_not_matched)        

        # test matched queue, unmatched binding
        response = channel.binding_query(exchange="amq.match", queue="used-queue", arguments={"x-match":"all", "b":"B"})
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(False, response.queue_not_matched)        
        self.assertEqual(True, response.args_not_matched)        

        # test unmatched queue, matched binding
        response = channel.binding_query(exchange="amq.match", queue="unused-queue", arguments={"x-match":"all", "a":"A"})
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assertEqual(False, response.args_not_matched)        

        # test unmatched queue, unmatched binding
        response = channel.binding_query(exchange="amq.match", queue="unused-queue", arguments={"x-match":"all", "b":"B"})
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assertEqual(True, response.args_not_matched)        

        #test exchange not found
        self.assertEqual(True, channel.binding_query(exchange="unknown-exchange").exchange_not_found)

        #test queue not found
        self.assertEqual(True, channel.binding_query(exchange="amq.match", queue="unknown-queue").queue_not_found)
        
