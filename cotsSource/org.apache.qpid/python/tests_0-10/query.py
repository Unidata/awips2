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
from qpid.testlib import TestBase010

class QueryTests(TestBase010):
    """Tests for various query methods"""

    def test_queue_query(self):
        session = self.session
        session.queue_declare(queue="my-queue", exclusive=True)
        result = session.queue_query(queue="my-queue")
        self.assertEqual("my-queue", result.queue)

    def test_queue_query_unknown(self):
        session = self.session
        result = session.queue_query(queue="I don't exist")
        self.assert_(not result.queue)

    def test_exchange_query(self):
        """
        Test that the exchange_query method works as expected
        """
        session = self.session
        #check returned type for the standard exchanges
        self.assertEqual("direct", session.exchange_query(name="amq.direct").type)
        self.assertEqual("topic", session.exchange_query(name="amq.topic").type)
        self.assertEqual("fanout", session.exchange_query(name="amq.fanout").type)
        self.assertEqual("headers", session.exchange_query(name="amq.match").type)
        self.assertEqual("direct", session.exchange_query(name="").type)        
        #declare an exchange
        session.exchange_declare(exchange="my-test-exchange", type= "direct", durable=False)
        #check that the result of a query is as expected
        response = session.exchange_query(name="my-test-exchange")
        self.assertEqual("direct", response.type)
        self.assert_(not response.durable)
        self.assert_(not response.not_found)
        #delete the exchange
        session.exchange_delete(exchange="my-test-exchange")
        #check that the query now reports not-found
        self.assert_(session.exchange_query(name="my-test-exchange").not_found)

    def test_exchange_bound_direct(self):
        """
        Test that the exchange_bound method works as expected with the direct exchange
        """
        self.exchange_bound_with_key("amq.direct")

    def test_exchange_bound_topic(self):
        """
        Test that the exchange_bound method works as expected with the direct exchange
        """
        self.exchange_bound_with_key("amq.topic")

    def exchange_bound_with_key(self, exchange_name):
        session = self.session
        #setup: create two queues
        session.queue_declare(queue="used-queue", exclusive=True, auto_delete=True)
        session.queue_declare(queue="unused-queue", exclusive=True, auto_delete=True)
        
        session.exchange_bind(exchange=exchange_name, queue="used-queue", binding_key="used-key")

        # test detection of any binding to specific queue
        response = session.exchange_bound(exchange=exchange_name, queue="used-queue")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.queue_not_matched)        

        # test detection of specific binding to any queue
        response = session.exchange_bound(exchange=exchange_name, binding_key="used-key")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.key_not_matched)        

        # test detection of specific binding to specific queue
        response = session.exchange_bound(exchange=exchange_name, queue="used-queue", binding_key="used-key")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.queue_not_matched)        
        self.assert_(not response.key_not_matched)        

        # test unmatched queue, unspecified binding
        response = session.exchange_bound(exchange=exchange_name, queue="unused-queue")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        

        # test unspecified queue, unmatched binding
        response = session.exchange_bound(exchange=exchange_name, binding_key="unused-key")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.key_not_matched)        

        # test matched queue, unmatched binding
        response = session.exchange_bound(exchange=exchange_name, queue="used-queue", binding_key="unused-key")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.queue_not_matched)        
        self.assertEqual(True, response.key_not_matched)        

        # test unmatched queue, matched binding
        response = session.exchange_bound(exchange=exchange_name, queue="unused-queue", binding_key="used-key")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assert_(not response.key_not_matched)        

        # test unmatched queue, unmatched binding
        response = session.exchange_bound(exchange=exchange_name, queue="unused-queue", binding_key="unused-key")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assertEqual(True, response.key_not_matched)        

        #test exchange not found
        self.assertEqual(True, session.exchange_bound(exchange="unknown-exchange").exchange_not_found)

        #test exchange found, queue not found
        response = session.exchange_bound(exchange=exchange_name, queue="unknown-queue")
        self.assertEqual(False, response.exchange_not_found)
        self.assertEqual(True, response.queue_not_found)

        #test exchange not found, queue found
        response = session.exchange_bound(exchange="unknown-exchange", queue="used-queue")
        self.assertEqual(True, response.exchange_not_found)
        self.assertEqual(False, response.queue_not_found)

        #test not exchange found, queue not found
        response = session.exchange_bound(exchange="unknown-exchange", queue="unknown-queue")
        self.assertEqual(True, response.exchange_not_found)
        self.assertEqual(True, response.queue_not_found)


    def test_exchange_bound_fanout(self):
        """
        Test that the exchange_bound method works as expected with fanout exchange
        """
        session = self.session
        #setup
        session.queue_declare(queue="used-queue", exclusive=True, auto_delete=True)
        session.queue_declare(queue="unused-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(exchange="amq.fanout", queue="used-queue")

        # test detection of any binding to specific queue
        response = session.exchange_bound(exchange="amq.fanout", queue="used-queue")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.queue_not_matched)        

        # test unmatched queue, unspecified binding
        response = session.exchange_bound(exchange="amq.fanout", queue="unused-queue")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        

        #test exchange not found
        self.assertEqual(True, session.exchange_bound(exchange="unknown-exchange").exchange_not_found)

        #test queue not found
        self.assertEqual(True, session.exchange_bound(exchange="amq.fanout", queue="unknown-queue").queue_not_found)

    def test_exchange_bound_header(self):
        """
        Test that the exchange_bound method works as expected with headers exchanges
        """
        session = self.session
        #setup
        session.queue_declare(queue="used-queue", exclusive=True, auto_delete=True)
        session.queue_declare(queue="unused-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(exchange="amq.match", queue="used-queue", arguments={"x-match":"all", "a":"A"} )

        # test detection of any binding to specific queue
        response = session.exchange_bound(exchange="amq.match", queue="used-queue")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.queue_not_matched)        

        # test detection of specific binding to any queue
        response = session.exchange_bound(exchange="amq.match", arguments={"x-match":"all", "a":"A"})
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.args_not_matched)        

        # test detection of specific binding to specific queue
        response = session.exchange_bound(exchange="amq.match", queue="used-queue", arguments={"x-match":"all", "a":"A"})
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.queue_not_matched)        
        self.assert_(not response.args_not_matched)        

        # test unmatched queue, unspecified binding
        response = session.exchange_bound(exchange="amq.match", queue="unused-queue")
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        

        # test unspecified queue, unmatched binding
        response = session.exchange_bound(exchange="amq.match", arguments={"x-match":"all", "b":"B"})
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.args_not_matched)        

        # test matched queue, unmatched binding
        response = session.exchange_bound(exchange="amq.match", queue="used-queue", arguments={"x-match":"all", "b":"B"})
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assert_(not response.queue_not_matched)        
        self.assertEqual(True, response.args_not_matched)        

        # test unmatched queue, matched binding
        response = session.exchange_bound(exchange="amq.match", queue="unused-queue", arguments={"x-match":"all", "a":"A"})
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assert_(not response.args_not_matched)        

        # test unmatched queue, unmatched binding
        response = session.exchange_bound(exchange="amq.match", queue="unused-queue", arguments={"x-match":"all", "b":"B"})
        self.assert_(not response.exchange_not_found)
        self.assert_(not response.queue_not_found)
        self.assertEqual(True, response.queue_not_matched)        
        self.assertEqual(True, response.args_not_matched)        

        #test exchange not found
        self.assertEqual(True, session.exchange_bound(exchange="unknown-exchange").exchange_not_found)

        #test queue not found
        self.assertEqual(True, session.exchange_bound(exchange="amq.match", queue="unknown-queue").queue_not_found)
        
