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

"""
Tests for exchange behaviour.

Test classes ending in 'RuleTests' are derived from rules in amqp.xml.
"""

import Queue, logging
from qpid.testlib import TestBase
from qpid.content import Content
from qpid.client import Closed


class StandardExchangeVerifier:
    """Verifies standard exchange behavior.

    Used as base class for classes that test standard exchanges."""

    def verifyDirectExchange(self, ex):
        """Verify that ex behaves like a direct exchange."""
        self.queue_declare(queue="q")
        self.channel.queue_bind(queue="q", exchange=ex, routing_key="k")
        self.assertPublishConsume(exchange=ex, queue="q", routing_key="k")
        try:
            self.assertPublishConsume(exchange=ex, queue="q", routing_key="kk")
            self.fail("Expected Empty exception")
        except Queue.Empty: None # Expected

    def verifyFanOutExchange(self, ex):
        """Verify that ex behaves like a fanout exchange."""
        self.queue_declare(queue="q") 
        self.channel.queue_bind(queue="q", exchange=ex)
        self.queue_declare(queue="p") 
        self.channel.queue_bind(queue="p", exchange=ex)
        for qname in ["q", "p"]: self.assertPublishGet(self.consume(qname), ex)

    def verifyTopicExchange(self, ex):
        """Verify that ex behaves like a topic exchange"""
        self.queue_declare(queue="a")
        self.channel.queue_bind(queue="a", exchange=ex, routing_key="a.#.b.*")
        q = self.consume("a")
        self.assertPublishGet(q, ex, "a.b.x")
        self.assertPublishGet(q, ex, "a.x.b.x")
        self.assertPublishGet(q, ex, "a.x.x.b.x")
        # Shouldn't match
        self.channel.basic_publish(exchange=ex, routing_key="a.b")        
        self.channel.basic_publish(exchange=ex, routing_key="a.b.x.y")        
        self.channel.basic_publish(exchange=ex, routing_key="x.a.b.x")        
        self.channel.basic_publish(exchange=ex, routing_key="a.b")
        self.assert_(q.empty())

    def verifyHeadersExchange(self, ex):
        """Verify that ex is a headers exchange"""
        self.queue_declare(queue="q")
        self.channel.queue_bind(queue="q", exchange=ex, arguments={ "x-match":"all", "name":"fred" , "age":3} )
        q = self.consume("q")
        headers = {"name":"fred", "age":3}
        self.assertPublishGet(q, exchange=ex, properties={'headers':headers})
        self.channel.basic_publish(exchange=ex) # No headers, won't deliver
        self.assertEmpty(q);                 
        

class RecommendedTypesRuleTests(TestBase, StandardExchangeVerifier):
    """
    The server SHOULD implement these standard exchange types: topic, headers.
    
    Client attempts to declare an exchange with each of these standard types.
    """

    def testDirect(self):
        """Declare and test a direct exchange"""
        self.exchange_declare(0, exchange="d", type="direct")
        self.verifyDirectExchange("d")

    def testFanout(self):
        """Declare and test a fanout exchange"""
        self.exchange_declare(0, exchange="f", type="fanout")
        self.verifyFanOutExchange("f")

    def testTopic(self):
        """Declare and test a topic exchange"""
        self.exchange_declare(0, exchange="t", type="topic")
        self.verifyTopicExchange("t")

    def testHeaders(self):
        """Declare and test a headers exchange"""
        self.exchange_declare(0, exchange="h", type="headers")
        self.verifyHeadersExchange("h")
        

class RequiredInstancesRuleTests(TestBase, StandardExchangeVerifier):
    """
    The server MUST, in each virtual host, pre-declare an exchange instance
    for each standard exchange type that it implements, where the name of the
    exchange instance is amq. followed by the exchange type name.
    
    Client creates a temporary queue and attempts to bind to each required
    exchange instance (amq.fanout, amq.direct, and amq.topic, amq.match if
    those types are defined).
    """
    def testAmqDirect(self): self.verifyDirectExchange("amq.direct")

    def testAmqFanOut(self): self.verifyFanOutExchange("amq.fanout")

    def testAmqTopic(self):  self.verifyTopicExchange("amq.topic")
        
    def testAmqMatch(self): self.verifyHeadersExchange("amq.match")

class DefaultExchangeRuleTests(TestBase, StandardExchangeVerifier):
    """
    The server MUST predeclare a direct exchange to act as the default exchange
    for content Publish methods and for default queue bindings.
    
    Client checks that the default exchange is active by specifying a queue
    binding with no exchange name, and publishing a message with a suitable
    routing key but without specifying the exchange name, then ensuring that
    the message arrives in the queue correctly.
    """
    def testDefaultExchange(self):
        # Test automatic binding by queue name.
        self.queue_declare(queue="d")
        self.assertPublishConsume(queue="d", routing_key="d")
        # Test explicit bind to default queue
        self.verifyDirectExchange("")


# TODO aconway 2006-09-27: Fill in empty tests:

class DefaultAccessRuleTests(TestBase):
    """
    The server MUST NOT allow clients to access the default exchange except
    by specifying an empty exchange name in the Queue.Bind and content Publish
    methods.
    """

class ExtensionsRuleTests(TestBase):
    """
    The server MAY implement other exchange types as wanted.
    """


class DeclareMethodMinimumRuleTests(TestBase):
    """
    The server SHOULD support a minimum of 16 exchanges per virtual host and
    ideally, impose no limit except as defined by available resources.
    
    The client creates as many exchanges as it can until the server reports
    an error; the number of exchanges successfuly created must be at least
    sixteen.
    """


class DeclareMethodTicketFieldValidityRuleTests(TestBase):
    """
    The client MUST provide a valid access ticket giving "active" access to
    the realm in which the exchange exists or will be created, or "passive"
    access if the if-exists flag is set.
    
    Client creates access ticket with wrong access rights and attempts to use
    in this method.
    """


class DeclareMethodExchangeFieldReservedRuleTests(TestBase):
    """
    Exchange names starting with "amq." are reserved for predeclared and
    standardised exchanges. The client MUST NOT attempt to create an exchange
    starting with "amq.".
    
    
    """


class DeclareMethodTypeFieldTypedRuleTests(TestBase):
    """
    Exchanges cannot be redeclared with different types.  The client MUST not
    attempt to redeclare an existing exchange with a different type than used
    in the original Exchange.Declare method.
    
    
    """


class DeclareMethodTypeFieldSupportRuleTests(TestBase):
    """
    The client MUST NOT attempt to create an exchange with a type that the
    server does not support.
    
    
    """


class DeclareMethodPassiveFieldNotFoundRuleTests(TestBase):
    """
    If set, and the exchange does not already exist, the server MUST raise a
    channel exception with reply code 404 (not found).    
    """
    def test(self):
        try:
            self.channel.exchange_declare(exchange="humpty_dumpty", passive=True)
            self.fail("Expected 404 for passive declaration of unknown exchange.")
        except Closed, e:
            self.assertChannelException(404, e.args[0])


class DeclareMethodDurableFieldSupportRuleTests(TestBase):
    """
    The server MUST support both durable and transient exchanges.
    
    
    """


class DeclareMethodDurableFieldStickyRuleTests(TestBase):
    """
    The server MUST ignore the durable field if the exchange already exists.
    
    
    """


class DeclareMethodAutoDeleteFieldStickyRuleTests(TestBase):
    """
    The server MUST ignore the auto-delete field if the exchange already
    exists.
    
    
    """


class DeleteMethodTicketFieldValidityRuleTests(TestBase):
    """
    The client MUST provide a valid access ticket giving "active" access
    rights to the exchange's access realm.
    
    Client creates access ticket with wrong access rights and attempts to use
    in this method.
    """


class DeleteMethodExchangeFieldExistsRuleTests(TestBase):
    """
    The client MUST NOT attempt to delete an exchange that does not exist.
    """


class HeadersExchangeTests(TestBase):
    """
    Tests for headers exchange functionality.
    """
    def setUp(self):
        TestBase.setUp(self)
        self.queue_declare(queue="q")
        self.q = self.consume("q")

    def myAssertPublishGet(self, headers):
        self.assertPublishGet(self.q, exchange="amq.match", properties={'headers':headers})

    def myBasicPublish(self, headers):
        self.channel.basic_publish(exchange="amq.match", content=Content("foobar", properties={'headers':headers}))
        
    def testMatchAll(self):
        self.channel.queue_bind(queue="q", exchange="amq.match", arguments={ 'x-match':'all', "name":"fred", "age":3})
        self.myAssertPublishGet({"name":"fred", "age":3})
        self.myAssertPublishGet({"name":"fred", "age":3, "extra":"ignoreme"})
        
        # None of these should match
        self.myBasicPublish({})
        self.myBasicPublish({"name":"barney"})
        self.myBasicPublish({"name":10})
        self.myBasicPublish({"name":"fred", "age":2})
        self.assertEmpty(self.q)

    def testMatchAny(self):
        self.channel.queue_bind(queue="q", exchange="amq.match", arguments={ 'x-match':'any', "name":"fred", "age":3})
        self.myAssertPublishGet({"name":"fred"})
        self.myAssertPublishGet({"name":"fred", "ignoreme":10})
        self.myAssertPublishGet({"ignoreme":10, "age":3})

        # Wont match
        self.myBasicPublish({})
        self.myBasicPublish({"irrelevant":0})
        self.assertEmpty(self.q)


class MiscellaneousErrorsTests(TestBase):
    """
    Test some miscellaneous error conditions
    """
    def testTypeNotKnown(self):
        try:
            self.channel.exchange_declare(exchange="test_type_not_known_exchange", type="invalid_type")
            self.fail("Expected 503 for declaration of unknown exchange type.")
        except Closed, e:
            self.assertConnectionException(503, e.args[0])

    def testDifferentDeclaredType(self):
        self.channel.exchange_declare(exchange="test_different_declared_type_exchange", type="direct")
        try:
            self.channel.exchange_declare(exchange="test_different_declared_type_exchange", type="topic")
            self.fail("Expected 530 for redeclaration of exchange with different type.")
        except Closed, e:
            self.assertConnectionException(530, e.args[0])
        #cleanup    
        other = self.connect()
        c2 = other.channel(1)
        c2.channel_open()
        c2.exchange_delete(exchange="test_different_declared_type_exchange")
    
