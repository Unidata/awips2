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

import Queue, logging, traceback
from qpid.testlib import TestBase010
from qpid.datatypes import Message
from qpid.client import Closed
from qpid.session import SessionException


class TestHelper(TestBase010):
    def setUp(self):
        TestBase010.setUp(self)
        self.queues = []
        self.exchanges = []

    def tearDown(self):
        try:
            for ssn, q in self.queues:
                ssn.queue_delete(queue=q)
            for ssn, ex in self.exchanges:
                ssn.exchange_delete(exchange=ex)
        except:
            print "Error on tearDown:"
            print traceback.print_exc()
        TestBase010.tearDown(self)

    def createMessage(self, key="", body=""):
        return Message(self.session.delivery_properties(routing_key=key), body)

    def getApplicationHeaders(self, msg):
        for h in msg.headers:
            if hasattr(h, 'application_headers'): return getattr(h, 'application_headers')
        return None            

    def assertPublishGet(self, queue, exchange="", routing_key="", properties=None):
        """
        Publish to exchange and assert queue.get() returns the same message.
        """
        body = self.uniqueString()
        dp=self.session.delivery_properties(routing_key=routing_key)
        mp=self.session.message_properties(application_headers=properties)
        self.session.message_transfer(destination=exchange, message=Message(dp, mp, body))
        msg = queue.get(timeout=1)
        self.assertEqual(body, msg.body)
        if (properties):
            self.assertEqual(properties, self.getApplicationHeaders(msg))

    def assertPublishConsume(self, queue="", exchange="", routing_key="", properties=None):
        """
        Publish a message and consume it, assert it comes back intact.
        Return the Queue object used to consume.
        """
        self.assertPublishGet(self.consume(queue), exchange, routing_key, properties)

    def assertEmpty(self, queue):
        """Assert that the queue is empty"""
        try:
            queue.get(timeout=1)
            self.fail("Queue is not empty.")
        except Queue.Empty: None              # Ignore
        
    def queue_declare(self, session=None, *args, **keys):
        session = session or self.session
        reply = session.queue_declare(*args, **keys)
        self.queues.append((session, keys["queue"]))
        return reply

    def exchange_declare(self, session=None, ticket=0, exchange='',
                         type='', passive=False, durable=False,
                         auto_delete=False,
                         arguments={}):
        session = session or self.session
        reply = session.exchange_declare(exchange=exchange, type=type, passive=passive,durable=durable, auto_delete=auto_delete, arguments=arguments)
        self.exchanges.append((session,exchange))
        return reply

    def uniqueString(self):
        """Generate a unique string, unique for this TestBase instance"""
        if not "uniqueCounter" in dir(self): self.uniqueCounter = 1;
        return "Test Message " + str(self.uniqueCounter)

    def consume(self, queueName):
        """Consume from named queue returns the Queue object."""
        if not "uniqueTag" in dir(self): self.uniqueTag = 1
        else: self.uniqueTag += 1
        consumer_tag = "tag" + str(self.uniqueTag)
        self.session.message_subscribe(queue=queueName, destination=consumer_tag)
        self.session.message_flow(destination=consumer_tag, unit=self.session.credit_unit.message, value=0xFFFFFFFFL)
        self.session.message_flow(destination=consumer_tag, unit=self.session.credit_unit.byte, value=0xFFFFFFFFL)
        return self.session.incoming(consumer_tag)


class StandardExchangeVerifier:
    """Verifies standard exchange behavior.

    Used as base class for classes that test standard exchanges."""

    def verifyDirectExchange(self, ex):
        """Verify that ex behaves like a direct exchange."""
        self.queue_declare(queue="q")
        self.session.exchange_bind(queue="q", exchange=ex, binding_key="k")
        self.assertPublishConsume(exchange=ex, queue="q", routing_key="k")
        try:
            self.assertPublishConsume(exchange=ex, queue="q", routing_key="kk")
            self.fail("Expected Empty exception")
        except Queue.Empty: None # Expected

    def verifyFanOutExchange(self, ex):
        """Verify that ex behaves like a fanout exchange."""
        self.queue_declare(queue="q") 
        self.session.exchange_bind(queue="q", exchange=ex)
        self.queue_declare(queue="p") 
        self.session.exchange_bind(queue="p", exchange=ex)
        for qname in ["q", "p"]: self.assertPublishGet(self.consume(qname), ex)

    def verifyTopicExchange(self, ex):
        """Verify that ex behaves like a topic exchange"""
        self.queue_declare(queue="a")
        self.session.exchange_bind(queue="a", exchange=ex, binding_key="a.#.b.*")
        q = self.consume("a")
        self.assertPublishGet(q, ex, "a.b.x")
        self.assertPublishGet(q, ex, "a.x.b.x")
        self.assertPublishGet(q, ex, "a.x.x.b.x")
        # Shouldn't match
        self.session.message_transfer(destination=ex, message=self.createMessage("a.b"))        
        self.session.message_transfer(destination=ex, message=self.createMessage("a.b.x.y"))        
        self.session.message_transfer(destination=ex, message=self.createMessage("x.a.b.x"))        
        self.session.message_transfer(destination=ex, message=self.createMessage("a.b"))
        self.assert_(q.empty())

    def verifyHeadersExchange(self, ex):
        """Verify that ex is a headers exchange"""
        self.queue_declare(queue="q")
        self.session.exchange_bind(queue="q", exchange=ex, arguments={ "x-match":"all", "name":"fred" , "age":3} )
        q = self.consume("q")
        headers = {"name":"fred", "age":3}
        self.assertPublishGet(q, exchange=ex, properties=headers)
        self.session.message_transfer(destination=ex) # No headers, won't deliver
        self.assertEmpty(q);                 
        

class RecommendedTypesRuleTests(TestHelper, StandardExchangeVerifier):
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
        

class RequiredInstancesRuleTests(TestHelper, StandardExchangeVerifier):
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

class DefaultExchangeRuleTests(TestHelper, StandardExchangeVerifier):
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

class DefaultAccessRuleTests(TestHelper):
    """
    The server MUST NOT allow clients to access the default exchange except
    by specifying an empty exchange name in the Queue.Bind and content Publish
    methods.
    """

class ExtensionsRuleTests(TestHelper):
    """
    The server MAY implement other exchange types as wanted.
    """


class DeclareMethodMinimumRuleTests(TestHelper):
    """
    The server SHOULD support a minimum of 16 exchanges per virtual host and
    ideally, impose no limit except as defined by available resources.
    
    The client creates as many exchanges as it can until the server reports
    an error; the number of exchanges successfuly created must be at least
    sixteen.
    """


class DeclareMethodTicketFieldValidityRuleTests(TestHelper):
    """
    The client MUST provide a valid access ticket giving "active" access to
    the realm in which the exchange exists or will be created, or "passive"
    access if the if-exists flag is set.
    
    Client creates access ticket with wrong access rights and attempts to use
    in this method.
    """


class DeclareMethodExchangeFieldReservedRuleTests(TestHelper):
    """
    Exchange names starting with "amq." are reserved for predeclared and
    standardised exchanges. The client MUST NOT attempt to create an exchange
    starting with "amq.".
    
    Similarly, exchanges starting with "qpid." are reserved for Qpid
    implementation-specific system exchanges (such as the management exchange).
    The client must not attempt to create an exchange starting with the string
    "qpid.".
    """
    def template(self, reservedString, exchangeType):
        try:
            self.session.exchange_declare(exchange=reservedString, type=exchangeType)
            self.fail("Expected not allowed error (530) for exchanges starting with \"" + reservedString + "\".")
        except SessionException, e:
            self.assertEquals(e.args[0].error_code, 530)
        # connection closed, reopen it
        self.tearDown()
        self.setUp()
        try:
            self.session.exchange_declare(exchange=reservedString + "abc123", type=exchangeType)
            self.fail("Expected not allowed error (530) for exchanges starting with \"" + reservedString + "\".")
        except SessionException, e:
            self.assertEquals(e.args[0].error_code, 530)
        # connection closed, reopen it
        self.tearDown()
        self.setUp()
        # The following should be legal:
        self.session.exchange_declare(exchange=reservedString[:-1], type=exchangeType)
        self.session.exchange_delete(exchange=reservedString[:-1])
        self.session.exchange_declare(exchange=reservedString[1:], type=exchangeType)
        self.session.exchange_delete(exchange=reservedString[1:])
        self.session.exchange_declare(exchange="." + reservedString, type=exchangeType)
        self.session.exchange_delete(exchange="." + reservedString)
        self.session.exchange_declare(exchange="abc." + reservedString, type=exchangeType)
        self.session.exchange_delete(exchange="abc." + reservedString)
        self.session.exchange_declare(exchange="abc." + reservedString + "def", type=exchangeType)
        self.session.exchange_delete(exchange="abc." + reservedString + "def")

    def test_amq(self):
        self.template("amq.", "direct")
        self.template("amq.", "topic")
        self.template("amq.", "fanout")

    def test_qpid(self):
        self.template("qpid.", "direct")
        self.template("qpid.", "topic")
        self.template("qpid.", "fanout")


class DeclareMethodTypeFieldTypedRuleTests(TestHelper):
    """
    Exchanges cannot be redeclared with different types.  The client MUST not
    attempt to redeclare an existing exchange with a different type than used
    in the original Exchange.Declare method.
    
    
    """


class DeclareMethodTypeFieldSupportRuleTests(TestHelper):
    """
    The client MUST NOT attempt to create an exchange with a type that the
    server does not support.
    
    
    """


class DeclareMethodPassiveFieldNotFoundRuleTests(TestHelper):
    """
    If set, and the exchange does not already exist, the server MUST raise a
    channel exception with reply code 404 (not found).    
    """
    def test(self):
        try:
            self.session.exchange_declare(exchange="humpty_dumpty", passive=True)
            self.fail("Expected 404 for passive declaration of unknown exchange.")
        except SessionException, e:
            self.assertEquals(404, e.args[0].error_code)


class DeclareMethodDurableFieldSupportRuleTests(TestHelper):
    """
    The server MUST support both durable and transient exchanges.
    
    
    """


class DeclareMethodDurableFieldStickyRuleTests(TestHelper):
    """
    The server MUST ignore the durable field if the exchange already exists.
    
    
    """


class DeclareMethodAutoDeleteFieldStickyRuleTests(TestHelper):
    """
    The server MUST ignore the auto-delete field if the exchange already
    exists.
    
    
    """


class DeleteMethodTicketFieldValidityRuleTests(TestHelper):
    """
    The client MUST provide a valid access ticket giving "active" access
    rights to the exchange's access realm.
    
    Client creates access ticket with wrong access rights and attempts to use
    in this method.
    """


class DeleteMethodExchangeFieldExistsRuleTests(TestHelper):
    """
    The client MUST NOT attempt to delete an exchange that does not exist.
    """


class HeadersExchangeTests(TestHelper):
    """
    Tests for headers exchange functionality.
    """
    def setUp(self):
        TestHelper.setUp(self)
        self.queue_declare(queue="q")
        self.q = self.consume("q")

    def myAssertPublishGet(self, headers):
        self.assertPublishGet(self.q, exchange="amq.match", properties=headers)

    def myBasicPublish(self, headers):
        mp=self.session.message_properties(application_headers=headers)
        self.session.message_transfer(destination="amq.match", message=Message(mp, "foobar"))
        
    def testMatchAll(self):
        self.session.exchange_bind(queue="q", exchange="amq.match", arguments={ 'x-match':'all', "name":"fred", "age":3})
        self.myAssertPublishGet({"name":"fred", "age":3})
        self.myAssertPublishGet({"name":"fred", "age":3, "extra":"ignoreme"})
        
        # None of these should match
        self.myBasicPublish({})
        self.myBasicPublish({"name":"barney"})
        self.myBasicPublish({"name":10})
        self.myBasicPublish({"name":"fred", "age":2})
        self.assertEmpty(self.q)

    def testMatchAny(self):
        self.session.exchange_bind(queue="q", exchange="amq.match", arguments={ 'x-match':'any', "name":"fred", "age":3})
        self.myAssertPublishGet({"name":"fred"})
        self.myAssertPublishGet({"name":"fred", "ignoreme":10})
        self.myAssertPublishGet({"ignoreme":10, "age":3})

        # Wont match
        self.myBasicPublish({})
        self.myBasicPublish({"irrelevant":0})
        self.assertEmpty(self.q)


class MiscellaneousErrorsTests(TestHelper):
    """
    Test some miscellaneous error conditions
    """
    def testTypeNotKnown(self):
        try:
            self.session.exchange_declare(exchange="test_type_not_known_exchange", type="invalid_type")
            self.fail("Expected 503 for declaration of unknown exchange type.")
        except SessionException, e:
            self.assertEquals(503, e.args[0].error_code)

    def testDifferentDeclaredType(self):
        self.exchange_declare(exchange="test_different_declared_type_exchange", type="direct")
        try:
            session = self.conn.session("alternate", 2)
            session.exchange_declare(exchange="test_different_declared_type_exchange", type="topic")
            self.fail("Expected 530 for redeclaration of exchange with different type.")
        except SessionException, e:
            self.assertEquals(530, e.args[0].error_code)
    
class ExchangeTests(TestHelper):
    def testHeadersBindNoMatchArg(self):
        self.session.queue_declare(queue="q", exclusive=True, auto_delete=True)
        try: 
            self.session.exchange_bind(queue="q", exchange="amq.match", arguments={"name":"fred" , "age":3} )
            self.fail("Expected failure for missing x-match arg.")
        except SessionException, e:    
            self.assertEquals(541, e.args[0].error_code)
