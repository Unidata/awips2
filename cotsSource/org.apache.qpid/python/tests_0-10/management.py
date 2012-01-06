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
from qpid.testlib import TestBase010
from qpid.management import managementChannel, managementClient
from threading import Condition
from time import sleep
import qmf.console

class ManagementTest (TestBase010):
    """
    Tests for the management hooks
    """

    def test_broker_connectivity_oldAPI (self):
        """
        Call the "echo" method on the broker to verify it is alive and talking.
        """
        session = self.session
 
        mc  = managementClient ()
        mch = mc.addChannel (session)

        mc.syncWaitForStable (mch)
        brokers = mc.syncGetObjects (mch, "broker")
        self.assertEqual (len (brokers), 1)
        broker = brokers[0]
        args = {}
        body = "Echo Message Body"
        args["body"] = body

        for seq in range (1, 5):
            args["sequence"] = seq
            res = mc.syncCallMethod (mch, broker.id, broker.classKey, "echo", args)
            self.assertEqual (res.status,     0)
            self.assertEqual (res.statusText, "OK")
            self.assertEqual (res.sequence,   seq)
            self.assertEqual (res.body,       body)
        mc.removeChannel (mch)

    def test_methods_sync (self):
        """
        Call the "echo" method on the broker to verify it is alive and talking.
        """
        session = self.session
        self.startQmf()
 
        brokers = self.qmf.getObjects(_class="broker")
        self.assertEqual(len(brokers), 1)
        broker = brokers[0]

        body = "Echo Message Body"
        for seq in range(1, 20):
            res = broker.echo(seq, body)
            self.assertEqual(res.status, 0)
            self.assertEqual(res.text, "OK")
            self.assertEqual(res.sequence, seq)
            self.assertEqual(res.body, body)

    def test_get_objects(self):
        self.startQmf()

        # get the package list, verify that the qpid broker package is there
        packages = self.qmf.getPackages()
        assert 'org.apache.qpid.broker' in packages

        # get the schema class keys for the broker, verify the broker table and link-down event
        keys = self.qmf.getClasses('org.apache.qpid.broker')
        broker = None
        linkDown = None
        for key in keys:
            if key.getClassName() == "broker":  broker = key
            if key.getClassName() == "brokerLinkDown" : linkDown = key
        assert broker
        assert linkDown

        brokerObjs = self.qmf.getObjects(_class="broker")
        assert len(brokerObjs) == 1
        brokerObjs = self.qmf.getObjects(_key=broker)
        assert len(brokerObjs) == 1

    def test_self_session_id (self):
        self.startQmf()
        sessionId = self.qmf_broker.getSessionId()
        brokerSessions = self.qmf.getObjects(_class="session")

        found = False
        for bs in brokerSessions:
            if bs.name == sessionId:
                found = True
        self.assertEqual (found, True)

    def test_standard_exchanges (self):
        self.startQmf()

        exchanges = self.qmf.getObjects(_class="exchange")
        exchange = self.findExchange (exchanges, "")
        self.assertEqual (exchange.type, "direct")
        exchange = self.findExchange (exchanges, "amq.direct")
        self.assertEqual (exchange.type, "direct")
        exchange = self.findExchange (exchanges, "amq.topic")
        self.assertEqual (exchange.type, "topic")
        exchange = self.findExchange (exchanges, "amq.fanout")
        self.assertEqual (exchange.type, "fanout")
        exchange = self.findExchange (exchanges, "amq.match")
        self.assertEqual (exchange.type, "headers")
        exchange = self.findExchange (exchanges, "qpid.management")
        self.assertEqual (exchange.type, "topic")

    def findExchange (self, exchanges, name):
        for exchange in exchanges:
            if exchange.name == name:
                return exchange
        return None

    def test_move_queued_messages(self):
        """
        Test ability to move messages from the head of one queue to another.
        Need to test moveing all and N messages.
        """
        self.startQmf()
        session = self.session
        "Set up source queue"
        session.queue_declare(queue="src-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="src-queue", exchange="amq.direct", binding_key="routing_key")

        twenty = range(1,21)
        props = session.delivery_properties(routing_key="routing_key")
        for count in twenty:
            body = "Move Message %d" % count
            src_msg = Message(props, body)
            session.message_transfer(destination="amq.direct", message=src_msg)

        "Set up destination queue"
        session.queue_declare(queue="dest-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="dest-queue", exchange="amq.direct")

        queues = self.qmf.getObjects(_class="queue")

        "Move 10 messages from src-queue to dest-queue"
        result = self.qmf.getObjects(_class="broker")[0].queueMoveMessages("src-queue", "dest-queue", 10)
        self.assertEqual (result.status, 0) 

        sq = self.qmf.getObjects(_class="queue", name="src-queue")[0]
        dq = self.qmf.getObjects(_class="queue", name="dest-queue")[0]

        self.assertEqual (sq.msgDepth,10)
        self.assertEqual (dq.msgDepth,10)

        "Move all remaining messages to destination"
        result = self.qmf.getObjects(_class="broker")[0].queueMoveMessages("src-queue", "dest-queue", 0)
        self.assertEqual (result.status,0)

        sq = self.qmf.getObjects(_class="queue", name="src-queue")[0]
        dq = self.qmf.getObjects(_class="queue", name="dest-queue")[0]

        self.assertEqual (sq.msgDepth,0)
        self.assertEqual (dq.msgDepth,20)

        "Use a bad source queue name"
        result = self.qmf.getObjects(_class="broker")[0].queueMoveMessages("bad-src-queue", "dest-queue", 0)
        self.assertEqual (result.status,4)

        "Use a bad destination queue name"
        result = self.qmf.getObjects(_class="broker")[0].queueMoveMessages("src-queue", "bad-dest-queue", 0)
        self.assertEqual (result.status,4)

        " Use a large qty (40) to move from dest-queue back to "
        " src-queue- should move all "
        result = self.qmf.getObjects(_class="broker")[0].queueMoveMessages("dest-queue", "src-queue", 40)
        self.assertEqual (result.status,0)

        sq = self.qmf.getObjects(_class="queue", name="src-queue")[0]
        dq = self.qmf.getObjects(_class="queue", name="dest-queue")[0]

        self.assertEqual (sq.msgDepth,20)
        self.assertEqual (dq.msgDepth,0)

        "Consume the messages of the queue and check they are all there in order"
        session.message_subscribe(queue="src-queue", destination="tag")
        session.message_flow(destination="tag", unit=session.credit_unit.message, value=0xFFFFFFFFL)
        session.message_flow(destination="tag", unit=session.credit_unit.byte, value=0xFFFFFFFFL)
        queue = session.incoming("tag")
        for count in twenty:
            consumed_msg = queue.get(timeout=1)
            body = "Move Message %d" % count
            self.assertEqual(body, consumed_msg.body)

    def test_purge_queue(self):
        """
        Test ability to purge messages from the head of a queue.
        Need to test moveing all, 1 (top message) and N messages.
        """
        self.startQmf()
        session = self.session
        "Set up purge queue"
        session.queue_declare(queue="purge-queue", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="purge-queue", exchange="amq.direct", binding_key="routing_key")

        twenty = range(1,21)
        props = session.delivery_properties(routing_key="routing_key")
        for count in twenty:
            body = "Purge Message %d" % count
            msg = Message(props, body)
            session.message_transfer(destination="amq.direct", message=msg)

        pq = self.qmf.getObjects(_class="queue", name="purge-queue")[0]

        "Purge top message from purge-queue"
        result = pq.purge(1)
        self.assertEqual (result.status, 0) 
        pq = self.qmf.getObjects(_class="queue", name="purge-queue")[0]
        self.assertEqual (pq.msgDepth,19)

        "Purge top 9 messages from purge-queue"
        result = pq.purge(9)
        self.assertEqual (result.status, 0) 
        pq = self.qmf.getObjects(_class="queue", name="purge-queue")[0]
        self.assertEqual (pq.msgDepth,10)

        "Purge all messages from purge-queue"
        result = pq.purge(0)
        self.assertEqual (result.status, 0) 
        pq = self.qmf.getObjects(_class="queue", name="purge-queue")[0]
        self.assertEqual (pq.msgDepth,0)

    def test_methods_async (self):
        """
        """
        class Handler (qmf.console.Console):
            def __init__(self):
                self.cv = Condition()
                self.xmtList = {}
                self.rcvList = {}

            def methodResponse(self, broker, seq, response):
                self.cv.acquire()
                try:
                    self.rcvList[seq] = response
                finally:
                    self.cv.release()

            def request(self, broker, count):
                self.count = count
                for idx in range(count):
                    self.cv.acquire()
                    try:
                        seq = broker.echo(idx, "Echo Message", _async = True)
                        self.xmtList[seq] = idx
                    finally:
                        self.cv.release()

            def check(self):
                if self.count != len(self.xmtList):
                    return "fail (attempted send=%d, actual sent=%d)" % (self.count, len(self.xmtList))
                lost = 0
                mismatched = 0
                for seq in self.xmtList:
                    value = self.xmtList[seq]
                    if seq in self.rcvList:
                        result = self.rcvList.pop(seq)
                        if result.sequence != value:
                            mismatched += 1
                    else:
                        lost += 1
                spurious = len(self.rcvList)
                if lost == 0 and mismatched == 0 and spurious == 0:
                    return "pass"
                else:
                    return "fail (lost=%d, mismatch=%d, spurious=%d)" % (lost, mismatched, spurious)

        handler = Handler()
        self.startQmf(handler)
        brokers = self.qmf.getObjects(_class="broker")
        self.assertEqual(len(brokers), 1)
        broker = brokers[0]
        handler.request(broker, 20)
        sleep(1)
        self.assertEqual(handler.check(), "pass")

    def test_connection_close(self):
        """
        Test management method for closing connection
        """
        self.startQmf()
        conn = self.connect()
        session = conn.session("my-named-session")

        #using qmf find named session and close the corresponding connection:
        qmf_ssn_object = self.qmf.getObjects(_class="session", name="my-named-session")[0]
        qmf_ssn_object._connectionRef_.close()

        #check that connection is closed
        try:
            conn.session("another-session")
            self.fail("Expected failure from closed connection")
        except: None
        
        #make sure that the named session has been closed and the name can be re-used
        conn = self.connect()
        session = conn.session("my-named-session")
        session.queue_declare(queue="whatever", exclusive=True, auto_delete=True)

    def test_binding_count_on_queue(self):
        self.startQmf()
        conn = self.connect()
        session = self.session

        QUEUE = "binding_test_queue"
        EX_DIR = "binding_test_exchange_direct"
        EX_FAN = "binding_test_exchange_fanout"
        EX_TOPIC = "binding_test_exchange_topic"
        EX_HDR = "binding_test_exchange_headers"

        #
        # Create a test queue
        #
        session.queue_declare(queue=QUEUE, exclusive=True, auto_delete=True)
        queue = self.qmf.getObjects(_class="queue", name=QUEUE)[0]
        if not queue:
            self.fail("Queue not found")
        self.assertEqual(queue.bindingCount, 1, "wrong initial binding count")

        #
        # Create an exchange of each supported type
        #
        session.exchange_declare(exchange=EX_DIR, type="direct")
        session.exchange_declare(exchange=EX_FAN, type="fanout")
        session.exchange_declare(exchange=EX_TOPIC, type="topic")
        session.exchange_declare(exchange=EX_HDR, type="headers")

        #
        # Bind each exchange to the test queue
        #
        match = {}
        match['x-match'] = "all"
        match['key'] = "value"
        session.exchange_bind(exchange=EX_DIR, queue=QUEUE, binding_key="key1")
        session.exchange_bind(exchange=EX_DIR, queue=QUEUE, binding_key="key2")
        session.exchange_bind(exchange=EX_FAN, queue=QUEUE)
        session.exchange_bind(exchange=EX_TOPIC, queue=QUEUE, binding_key="key1.#")
        session.exchange_bind(exchange=EX_TOPIC, queue=QUEUE, binding_key="key2.#")
        session.exchange_bind(exchange=EX_HDR, queue=QUEUE, binding_key="key1", arguments=match)
        match['key2'] = "value2"
        session.exchange_bind(exchange=EX_HDR, queue=QUEUE, binding_key="key2", arguments=match)

        #
        # Verify that the queue's binding count accounts for the new bindings
        #
        queue.update()
        self.assertEqual(queue.bindingCount, 8,
                         "added bindings not accounted for (expected 8, got %d)" % queue.bindingCount)

        #
        # Remove some of the bindings
        #
        session.exchange_unbind(exchange=EX_DIR, queue=QUEUE, binding_key="key2")
        session.exchange_unbind(exchange=EX_TOPIC, queue=QUEUE, binding_key="key2.#")
        session.exchange_unbind(exchange=EX_HDR, queue=QUEUE, binding_key="key2")

        #
        # Verify that the queue's binding count accounts for the deleted bindings
        #
        queue.update()
        self.assertEqual(queue.bindingCount, 5,
                         "deleted bindings not accounted for (expected 5, got %d)" % queue.bindingCount)
        #
        # Delete the exchanges
        #
        session.exchange_delete(exchange=EX_DIR)
        session.exchange_delete(exchange=EX_FAN)
        session.exchange_delete(exchange=EX_TOPIC)
        session.exchange_delete(exchange=EX_HDR)

        #
        # Verify that the queue's binding count accounts for the lost bindings
        #
        queue.update()
        self.assertEqual(queue.bindingCount, 1,
                         "deleted bindings not accounted for (expected 1, got %d)" % queue.bindingCount)

