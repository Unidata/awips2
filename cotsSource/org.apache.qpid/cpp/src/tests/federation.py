#!/usr/bin/env python
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

import sys
from qpid.testlib import TestBase010
from qpid.datatypes import Message
from qpid.queue import Empty
from time import sleep

class FederationTests(TestBase010):

    def remote_host(self):
        return self.defines.get("remote-host", "localhost")

    def remote_port(self):
        return int(self.defines["remote-port"])

    def verify_cleanup(self):
        attempts = 0
        total = len(self.qmf.getObjects(_class="bridge")) + len(self.qmf.getObjects(_class="link"))
        while total > 0:
            attempts += 1
            if attempts >= 10:
                self.fail("Bridges and links didn't clean up")
                return
            sleep(1)
            total = len(self.qmf.getObjects(_class="bridge")) + len(self.qmf.getObjects(_class="link"))

    def test_bridge_create_and_close(self):
        self.startQmf();
        qmf = self.qmf

        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "amq.direct", "amq.direct", "my-key", "", "", False, False, False, 0)
        self.assertEqual(result.status, 0)

        bridge = qmf.getObjects(_class="bridge")[0]
        result = bridge.close()
        self.assertEqual(result.status, 0)

        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def test_pull_from_exchange(self):
        session = self.session
        
        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "amq.direct", "amq.fanout", "my-key", "", "", False, False, False, 0)
        self.assertEqual(result.status, 0)

        bridge = qmf.getObjects(_class="bridge")[0]

        #setup queue to receive messages from local broker
        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="amq.fanout")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")
        sleep(6)

        #send messages to remote broker and confirm it is routed to local broker
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_pull_from_exchange")

        for i in range(1, 11):
            dp = r_session.delivery_properties(routing_key="my-key")
            r_session.message_transfer(destination="amq.direct", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            msg = queue.get(timeout=5)
            self.assertEqual("Message %d" % i, msg.body)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def test_push_to_exchange(self):
        session = self.session
        
        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "amq.direct", "amq.fanout", "my-key", "", "", False, True, False, 0)
        self.assertEqual(result.status, 0)

        bridge = qmf.getObjects(_class="bridge")[0]

        #setup queue to receive messages from remote broker
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_push_to_exchange")
        r_session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        r_session.exchange_bind(queue="fed1", exchange="amq.fanout")
        self.subscribe(session=r_session, queue="fed1", destination="f1")
        queue = r_session.incoming("f1")
        sleep(6)

        #send messages to local broker and confirm it is routed to remote broker
        for i in range(1, 11):
            dp = session.delivery_properties(routing_key="my-key")
            session.message_transfer(destination="amq.direct", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            msg = queue.get(timeout=5)
            self.assertEqual("Message %d" % i, msg.body)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def test_pull_from_queue(self):
        session = self.session

        #setup queue on remote broker and add some messages
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_pull_from_queue")
        r_session.queue_declare(queue="my-bridge-queue", auto_delete=True)
        for i in range(1, 6):
            dp = r_session.delivery_properties(routing_key="my-bridge-queue")
            r_session.message_transfer(message=Message(dp, "Message %d" % i))

        #setup queue to receive messages from local broker
        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="amq.fanout")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")

        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "my-bridge-queue", "amq.fanout", "my-key", "", "", True, False, False, 1)
        self.assertEqual(result.status, 0)

        bridge = qmf.getObjects(_class="bridge")[0]
        sleep(3)

        #add some more messages (i.e. after bridge was created)
        for i in range(6, 11):
            dp = r_session.delivery_properties(routing_key="my-bridge-queue")
            r_session.message_transfer(message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            try:
                msg = queue.get(timeout=5)
                self.assertEqual("Message %d" % i, msg.body)
            except Empty:
                self.fail("Failed to find expected message containing 'Message %d'" % i)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def test_tracing_automatic(self):
        remoteUrl = "%s:%d" % (self.remote_host(), self.remote_port())
        self.startQmf()
        l_broker = self.qmf_broker
        r_broker = self.qmf.addBroker(remoteUrl)

        l_brokerObj = self.qmf.getObjects(_class="broker", _broker=l_broker)[0]
        r_brokerObj = self.qmf.getObjects(_class="broker", _broker=r_broker)[0]

        l_res = l_brokerObj.connect(self.remote_host(), self.remote_port(),     False, "PLAIN", "guest", "guest", "tcp")
        r_res = r_brokerObj.connect(self.broker.host, self.broker.port, False, "PLAIN", "guest", "guest", "tcp")

        self.assertEqual(l_res.status, 0)
        self.assertEqual(r_res.status, 0)

        l_link = self.qmf.getObjects(_class="link", _broker=l_broker)[0]
        r_link = self.qmf.getObjects(_class="link", _broker=r_broker)[0]

        l_res = l_link.bridge(False, "amq.direct", "amq.direct", "key", "", "", False, False, False, 0)
        r_res = r_link.bridge(False, "amq.direct", "amq.direct", "key", "", "", False, False, False, 0)

        self.assertEqual(l_res.status, 0)
        self.assertEqual(r_res.status, 0)

        count = 0
        while l_link.state != "Operational" or r_link.state != "Operational":
            count += 1
            if count > 10:
                self.fail("Fed links didn't become operational after 10 seconds")
            sleep(1)
            l_link = self.qmf.getObjects(_class="link", _broker=l_broker)[0]
            r_link = self.qmf.getObjects(_class="link", _broker=r_broker)[0]
        sleep(3)

        #setup queue to receive messages from local broker
        session = self.session
        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="amq.direct", binding_key="key")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")

        #setup queue on remote broker and add some messages
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_trace")
        for i in range(1, 11):
            dp = r_session.delivery_properties(routing_key="key")
            r_session.message_transfer(destination="amq.direct", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            try:
                msg = queue.get(timeout=5)
                self.assertEqual("Message %d" % i, msg.body)
            except Empty:
                self.fail("Failed to find expected message containing 'Message %d'" % i)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

    def test_tracing(self):
        session = self.session
        
        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "amq.direct", "amq.fanout", "my-key", "my-bridge-id",
                             "exclude-me,also-exclude-me", False, False, False, 0)
        self.assertEqual(result.status, 0)
        bridge = qmf.getObjects(_class="bridge")[0]

        #setup queue to receive messages from local broker
        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="amq.fanout")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")
        sleep(6)

        #send messages to remote broker and confirm it is routed to local broker
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_tracing")

        trace = [None, "exclude-me", "a,exclude-me,b", "also-exclude-me,c", "dont-exclude-me"]
        body = ["yes", "first-bad", "second-bad", "third-bad", "yes"]
        for b, t in zip(body, trace):
            headers = {}
            if (t): headers["x-qpid.trace"]=t
            dp = r_session.delivery_properties(routing_key="my-key")
            mp = r_session.message_properties(application_headers=headers)
            r_session.message_transfer(destination="amq.direct", message=Message(dp, mp, b))

        for e in ["my-bridge-id", "dont-exclude-me,my-bridge-id"]:
            msg = queue.get(timeout=5)
            self.assertEqual("yes", msg.body)
            self.assertEqual(e, self.getAppHeader(msg, "x-qpid.trace"))

        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def test_dynamic_fanout(self):
        session = self.session
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_dynamic_fanout")

        session.exchange_declare(exchange="fed.fanout", type="fanout")
        r_session.exchange_declare(exchange="fed.fanout", type="fanout")

        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "fed.fanout", "fed.fanout", "", "", "", False, False, True, 0)
        self.assertEqual(result.status, 0)
        bridge = qmf.getObjects(_class="bridge")[0]
        sleep(5)

        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="fed.fanout")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")

        for i in range(1, 11):
            dp = r_session.delivery_properties()
            r_session.message_transfer(destination="fed.fanout", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            msg = queue.get(timeout=5)
            self.assertEqual("Message %d" % i, msg.body)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()


    def test_dynamic_direct(self):
        session = self.session
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_dynamic_direct")

        session.exchange_declare(exchange="fed.direct", type="direct")
        r_session.exchange_declare(exchange="fed.direct", type="direct")

        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "fed.direct", "fed.direct", "", "", "", False, False, True, 0)
        self.assertEqual(result.status, 0)
        bridge = qmf.getObjects(_class="bridge")[0]
        sleep(5)

        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="fed.direct", binding_key="fd-key")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")

        for i in range(1, 11):
            dp = r_session.delivery_properties(routing_key="fd-key")
            r_session.message_transfer(destination="fed.direct", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            msg = queue.get(timeout=5)
            self.assertEqual("Message %d" % i, msg.body)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def test_dynamic_topic(self):
        session = self.session
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_dynamic_topic")

        session.exchange_declare(exchange="fed.topic", type="topic")
        r_session.exchange_declare(exchange="fed.topic", type="topic")

        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "fed.topic", "fed.topic", "", "", "", False, False, True, 0)
        self.assertEqual(result.status, 0)
        bridge = qmf.getObjects(_class="bridge")[0]
        sleep(5)

        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="fed.topic", binding_key="ft-key.#")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")

        for i in range(1, 11):
            dp = r_session.delivery_properties(routing_key="ft-key.one.two")
            r_session.message_transfer(destination="fed.topic", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            msg = queue.get(timeout=5)
            self.assertEqual("Message %d" % i, msg.body)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def test_dynamic_topic_reorigin(self):
        session = self.session
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_dynamic_topic_reorigin")

        session.exchange_declare(exchange="fed.topic_reorigin", type="topic")
        r_session.exchange_declare(exchange="fed.topic_reorigin", type="topic")

        session.exchange_declare(exchange="fed.topic_reorigin_2", type="topic")
        r_session.exchange_declare(exchange="fed.topic_reorigin_2", type="topic")

        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        session.queue_declare(queue="fed2", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed2", exchange="fed.topic_reorigin_2", binding_key="ft-key.one.#")
        self.subscribe(queue="fed2", destination="f2")
        queue2 = session.incoming("f2")

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "fed.topic_reorigin", "fed.topic_reorigin", "", "", "", False, False, True, 0)
        self.assertEqual(result.status, 0)
        result = link.bridge(False, "fed.topic_reorigin_2", "fed.topic_reorigin_2", "", "", "", False, False, True, 0)
        self.assertEqual(result.status, 0)

        bridge = qmf.getObjects(_class="bridge")[0]
        bridge2 = qmf.getObjects(_class="bridge")[1]
        sleep(5)

        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="fed.topic_reorigin", binding_key="ft-key.#")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")

        for i in range(1, 11):
            dp = r_session.delivery_properties(routing_key="ft-key.one.two")
            r_session.message_transfer(destination="fed.topic_reorigin", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            msg = queue.get(timeout=5)
            self.assertEqual("Message %d" % i, msg.body)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        result = bridge2.close()
        self.assertEqual(result.status, 0)

        # extra check: verify we don't leak bridge objects - keep the link
        # around and verify the bridge count has gone to zero

        attempts = 0
        bridgeCount = len(qmf.getObjects(_class="bridge"))
        while bridgeCount > 0:
            attempts += 1
            if attempts >= 5:
                self.fail("Bridges didn't clean up")
                return
            sleep(1)
            bridgeCount = len(qmf.getObjects(_class="bridge"))

        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()
        
    def test_dynamic_direct_reorigin(self):
        session = self.session
        r_conn = self.connect(host=self.remote_host(), port=self.remote_port())
        r_session = r_conn.session("test_dynamic_direct_reorigin")

        session.exchange_declare(exchange="fed.direct_reorigin", type="direct")
        r_session.exchange_declare(exchange="fed.direct_reorigin", type="direct")

        session.exchange_declare(exchange="fed.direct_reorigin_2", type="direct")
        r_session.exchange_declare(exchange="fed.direct_reorigin_2", type="direct")

        self.startQmf()
        qmf = self.qmf
        broker = qmf.getObjects(_class="broker")[0]
        result = broker.connect(self.remote_host(), self.remote_port(), False, "PLAIN", "guest", "guest", "tcp")
        self.assertEqual(result.status, 0)

        session.queue_declare(queue="fed2", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed2", exchange="fed.direct_reorigin_2", binding_key="ft-key.two")
        self.subscribe(queue="fed2", destination="f2")
        queue2 = session.incoming("f2")

        link = qmf.getObjects(_class="link")[0]
        result = link.bridge(False, "fed.direct_reorigin", "fed.direct_reorigin", "", "", "", False, False, True, 0)
        self.assertEqual(result.status, 0)
        result = link.bridge(False, "fed.direct_reorigin_2", "fed.direct_reorigin_2", "", "", "", False, False, True, 0)
        self.assertEqual(result.status, 0)

        bridge = qmf.getObjects(_class="bridge")[0]
        bridge2 = qmf.getObjects(_class="bridge")[1]
        sleep(5)

        session.queue_declare(queue="fed1", exclusive=True, auto_delete=True)
        session.exchange_bind(queue="fed1", exchange="fed.direct_reorigin", binding_key="ft-key.one")
        self.subscribe(queue="fed1", destination="f1")
        queue = session.incoming("f1")

        for i in range(1, 11):
            dp = r_session.delivery_properties(routing_key="ft-key.one")
            r_session.message_transfer(destination="fed.direct_reorigin", message=Message(dp, "Message %d" % i))

        for i in range(1, 11):
            msg = queue.get(timeout=5)
            self.assertEqual("Message %d" % i, msg.body)
        try:
            extra = queue.get(timeout=1)
            self.fail("Got unexpected message in queue: " + extra.body)
        except Empty: None

        result = bridge.close()
        self.assertEqual(result.status, 0)
        
        # Extra test: don't explicitly close() bridge2.  When the link is closed,
        # it should clean up bridge2 automagically.  verify_cleanup() will detect
        # if bridge2 isn't cleaned up and will fail the test.
        #
        #result = bridge2.close()
        #self.assertEqual(result.status, 0)
        result = link.close()
        self.assertEqual(result.status, 0)

        self.verify_cleanup()

    def getProperty(self, msg, name):
        for h in msg.headers:
            if hasattr(h, name): return getattr(h, name)
        return None            

    def getAppHeader(self, msg, name):
        headers = self.getProperty(msg, "application_headers")
        if headers:
            return headers[name]
        return None
