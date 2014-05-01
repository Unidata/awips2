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

# setup, usage, teardown, errors(sync), errors(async), stress, soak,
# boundary-conditions, config

import time
from qpid import compat
from qpid.tests import Test
from qpid.harness import Skipped
from qpid.messaging import Connection, ConnectError, Disconnected, Empty, \
    InsufficientCapacity, Message, ReceiveError, SendError, SessionError, \
    UNLIMITED, uuid4
from Queue import Queue, Empty as QueueEmpty

class Base(Test):

  def setup_connection(self):
    return None

  def setup_session(self):
    return None

  def setup_sender(self):
    return None

  def setup_receiver(self):
    return None

  def setup(self):
    self.test_id = uuid4()
    self.broker = self.config.broker
    try:
      self.conn = self.setup_connection()
    except ConnectError, e:
      raise Skipped(e)
    self.ssn = self.setup_session()
    self.snd = self.setup_sender()
    if self.snd is not None:
      self.snd.durable = self.durable()
    self.rcv = self.setup_receiver()

  def teardown(self):
    if self.conn is not None and self.conn.connected():
      self.conn.close()

  def content(self, base, count = None):
    if count is None:
      return "%s[%s]" % (base, self.test_id)
    else:
      return "%s[%s, %s]" % (base, count, self.test_id)

  def ping(self, ssn):
    PING_Q = 'ping-queue; {create: always, delete: always}'
    # send a message
    sender = ssn.sender(PING_Q, durable=self.durable())
    content = self.content("ping")
    sender.send(content)
    receiver = ssn.receiver(PING_Q)
    msg = receiver.fetch(0)
    ssn.acknowledge()
    assert msg.content == content, "expected %r, got %r" % (content, msg.content)

  def drain(self, rcv, limit=None, timeout=0, expected=None):
    contents = []
    try:
      while limit is None or len(contents) < limit:
        contents.append(rcv.fetch(timeout=timeout).content)
    except Empty:
      pass
    if expected is not None:
      assert expected == contents, "expected %s, got %s" % (expected, contents)
    return contents

  def assertEmpty(self, rcv):
    contents = self.drain(rcv)
    assert len(contents) == 0, "%s is supposed to be empty: %s" % (rcv, contents)

  def assertPending(self, rcv, expected):
    p = rcv.pending()
    assert p == expected, "expected %s, got %s" % (expected, p)

  def sleep(self):
    time.sleep(self.delay())

  def delay(self):
    return float(self.config.defines.get("delay", "2"))

  def get_bool(self, name):
    return self.config.defines.get(name, "false").lower() in ("true", "yes", "1")

  def durable(self):
    return self.get_bool("durable")

  def reconnect(self):
    return self.get_bool("reconnect")

class SetupTests(Base):

  def testOpen(self):
    # XXX: need to flesh out URL support/syntax
    self.conn = Connection.open(self.broker.host, self.broker.port,
                                reconnect=self.reconnect())
    self.ping(self.conn.session())

  def testConnect(self):
    # XXX: need to flesh out URL support/syntax
    self.conn = Connection(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())
    self.conn.connect()
    self.ping(self.conn.session())

  def testConnectError(self):
    try:
      self.conn = Connection.open("localhost", 0)
      assert False, "connect succeeded"
    except ConnectError, e:
      # XXX: should verify that e includes appropriate diagnostic info
      pass

class ConnectionTests(Base):

  def setup_connection(self):
    return Connection.open(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())

  def testSessionAnon(self):
    ssn1 = self.conn.session()
    ssn2 = self.conn.session()
    self.ping(ssn1)
    self.ping(ssn2)
    assert ssn1 is not ssn2

  def testSessionNamed(self):
    ssn1 = self.conn.session("one")
    ssn2 = self.conn.session("two")
    self.ping(ssn1)
    self.ping(ssn2)
    assert ssn1 is not ssn2
    assert ssn1 is self.conn.session("one")
    assert ssn2 is self.conn.session("two")

  def testDisconnect(self):
    ssn = self.conn.session()
    self.ping(ssn)
    self.conn.disconnect()
    try:
      self.ping(ssn)
      assert False, "ping succeeded"
    except Disconnected:
      # this is the expected failure when pinging on a disconnected
      # connection
      pass
    self.conn.connect()
    self.ping(ssn)

  def testClose(self):
    self.conn.close()
    assert not self.conn.connected()

ACK_QC = 'test-ack-queue; {create: always}'
ACK_QD = 'test-ack-queue; {delete: always}'

class SessionTests(Base):

  def setup_connection(self):
    return Connection.open(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())

  def setup_session(self):
    return self.conn.session()

  def testSender(self):
    snd = self.ssn.sender('test-snd-queue; {create: sender, delete: receiver}',
                          durable=self.durable())
    snd2 = self.ssn.sender(snd.target, durable=self.durable())
    assert snd is not snd2
    snd2.close()

    content = self.content("testSender")
    snd.send(content)
    rcv = self.ssn.receiver(snd.target)
    msg = rcv.fetch(0)
    assert msg.content == content
    self.ssn.acknowledge(msg)

  def testReceiver(self):
    rcv = self.ssn.receiver('test-rcv-queue; {create: always}')
    rcv2 = self.ssn.receiver(rcv.source)
    assert rcv is not rcv2
    rcv2.close()

    content = self.content("testReceiver")
    snd = self.ssn.sender(rcv.source, durable=self.durable())
    snd.send(content)
    msg = rcv.fetch(0)
    assert msg.content == content
    self.ssn.acknowledge(msg)
    snd2 = self.ssn.receiver('test-rcv-queue; {delete: always}')

  def testNextReceiver(self):
    ADDR = 'test-next-rcv-queue; {create: always, delete: always}'
    rcv1 = self.ssn.receiver(ADDR, capacity=UNLIMITED)
    rcv2 = self.ssn.receiver(ADDR, capacity=UNLIMITED)
    rcv3 = self.ssn.receiver(ADDR, capacity=UNLIMITED)

    snd = self.ssn.sender(ADDR)

    msgs = []
    for i in range(10):
      content = self.content("testNextReceiver", i)
      snd.send(content)
      msgs.append(content)

    fetched = []
    try:
      while True:
        rcv = self.ssn.next_receiver(timeout=self.delay())
        assert rcv in (rcv1, rcv2, rcv3)
        assert rcv.pending() > 0
        fetched.append(rcv.fetch().content)
    except Empty:
      pass
    assert msgs == fetched, "expecting %s, got %s" % (msgs, fetched)
    self.ssn.acknowledge()

  # XXX, we need a convenient way to assert that required queues are
  # empty on setup, and possibly also to drain queues on teardown
  def ackTest(self, acker, ack_capacity=None):
    # send a bunch of messages
    snd = self.ssn.sender(ACK_QC, durable=self.durable())
    contents = [self.content("ackTest", i) for i in range(15)]
    for c in contents:
      snd.send(c)

    # drain the queue, verify the messages are there and then close
    # without acking
    rcv = self.ssn.receiver(ACK_QC)
    self.drain(rcv, expected=contents)
    self.ssn.close()

    # drain the queue again, verify that they are all the messages
    # were requeued, and ack this time before closing
    self.ssn = self.conn.session()
    if ack_capacity is not None:
      self.ssn.ack_capacity = ack_capacity
    rcv = self.ssn.receiver(ACK_QC)
    self.drain(rcv, expected=contents)
    acker(self.ssn)
    self.ssn.close()

    # drain the queue a final time and verify that the messages were
    # dequeued
    self.ssn = self.conn.session()
    rcv = self.ssn.receiver(ACK_QD)
    self.assertEmpty(rcv)

  def testAcknowledge(self):
    self.ackTest(lambda ssn: ssn.acknowledge())

  def testAcknowledgeAsync(self):
    self.ackTest(lambda ssn: ssn.acknowledge(sync=False))

  def testAcknowledgeAsyncAckCap0(self):
    try:
      try:
        self.ackTest(lambda ssn: ssn.acknowledge(sync=False), 0)
        assert False, "acknowledge shouldn't succeed with ack_capacity of zero"
      except InsufficientCapacity:
        pass
    finally:
      self.ssn.ack_capacity = UNLIMITED
      self.drain(self.ssn.receiver(ACK_QD))
      self.ssn.acknowledge()

  def testAcknowledgeAsyncAckCap1(self):
    self.ackTest(lambda ssn: ssn.acknowledge(sync=False), 1)

  def testAcknowledgeAsyncAckCap5(self):
    self.ackTest(lambda ssn: ssn.acknowledge(sync=False), 5)

  def testAcknowledgeAsyncAckCapUNLIMITED(self):
    self.ackTest(lambda ssn: ssn.acknowledge(sync=False), UNLIMITED)

  def send(self, ssn, queue, base, count=1):
    snd = ssn.sender(queue, durable=self.durable())
    contents = []
    for i in range(count):
      c = self.content(base, i)
      snd.send(c)
      contents.append(c)
    snd.close()
    return contents

  def txTest(self, commit):
    TX_Q = 'test-tx-queue; {create: sender, delete: receiver}'
    TX_Q_COPY = 'test-tx-queue-copy; {create: always, delete: always}'
    txssn = self.conn.session(transactional=True)
    contents = self.send(self.ssn, TX_Q, "txTest", 3)
    txrcv = txssn.receiver(TX_Q)
    txsnd = txssn.sender(TX_Q_COPY, durable=self.durable())
    rcv = self.ssn.receiver(txrcv.source)
    copy_rcv = self.ssn.receiver(txsnd.target)
    self.assertEmpty(copy_rcv)
    for i in range(3):
      m = txrcv.fetch(0)
      txsnd.send(m)
      self.assertEmpty(copy_rcv)
    txssn.acknowledge()
    if commit:
      txssn.commit()
      self.assertEmpty(rcv)
      assert contents == self.drain(copy_rcv)
    else:
      txssn.rollback()
      assert contents == self.drain(rcv)
      self.assertEmpty(copy_rcv)
    self.ssn.acknowledge()

  def testCommit(self):
    self.txTest(True)

  def testRollback(self):
    self.txTest(False)

  def txTestSend(self, commit):
    TX_SEND_Q = 'test-tx-send-queue; {create: sender, delete: receiver}'
    txssn = self.conn.session(transactional=True)
    contents = self.send(txssn, TX_SEND_Q, "txTestSend", 3)
    rcv = self.ssn.receiver(TX_SEND_Q)
    self.assertEmpty(rcv)

    if commit:
      txssn.commit()
      assert contents == self.drain(rcv)
      self.ssn.acknowledge()
    else:
      txssn.rollback()
      self.assertEmpty(rcv)
      txssn.commit()
      self.assertEmpty(rcv)

  def testCommitSend(self):
    self.txTestSend(True)

  def testRollbackSend(self):
    self.txTestSend(False)

  def txTestAck(self, commit):
    TX_ACK_QC = 'test-tx-ack-queue; {create: always}'
    TX_ACK_QD = 'test-tx-ack-queue; {delete: always}'
    txssn = self.conn.session(transactional=True)
    txrcv = txssn.receiver(TX_ACK_QC)
    self.assertEmpty(txrcv)
    contents = self.send(self.ssn, TX_ACK_QC, "txTestAck", 3)
    assert contents == self.drain(txrcv)

    if commit:
      txssn.acknowledge()
    else:
      txssn.rollback()
      drained = self.drain(txrcv)
      assert contents == drained, "expected %s, got %s" % (contents, drained)
      txssn.acknowledge()
      txssn.rollback()
      assert contents == self.drain(txrcv)
      txssn.commit() # commit without ack
      self.assertEmpty(txrcv)

    txssn.close()

    txssn = self.conn.session(transactional=True)
    txrcv = txssn.receiver(TX_ACK_QC)
    assert contents == self.drain(txrcv)
    txssn.acknowledge()
    txssn.commit()
    rcv = self.ssn.receiver(TX_ACK_QD)
    self.assertEmpty(rcv)
    txssn.close()
    self.assertEmpty(rcv)

  def testCommitAck(self):
    self.txTestAck(True)

  def testRollbackAck(self):
    self.txTestAck(False)

  def testClose(self):
    self.ssn.close()
    try:
      self.ping(self.ssn)
      assert False, "ping succeeded"
    except Disconnected:
      pass

RECEIVER_Q = 'test-receiver-queue; {create: always, delete: always}'

class ReceiverTests(Base):

  def setup_connection(self):
    return Connection.open(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())

  def setup_session(self):
    return self.conn.session()

  def setup_sender(self):
    return self.ssn.sender(RECEIVER_Q)

  def setup_receiver(self):
    return self.ssn.receiver(RECEIVER_Q)

  def send(self, base, count = None):
    content = self.content(base, count)
    self.snd.send(content)
    return content

  def testFetch(self):
    try:
      msg = self.rcv.fetch(0)
      assert False, "unexpected message: %s" % msg
    except Empty:
      pass
    try:
      start = time.time()
      msg = self.rcv.fetch(self.delay())
      assert False, "unexpected message: %s" % msg
    except Empty:
      elapsed = time.time() - start
      assert elapsed >= self.delay()

    one = self.send("testFetch", 1)
    two = self.send("testFetch", 2)
    three = self.send("testFetch", 3)
    msg = self.rcv.fetch(0)
    assert msg.content == one
    msg = self.rcv.fetch(self.delay())
    assert msg.content == two
    msg = self.rcv.fetch()
    assert msg.content == three
    self.ssn.acknowledge()

  def testCapacityIncrease(self):
    content = self.send("testCapacityIncrease")
    self.sleep()
    assert self.rcv.pending() == 0
    self.rcv.capacity = UNLIMITED
    self.sleep()
    assert self.rcv.pending() == 1
    msg = self.rcv.fetch(0)
    assert msg.content == content
    assert self.rcv.pending() == 0
    self.ssn.acknowledge()

  def testCapacityDecrease(self):
    self.rcv.capacity = UNLIMITED
    one = self.send("testCapacityDecrease", 1)
    self.sleep()
    assert self.rcv.pending() == 1
    msg = self.rcv.fetch(0)
    assert msg.content == one

    self.rcv.capacity = 0

    two = self.send("testCapacityDecrease", 2)
    self.sleep()
    assert self.rcv.pending() == 0
    msg = self.rcv.fetch(0)
    assert msg.content == two

    self.ssn.acknowledge()

  def testCapacity(self):
    self.rcv.capacity = 5
    self.assertPending(self.rcv, 0)

    for i in range(15):
      self.send("testCapacity", i)
    self.sleep()
    self.assertPending(self.rcv, 5)

    self.drain(self.rcv, limit = 5)
    self.sleep()
    self.assertPending(self.rcv, 5)

    drained = self.drain(self.rcv)
    assert len(drained) == 10, "%s, %s" % (len(drained), drained)
    self.assertPending(self.rcv, 0)

    self.ssn.acknowledge()

  def testCapacityUNLIMITED(self):
    self.rcv.capacity = UNLIMITED
    self.assertPending(self.rcv, 0)

    for i in range(10):
      self.send("testCapacityUNLIMITED", i)
    self.sleep()
    self.assertPending(self.rcv, 10)

    self.drain(self.rcv)
    self.assertPending(self.rcv, 0)

    self.ssn.acknowledge()

  def testPending(self):
    self.rcv.capacity = UNLIMITED
    assert self.rcv.pending() == 0

    for i in range(3):
      self.send("testPending", i)
    self.sleep()
    assert self.rcv.pending() == 3

    for i in range(3, 10):
      self.send("testPending", i)
    self.sleep()
    assert self.rcv.pending() == 10

    self.drain(self.rcv, limit=3)
    assert self.rcv.pending() == 7

    self.drain(self.rcv)
    assert self.rcv.pending() == 0

    self.ssn.acknowledge()

  # XXX: need testClose

class AddressTests(Base):

  def setup_connection(self):
    return Connection.open(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())

  def setup_session(self):
    return self.conn.session()

  def testBadOption(self):
    snd = self.ssn.sender("test-bad-option; {create: always, node-properties: {this-property-does-not-exist: 3}}")
    try:
      snd.send("ping")
    except SendError, e:
      assert "unrecognized option" in str(e)

  def testCreateQueue(self):
    snd = self.ssn.sender("test-create-queue; {create: always, delete: always, "
                          "node-properties: {type: queue, durable: False, "
                          "x-properties: {auto_delete: true}}}")
    content = self.content("testCreateQueue")
    snd.send(content)
    rcv = self.ssn.receiver("test-create-queue")
    self.drain(rcv, expected=[content])

  def createExchangeTest(self, props=""):
    addr = """test-create-exchange; {
                create: always,
                delete: always,
                node-properties: {
                  type: topic,
                  durable: False,
                  x-properties: {auto_delete: true, %s}
                }
              }""" % props
    snd = self.ssn.sender(addr)
    snd.send("ping")
    rcv1 = self.ssn.receiver("test-create-exchange/first")
    rcv2 = self.ssn.receiver("test-create-exchange/first")
    rcv3 = self.ssn.receiver("test-create-exchange/second")
    for r in (rcv1, rcv2, rcv3):
      try:
        r.fetch(0)
        assert False
      except Empty:
        pass
    msg1 = Message(self.content("testCreateExchange", 1), subject="first")
    msg2 = Message(self.content("testCreateExchange", 2), subject="second")
    snd.send(msg1)
    snd.send(msg2)
    self.drain(rcv1, expected=[msg1.content])
    self.drain(rcv2, expected=[msg1.content])
    self.drain(rcv3, expected=[msg2.content])

  def testCreateExchange(self):
    self.createExchangeTest()

  def testCreateExchangeDirect(self):
    self.createExchangeTest("type: direct")

  def testCreateExchangeTopic(self):
    self.createExchangeTest("type: topic")

  def testDeleteBySender(self):
    snd = self.ssn.sender("test-delete; {create: always}")
    snd.send("ping")
    snd.close()
    snd = self.ssn.sender("test-delete; {delete: always}")
    snd.send("ping")
    snd.close()
    snd = self.ssn.sender("test-delete")
    try:
      snd.send("ping")
    except SendError, e:
      assert "no such queue" in str(e)

  def testDeleteByReceiver(self):
    rcv = self.ssn.receiver("test-delete; {create: always, delete: always}")
    try:
      rcv.fetch(0)
    except Empty:
      pass
    rcv.close()

    try:
      self.ssn.receiver("test-delete")
    except SendError, e:
      assert "no such queue" in str(e)

  def testDeleteSpecial(self):
    snd = self.ssn.sender("amq.topic; {delete: always}")
    snd.send("asdf")
    try:
      snd.close()
    except SessionError, e:
      assert "Cannot delete default exchange" in str(e)
    # XXX: need to figure out close after error
    self.conn._remove_session(self.ssn)

  def testBindings(self):
    snd = self.ssn.sender("""
test-bindings-queue; {
  create: always,
  delete: always,
  node-properties: {
    x-properties: {
      bindings: ["amq.topic/a.#", "amq.direct/b", "amq.topic/c.*"]
    }
  }
}
""")
    snd.send("one")
    snd_a = self.ssn.sender("amq.topic/a.foo")
    snd_b = self.ssn.sender("amq.direct/b")
    snd_c = self.ssn.sender("amq.topic/c.bar")
    snd_a.send("two")
    snd_b.send("three")
    snd_c.send("four")
    rcv = self.ssn.receiver("test-bindings-queue")
    self.drain(rcv, expected=["one", "two", "three", "four"])

NOSUCH_Q = "this-queue-should-not-exist"
UNPARSEABLE_ADDR = "name/subject; {bad options"
UNLEXABLE_ADDR = "\0x0\0x1\0x2\0x3"

class AddressErrorTests(Base):

  def setup_connection(self):
    return Connection.open(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())

  def setup_session(self):
    return self.conn.session()

  def sendErrorTest(self, addr, exc, check=lambda e: True):
    snd = self.ssn.sender(addr, durable=self.durable())
    try:
      snd.send("hello")
      assert False, "send succeeded"
    except exc, e:
      assert check(e), "unexpected error: %s" % compat.format_exc(e)
      snd.close()

  def fetchErrorTest(self, addr, exc, check=lambda e: True):
    rcv = self.ssn.receiver(addr)
    try:
      rcv.fetch(timeout=0)
      assert False, "fetch succeeded"
    except exc, e:
      assert check(e), "unexpected error: %s" % compat.format_exc(e)
      rcv.close()

  def testNoneTarget(self):
    # XXX: should have specific exception for this
    self.sendErrorTest(None, SendError)

  def testNoneSource(self):
    # XXX: should have specific exception for this
    self.fetchErrorTest(None, ReceiveError)

  def testNoTarget(self):
    # XXX: should have specific exception for this
    self.sendErrorTest(NOSUCH_Q, SendError, lambda e: NOSUCH_Q in str(e))

  def testNoSource(self):
    # XXX: should have specific exception for this
    self.fetchErrorTest(NOSUCH_Q, ReceiveError, lambda e: NOSUCH_Q in str(e))

  def testUnparseableTarget(self):
    # XXX: should have specific exception for this
    self.sendErrorTest(UNPARSEABLE_ADDR, SendError,
                       lambda e: "expecting COLON" in str(e))

  def testUnparseableSource(self):
    # XXX: should have specific exception for this
    self.fetchErrorTest(UNPARSEABLE_ADDR, ReceiveError,
                        lambda e: "expecting COLON" in str(e))

  def testUnlexableTarget(self):
    # XXX: should have specific exception for this
    self.sendErrorTest(UNLEXABLE_ADDR, SendError,
                       lambda e: "unrecognized characters" in str(e))

  def testUnlexableSource(self):
    # XXX: should have specific exception for this
    self.fetchErrorTest(UNLEXABLE_ADDR, ReceiveError,
                        lambda e: "unrecognized characters" in str(e))

SENDER_Q = 'test-sender-q; {create: always, delete: always}'

class SenderTests(Base):

  def setup_connection(self):
    return Connection.open(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())

  def setup_session(self):
    return self.conn.session()

  def setup_sender(self):
    return self.ssn.sender(SENDER_Q)

  def setup_receiver(self):
    return self.ssn.receiver(SENDER_Q)

  def checkContent(self, content):
    self.snd.send(content)
    msg = self.rcv.fetch(0)
    assert msg.content == content

    out = Message(content)
    self.snd.send(out)
    echo = self.rcv.fetch(0)
    assert out.content == echo.content
    assert echo.content == msg.content
    self.ssn.acknowledge()

  def testSendString(self):
    self.checkContent(self.content("testSendString"))

  def testSendList(self):
    self.checkContent(["testSendList", 1, 3.14, self.test_id])

  def testSendMap(self):
    self.checkContent({"testSendMap": self.test_id, "pie": "blueberry", "pi": 3.14})

  def asyncTest(self, capacity):
    self.snd.capacity = capacity
    msgs = [self.content("asyncTest", i) for i in range(15)]
    for m in msgs:
      self.snd.send(m, sync=False)
    drained = self.drain(self.rcv, timeout=self.delay())
    assert msgs == drained, "expected %s, got %s" % (msgs, drained)
    self.ssn.acknowledge()

  def testSendAsyncCapacity0(self):
    try:
      self.asyncTest(0)
      assert False, "send shouldn't succeed with zero capacity"
    except InsufficientCapacity:
      # this is expected
      pass

  def testSendAsyncCapacity1(self):
    self.asyncTest(1)

  def testSendAsyncCapacity5(self):
    self.asyncTest(5)

  def testSendAsyncCapacityUNLIMITED(self):
    self.asyncTest(UNLIMITED)

  def testCapacityTimeout(self):
    self.snd.capacity = 1
    msgs = []
    caught = False
    while len(msgs) < 100:
      m = self.content("testCapacity", len(msgs))
      try:
        self.snd.send(m, sync=False, timeout=0)
        msgs.append(m)
      except InsufficientCapacity:
        caught = True
        break
    self.snd.sync()
    self.drain(self.rcv, expected=msgs)
    self.ssn.acknowledge()
    assert caught, "did not exceed capacity"

class MessageTests(Base):

  def testCreateString(self):
    m = Message("string")
    assert m.content == "string"
    assert m.content_type is None

  def testCreateUnicode(self):
    m = Message(u"unicode")
    assert m.content == u"unicode"
    assert m.content_type == "text/plain"

  def testCreateMap(self):
    m = Message({})
    assert m.content == {}
    assert m.content_type == "amqp/map"

  def testCreateList(self):
    m = Message([])
    assert m.content == []
    assert m.content_type == "amqp/list"

  def testContentTypeOverride(self):
    m = Message()
    m.content_type = "text/html; charset=utf8"
    m.content = u"<html/>"
    assert m.content_type == "text/html; charset=utf8"

ECHO_Q = 'test-message-echo-queue; {create: always, delete: always}'

class MessageEchoTests(Base):

  def setup_connection(self):
    return Connection.open(self.broker.host, self.broker.port,
                           reconnect=self.reconnect())

  def setup_session(self):
    return self.conn.session()

  def setup_sender(self):
    return self.ssn.sender(ECHO_Q)

  def setup_receiver(self):
    return self.ssn.receiver(ECHO_Q)

  def check(self, msg):
    self.snd.send(msg)
    echo = self.rcv.fetch(0)

    assert msg.id == echo.id
    assert msg.subject == echo.subject
    assert msg.user_id == echo.user_id
    assert msg.to == echo.to
    assert msg.reply_to == echo.reply_to
    assert msg.correlation_id == echo.correlation_id
    assert msg.properties == echo.properties
    assert msg.content_type == echo.content_type
    assert msg.content == echo.content, "%s, %s" % (msg, echo)

    self.ssn.acknowledge(echo)

  def testStringContent(self):
    self.check(Message("string"))

  def testUnicodeContent(self):
    self.check(Message(u"unicode"))


  TEST_MAP = {"key1": "string",
              "key2": u"unicode",
              "key3": 3,
              "key4": -3,
              "key5": 3.14,
              "key6": -3.14,
              "key7": ["one", 2, 3.14],
              "key8": [],
              "key9": {"sub-key0": 3}}

  def testMapContent(self):
    self.check(Message(MessageEchoTests.TEST_MAP))

  def testListContent(self):
    self.check(Message([]))
    self.check(Message([1, 2, 3]))
    self.check(Message(["one", 2, 3.14, {"four": 4}]))

  def testProperties(self):
    msg = Message()
    msg.to = "to-address"
    msg.subject = "subject"
    msg.correlation_id = str(self.test_id)
    msg.properties = MessageEchoTests.TEST_MAP
    msg.reply_to = "reply-address"
    self.check(msg)

class TestTestsXXX(Test):

  def testFoo(self):
    print "this test has output"

  def testBar(self):
    print "this test "*8
    print "has"*10
    print "a"*75
    print "lot of"*10
    print "output"*10

  def testQux(self):
    import sys
    sys.stdout.write("this test has output with no newline")

  def testQuxFail(self):
    import sys
    sys.stdout.write("this test has output with no newline")
    fdsa
