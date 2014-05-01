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

from threading import *
from unittest import TestCase
from qpid.util import connect, listen
from qpid.connection import *
from qpid.datatypes import Message
from qpid.delegates import Server
from qpid.queue import Queue
from qpid.session import Delegate
from qpid.ops import QueueQueryResult

PORT = 1234

class TestServer:

  def __init__(self, queue):
    self.queue = queue

  def connection(self, connection):
    return Server(connection, delegate=self.session)

  def session(self, session):
    session.auto_sync = False
    return TestSession(session, self.queue)

class TestSession(Delegate):

  def __init__(self, session, queue):
    self.session = session
    self.queue = queue

  def execution_sync(self, es):
    pass

  def queue_query(self, qq):
    return QueueQueryResult(qq.queue)

  def message_transfer(self, cmd):
    if cmd.destination == "echo":
      m = Message(cmd.payload)
      m.headers = cmd.headers
      self.session.message_transfer(cmd.destination, cmd.accept_mode,
                                    cmd.acquire_mode, m)
    elif cmd.destination == "abort":
      self.session.channel.connection.sock.close()
    elif cmd.destination == "heartbeat":
      self.session.channel.connection_heartbeat()
    else:
      self.queue.put(cmd)

class ConnectionTest(TestCase):

  def setUp(self):
    self.queue = Queue()
    self.running = True
    started = Event()

    def run():
      ts = TestServer(self.queue)
      for s in listen("0.0.0.0", PORT, lambda: self.running, lambda: started.set()):
        conn = Connection(s, delegate=ts.connection)
        try:
          conn.start(5)
        except Closed:
          pass

    self.server = Thread(target=run)
    self.server.setDaemon(True)
    self.server.start()

    started.wait(3)
    assert started.isSet()

  def tearDown(self):
    self.running = False
    connect("127.0.0.1", PORT).close()
    self.server.join(3)

  def connect(self, **kwargs):
    return Connection(connect("127.0.0.1", PORT), **kwargs)

  def test(self):
    c = self.connect()
    c.start(10)

    ssn1 = c.session("test1", timeout=10)
    ssn2 = c.session("test2", timeout=10)

    assert ssn1 == c.sessions["test1"]
    assert ssn2 == c.sessions["test2"]
    assert ssn1.channel != None
    assert ssn2.channel != None
    assert ssn1 in c.attached.values()
    assert ssn2 in c.attached.values()

    ssn1.close(5)

    assert ssn1.channel == None
    assert ssn1 not in c.attached.values()
    assert ssn2 in c.sessions.values()

    ssn2.close(5)

    assert ssn2.channel == None
    assert ssn2 not in c.attached.values()
    assert ssn2 not in c.sessions.values()

    ssn = c.session("session", timeout=10)

    assert ssn.channel != None
    assert ssn in c.sessions.values()

    destinations = ("one", "two", "three")

    for d in destinations:
      ssn.message_transfer(d)

    for d in destinations:
      cmd = self.queue.get(10)
      assert cmd.destination == d
      assert cmd.headers == None
      assert cmd.payload == None

    msg = Message("this is a test")
    ssn.message_transfer("four", message=msg)
    cmd = self.queue.get(10)
    assert cmd.destination == "four"
    assert cmd.headers == None
    assert cmd.payload == msg.body

    qq = ssn.queue_query("asdf")
    assert qq.queue == "asdf"
    c.close(5)

  def testCloseGet(self):
    c = self.connect()
    c.start(10)
    ssn = c.session("test", timeout=10)
    echos = ssn.incoming("echo")

    for i in range(10):
      ssn.message_transfer("echo", message=Message("test%d" % i))

    ssn.auto_sync=False
    ssn.message_transfer("abort")

    for i in range(10):
      m = echos.get(timeout=10)
      assert m.body == "test%d" % i

    try:
      m = echos.get(timeout=10)
      assert False
    except Closed, e:
      pass

  def testCloseListen(self):
    c = self.connect()
    c.start(10)
    ssn = c.session("test", timeout=10)
    echos = ssn.incoming("echo")

    messages = []
    exceptions = []
    condition = Condition()
    def listener(m): messages.append(m)
    def exc_listener(e):
      exceptions.append(e)
      condition.acquire()
      condition.notify()
      condition.release()

    echos.listen(listener, exc_listener)

    for i in range(10):
      ssn.message_transfer("echo", message=Message("test%d" % i))

    ssn.auto_sync=False
    ssn.message_transfer("abort")

    condition.acquire()
    condition.wait(10)
    condition.release()

    for i in range(10):
      m = messages.pop(0)
      assert m.body == "test%d" % i

    assert len(exceptions) == 1

  def testSync(self):
    c = self.connect()
    c.start(10)
    s = c.session("test")
    s.auto_sync = False
    s.message_transfer("echo", message=Message("test"))
    s.sync(10)

  def testHeartbeat(self):
    c = self.connect(heartbeat=10)
    c.start(10)
    s = c.session("test")
    s.channel.connection_heartbeat()
    s.message_transfer("heartbeat")
