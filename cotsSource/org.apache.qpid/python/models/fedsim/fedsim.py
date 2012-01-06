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

class Sim:
  def __init__(self):
    self.brokers = {}
    self.clients = {}
    self.errors = 0
    self.warnings = 0

  def error(self, text):
    self.errors += 1
    print "###### Error:", text

  def warning(self, text):
    self.warnings += 1
    print "###### Warning:", text

  def end(self):
    print "========================"
    print "Errors: %d, Warnings: %d" % (self.errors, self.warnings)
    print "========================"

  def dumpState(self):
    print "============================"
    print "===== Federation State ====="
    print "============================"
    for broker in self.brokers:
      for exchange in self.brokers[broker].exchanges:
        print "Exchange %s.%s" % (broker, exchange)
        for key in self.brokers[broker].exchanges[exchange].keys:
          print "  Key %s" % key
          for queue in self.brokers[broker].exchanges[exchange].keys[key]:
            print "    Queue %s origins=%s" % \
                (queue.name, self.brokers[broker].exchanges[exchange].keys[key][queue].originList)

  def addBroker(self, name):
    if name in self.brokers:
      raise Exception("Broker of same name already exists")
    broker = Broker(self, name)
    self.brokers[name] = broker
    return broker

  def addClient(self, name, broker):
    if name in self.clients:
      raise Exception("Client of same name already exists")
    client = Client(self, name, broker)
    self.clients[name] = client
    return client

  def link(self, left, right, bidir=True):
    print "====== link %s to %s, bidir=%s" % (left.tag, right.tag, bidir)
    l1 = left.createLink(right)
    l1.bridge("amq.direct")
    if bidir:
      l2 = right.createLink(left)
      l2.bridge("amq.direct")

  def bind(self, client, key):
    print "====== bind Client(%s): k=%s" % (client.name, key)
    client.bind(key)

  def unbind(self, client, key):
    print "====== unbind Client(%s): k=%s" % (client.name, key)
    client.unbind(key)

  def sendMessage(self, key, broker, body="Message Body"):
    print "====== sendMessage: broker=%s k=%s" % (broker.tag, key)
    msg = Message(key, body)
    exchange = broker.exchanges["amq.direct"]
    for client in self.clients:
      self.clients[client].expect(key);
    exchange.receive(key, msg, True)
    for client in self.clients:
      self.clients[client].checkReception()


class Destination:
  def receive(self, key, msg, fromUser=False):
    pass


class Client(Destination):
  def __init__(self, sim, name, broker):
    self.sim = sim
    self.name = name
    self.broker = broker
    self.broker.connect(self)
    self.queue = self.broker.declare_queue(name)
    self.subscription = self.broker.subscribe(self, name)
    self.expected = None
    self.boundKeys = []

  def bind(self, key):
    self.boundKeys.append(key)
    self.broker.bind("amq.direct", self.name, key)

  def unbind(self, key):
    self.boundKeys.remove(key)
    self.broker.unbind("amq.direct", self.name, key)

  def receive(self, key, msg, fromUser=False):
    print "Client(%s) received [%s]: %s" % (self.name, key, msg.body)
    if self.expected == key:
      self.expected = None
    else:
      self.sim.error("Client(%s) received unexpected message with key [%s]" % \
                       (self.name, self.expected))

  def expect(self, key):
    if key in self.boundKeys:
      self.expected = key

  def checkReception(self):
    if self.expected:
      self.sim.error("Client(%s) never received message with key [%s]" % \
                       (self.name, self.expected))

class Broker(Client):
  def __init__(self, sim, tag):
    self.sim = sim
    self.tag = tag
    self.connections = {}
    self.exchanges = {}
    self.queues = {}
    self.subscriptions = {}
    self.links = {}
    self.directExchange = self.declare_exchange("amq.direct")

  def connect(self, client):
    if client in self.connections:
      raise Exception("Client already connected")
    self.connections[client] = Connection(client)

  def declare_queue(self, name, tag=None, exclude=None):
    if name in self.queues:
      raise Exception("Queue already exists")
    self.queues[name] = Queue(self, name, tag, exclude)

  def subscribe(self, dest, queueName):
    if queueName not in self.queues:
      raise Exception("Queue does not exist")
    self.queues[queueName].setDest(dest)

  def declare_exchange(self, name):
    if name in self.exchanges:
      return
    exchange = Exchange(self, name)
    self.exchanges[name] = exchange
    return exchange

  def bind(self, exchangeName, queueName, key, tagList=[], fedOp=None, origin=None):
    if exchangeName not in self.exchanges:
      raise Exception("Exchange not found")
    if queueName not in self.queues:
      raise Exception("Queue not found")
    exchange = self.exchanges[exchangeName]
    queue = self.queues[queueName]
    exchange.bind(queue, key, tagList, fedOp, origin)

  def unbind(self, exchangeName, queueName, key):
    if exchangeName not in self.exchanges:
      raise Exception("Exchange not found")
    if queueName not in self.queues:
      raise Exception("Queue not found")
    exchange = self.exchanges[exchangeName]
    queue = self.queues[queueName]
    exchange.unbind(queue, key)

  def createLink(self, other):
    if other in self.links:
      raise Exception("Peer broker already linked")
    link = Link(self, other)
    self.links[other] = link
    return link


class Connection:
  def __init__(self, client):
    self.client = client


class Exchange(Destination):
  def __init__(self, broker, name):
    self.broker = broker
    self.sim = broker.sim
    self.name = name
    self.keys = {}
    self.bridges = []

  def bind(self, queue, key, tagList, fedOp, origin):
    if not fedOp: fedOp = "bind"
    print "Exchange(%s.%s) bind q=%s, k=%s, tags=%s, op=%s, origin=%s" % \
        (self.broker.tag, self.name, queue.name, key, tagList, fedOp, origin),

    if self.broker.tag in tagList:
      print "(tag ignored)"
      return

    if fedOp == "bind" or fedOp == "unbind":
      if key not in self.keys:
        self.keys[key] = {}
      queueMap = self.keys[key]

    if fedOp == "bind":
      ##
      ## Add local or federation binding case
      ##
      if queue in queueMap:
        if origin and origin in queueMap[queue].originList:
          print "(dup ignored)"
        elif origin:
          queueMap[queue].originList.append(origin)
          print "(origin added)"
      else:
        binding = Binding(origin)
        queueMap[queue] = binding
        print "(binding added)"

    elif fedOp == "unbind":
      ##
      ## Delete federation binding case
      ##
      if queue in queueMap:
        binding = queueMap[queue]
        if origin and origin in binding.originList:
          binding.originList.remove(origin)
          if len(binding.originList) == 0:
            queueMap.pop(queue)
            if len(queueMap) == 0:
              self.keys.pop(key)
            print "(last origin del)"
          else:
            print "(removed origin)"
        else:
          print "(origin not found)"
      else:
        print "(queue not found)"

    elif fedOp == "reorigin":
      print "(ok)"
      self.reorigin()

    elif fedOp == "hello":
      print "(ok)"

    else:
      raise Exception("Unknown fed-opcode '%s'" % fedOp)

    newTagList = []
    newTagList.append(self.broker.tag)
    for tag in tagList:
      newTagList.append(tag)
    if origin:
      propOrigin = origin
    else:
      propOrigin = self.broker.tag

    for bridge in self.bridges:
      if bridge.isDynamic():
        bridge.propagate(key, newTagList, fedOp, propOrigin)

  def reorigin(self):
    myTag = []
    myTag.append(self.broker.tag)
    for key in self.keys:
      queueMap = self.keys[key]
      found = False
      for queue in queueMap:
        binding = queueMap[queue]
        if binding.isLocal():
          found = True
      if found:
        for bridge in self.bridges:
          if bridge.isDynamic():
            bridge.propagate(key, myTag, "bind", self.broker.tag)

  def unbind(self, queue, key):
    print "Exchange(%s.%s) unbind q=%s, k=%s" % (self.broker.tag, self.name, queue.name, key),
    if key not in self.keys:
      print "(key not known)"
      return
    queueMap = self.keys[key]
    if queue not in queueMap:
      print "(queue not bound)"
      return
    queueMap.pop(queue)
    if len(queueMap) == 0:
      self.keys.pop(key)
      print "(ok, remove bound-key)"
    else:
      print "(ok)"

    count = 0
    for queue in queueMap:
      if len(queueMap[queue].originList) == 0:
        count += 1

    if count == 0:
      myTag = []
      myTag.append(self.broker.tag)
      for bridge in self.bridges:
        if bridge.isDynamic():
          bridge.propagate(key, myTag, "unbind", self.broker.tag)

  def receive(self, key, msg, fromUser=False):
    sent = False
    if key in self.keys:
      queueMap = self.keys[key]
      for queue in queueMap:
        if queue.enqueue(msg):
          sent = True
    if not sent and not fromUser:
      self.sim.warning("Exchange(%s.%s) received unroutable message: k=%s" % \
                         (self.broker.tag, self.name, key))

  def addDynamicBridge(self, bridge):
    if bridge in self.bridges:
      raise Exception("Dynamic bridge already added to exchange")
    self.bridges.append(bridge)

    for b in self.bridges:
      if b != bridge:
        b.sendReorigin()
    self.reorigin()

class Queue:
  def __init__(self, broker, name, tag=None, exclude=None):
    self.broker = broker
    self.name = name
    self.tag = tag
    self.exclude = exclude
    self.dest = None

  def setDest(self, dest):
    self.dest = dest

  def enqueue(self, msg):
    print "Queue(%s.%s) rcvd k=%s, tags=%s" % (self.broker.tag, self.name, msg.key, msg.tags),
    if self.dest == None:
      print "(dropped, no dest)"
      return False
    if self.exclude and msg.tagFound(self.exclude):
      print "(dropped, tag)"
      return False
    if self.tag:
      msg.appendTag(self.tag)
    print "(ok)"
    self.dest.receive(msg.key, msg)
    return True


class Binding:
  def __init__(self, origin):
    self.originList = []
    if origin:
      self.originList.append(origin)

  def isLocal(self):
    return len(self.originList) == 0


class Link:
  def __init__(self, local, remote):
    self.local = local
    self.remote = remote
    self.remote.connect(self)
    self.bridges = []

  def bridge(self, exchangeName):
    bridge = Bridge(self, exchangeName)


class Bridge:
  def __init__(self, link, exchangeName):
    self.link = link
    self.exchangeName = exchangeName
    if self.exchangeName not in link.local.exchanges:
      raise Exception("Exchange not found")
    self.exchange = link.local.exchanges[self.exchangeName]
    self.queueName = "bridge." + link.local.tag
    self.link.remote.declare_queue(self.queueName, self.link.remote.tag, self.link.local.tag)
    self.link.remote.subscribe(self.exchange, self.queueName)
    self.exchange.addDynamicBridge(self)

  def isDynamic(self):
    return True

  def localTag(self):
    return self.link.local.tag

  def remoteTag(self):
    return self.link.remote.tag

  def propagate(self, key, tagList, fedOp, origin):
    if self.link.remote.tag not in tagList:
      self.link.remote.bind(self.exchangeName, self.queueName, key, tagList, fedOp, origin)

  def sendReorigin(self):
    myTag = []
    myTag.append(self.link.local.tag)
    self.link.remote.bind(self.exchangeName, self.queueName, "", myTag, "reorigin", "")


class Message:
  def __init__(self, key, body):
    self.key = key
    self.body = body
    self.tags = []

  def appendTag(self, tag):
    if tag not in self.tags:
      self.tags.append(tag)

  def tagFound(self, tag):
    return tag in self.tags


