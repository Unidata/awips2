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

""" Console API for Qpid Management Framework """

import os
import platform
import qpid
import struct
import socket
import re
from qpid.datatypes  import UUID
from qpid.datatypes  import timestamp
from qpid.datatypes  import datetime
from qpid.peer       import Closed
from qpid.session    import SessionDetached
from qpid.connection import Connection, ConnectionFailed, Timeout
from qpid.datatypes  import Message, RangedSet, UUID
from qpid.util       import connect, ssl, URL
from qpid.codec010   import StringCodec as Codec
from threading       import Lock, Condition, Thread
from time            import time, strftime, gmtime
from cStringIO       import StringIO

#import qpid.log
#qpid.log.enable(name="qpid.io.cmd", level=qpid.log.DEBUG)

class Console:
  """ To access the asynchronous operations, a class must be derived from
  Console with overrides of any combination of the available methods. """

  def brokerConnected(self, broker):
    """ Invoked when a connection is established to a broker """
    pass

  def brokerDisconnected(self, broker):
    """ Invoked when the connection to a broker is lost """
    pass

  def newPackage(self, name):
    """ Invoked when a QMF package is discovered. """
    pass

  def newClass(self, kind, classKey):
    """ Invoked when a new class is discovered.  Session.getSchema can be
    used to obtain details about the class."""
    pass

  def newAgent(self, agent):
    """ Invoked when a QMF agent is discovered. """
    pass

  def delAgent(self, agent):
    """ Invoked when a QMF agent disconects. """
    pass

  def objectProps(self, broker, record):
    """ Invoked when an object is updated. """
    pass

  def objectStats(self, broker, record):
    """ Invoked when an object is updated. """
    pass

  def event(self, broker, event):
    """ Invoked when an event is raised. """
    pass

  def heartbeat(self, agent, timestamp):
    """ Invoked when an agent heartbeat is received. """
    pass

  def brokerInfo(self, broker):
    """ Invoked when the connection sequence reaches the point where broker information is available. """
    pass

  def methodResponse(self, broker, seq, response):
    """ Invoked when a method response from an asynchronous method call is received. """
    pass

class BrokerURL(URL):
  def __init__(self, text):
    URL.__init__(self, text)
    if self.port is None:
      if self.scheme == URL.AMQPS:
        self.port = 5671
      else:
        self.port = 5672
    self.authName = None
    self.authPass = None
    if self.user:
      self.authName = str(self.user)
    if self.password:
      self.authPass = str(self.password)

  def name(self):
    return self.host + ":" + str(self.port)

  def match(self, host, port):
    return socket.getaddrinfo(self.host, self.port)[0][4] == socket.getaddrinfo(host, port)[0][4]

class Object(object):
  """ This class defines a 'proxy' object representing a real managed object on an agent.
      Actions taken on this proxy are remotely affected on the real managed object.
  """
  def __init__(self, session, broker, schema, codec, prop, stat, managed=True, kwargs={}):
    self._session = session
    self._broker  = broker
    self._schema  = schema
    self._managed = managed
    if self._managed:
      self._currentTime = codec.read_uint64()
      self._createTime  = codec.read_uint64()
      self._deleteTime  = codec.read_uint64()
      self._objectId    = ObjectId(codec)
    else:
      self._currentTime = None
      self._createTime  = None
      self._deleteTime  = None
      self._objectId    = None
    self._properties  = []
    self._statistics  = []
    if codec:
      if prop:
        notPresent = self._parsePresenceMasks(codec, schema)
        for property in schema.getProperties():
          if property.name in notPresent:
            self._properties.append((property, None))
          else:
            self._properties.append((property, self._session._decodeValue(codec, property.type, broker)))
      if stat:
        for statistic in schema.getStatistics():
          self._statistics.append((statistic, self._session._decodeValue(codec, statistic.type, broker)))
    else:
      for property in schema.getProperties():
        if property.optional:
          self._properties.append((property, None))
        else:
          self._properties.append((property, self._session._defaultValue(property, broker, kwargs)))
      for statistic in schema.getStatistics():
          self._statistics.append((statistic, self._session._defaultValue(statistic, broker, kwargs)))

  def getBroker(self):
    """ Return the broker from which this object was sent """
    return self._broker

  def getObjectId(self):
    """ Return the object identifier for this object """
    return self._objectId

  def getClassKey(self):
    """ Return the class-key that references the schema describing this object. """
    return self._schema.getKey()

  def getSchema(self):
    """ Return the schema that describes this object. """
    return self._schema

  def getMethods(self):
    """ Return a list of methods available for this object. """
    return self._schema.getMethods()

  def getTimestamps(self):
    """ Return the current, creation, and deletion times for this object. """
    return self._currentTime, self._createTime, self._deleteTime

  def isDeleted(self):
    """ Return True iff this object has been deleted. """
    return self._deleteTime != 0

  def isManaged(self):
    """ Return True iff this object is a proxy for a managed object on an agent. """
    return self._managed

  def getIndex(self):
    """ Return a string describing this object's primary key. """
    result = u""
    for property, value in self._properties:
      if property.index:
        if result != u"":
          result += u":"
        try:
          valstr = unicode(self._session._displayValue(value, property.type))
        except:
          valstr = u"<undecodable>"
        result += valstr
    return result

  def getProperties(self):
    """ Return a list of object properties """
    return self._properties

  def getStatistics(self):
    """ Return a list of object statistics """
    return self._statistics

  def mergeUpdate(self, newer):
    """ Replace properties and/or statistics with a newly received update """
    if not self.isManaged():
      raise Exception("Object is not managed")
    if self._objectId != newer._objectId:
      raise Exception("Objects with different object-ids")
    if len(newer.getProperties()) > 0:
      self._properties = newer.getProperties()
    if len(newer.getStatistics()) > 0:
      self._statistics = newer.getStatistics()

  def update(self):
    """ Contact the agent and retrieve the lastest property and statistic values for this object. """
    if not self.isManaged():
      raise Exception("Object is not managed")
    obj = self._session.getObjects(_objectId = self._objectId, _broker=self._broker)
    if obj:
      self.mergeUpdate(obj[0])
    else:
      raise Exception("Underlying object no longer exists")

  def __repr__(self):
    if self.isManaged():
      id = self.getObjectId().__repr__()
    else:
      id = "unmanaged"
    key = self.getClassKey()
    return key.getPackageName() + ":" + key.getClassName() +\
        "[" + id + "] " + self.getIndex().encode("utf8")

  def __getattr__(self, name):
    for method in self._schema.getMethods():
      if name == method.name:
        return lambda *args, **kwargs : self._invoke(name, args, kwargs)
    for property, value in self._properties:
      if name == property.name:
        return value
      if name == "_" + property.name + "_" and property.type == 10:  # Dereference references
        deref = self._session.getObjects(_objectId=value, _broker=self._broker)
        if len(deref) != 1:
          return None
        else:
          return deref[0]
    for statistic, value in self._statistics:
      if name == statistic.name:
        return value
    raise Exception("Type Object has no attribute '%s'" % name)

  def __setattr__(self, name, value):
    if name[0] == '_':
      super.__setattr__(self, name, value)
      return

    for prop, unusedValue in self._properties:
      if name == prop.name:
        newprop = (prop, value)
        newlist = []
        for old, val in self._properties:
          if name == old.name:
            newlist.append(newprop)
          else:
            newlist.append((old, val))
        self._properties = newlist
        return
    super.__setattr__(self, name, value)

  def _sendMethodRequest(self, name, args, kwargs, synchronous=False, timeWait=None):
    for method in self._schema.getMethods():
      if name == method.name:
        aIdx = 0
        sendCodec = Codec()
        seq = self._session.seqMgr._reserve((method, synchronous))
        self._broker._setHeader(sendCodec, 'M', seq)
        self._objectId.encode(sendCodec)
        self._schema.getKey().encode(sendCodec)
        sendCodec.write_str8(name)

        count = 0
        for arg in method.arguments:
          if arg.dir.find("I") != -1:
            count += 1
        if count != len(args):
          raise Exception("Incorrect number of arguments: expected %d, got %d" % (count, len(args)))

        for arg in method.arguments:
          if arg.dir.find("I") != -1:
            self._session._encodeValue(sendCodec, args[aIdx], arg.type)
            aIdx += 1
        if timeWait:
          ttl = timeWait * 1000
        else:
          ttl = None
        smsg = self._broker._message(sendCodec.encoded, "agent.%d.%d" %
                                     (self._objectId.getBrokerBank(), self._objectId.getAgentBank()),
                                     ttl=ttl)
        if synchronous:
          try:
            self._broker.cv.acquire()
            self._broker.syncInFlight = True
          finally:
            self._broker.cv.release()
        self._broker._send(smsg)
        return seq
    return None

  def _invoke(self, name, args, kwargs):
    if not self.isManaged():
      raise Exception("Object is not managed")
    if "_timeout" in kwargs:
      timeout = kwargs["_timeout"]
    else:
      timeout = self._broker.SYNC_TIME

    if "_async" in kwargs and kwargs["_async"]:
      sync = False
      if "_timeout" not in kwargs:
        timeout = None
    else:
      sync = True

    seq = self._sendMethodRequest(name, args, kwargs, sync, timeout)
    if seq:
      if not sync:
        return seq
      try:
        self._broker.cv.acquire()
        starttime = time()
        while self._broker.syncInFlight and self._broker.error == None:
          self._broker.cv.wait(timeout)
          if time() - starttime > timeout:
            self._session.seqMgr._release(seq)
            raise RuntimeError("Timed out waiting for method to respond")
      finally:
        self._broker.cv.release()
      if self._broker.error != None:
        errorText = self._broker.error
        self._broker.error = None
        raise Exception(errorText)
      return self._broker.syncResult
    raise Exception("Invalid Method (software defect) [%s]" % name)

  def _encodeUnmanaged(self, codec):

    codec.write_uint8(20) 
    codec.write_str8(self._schema.getKey().getPackageName())
    codec.write_str8(self._schema.getKey().getClassName())
    codec.write_bin128(self._schema.getKey().getHash())

    # emit presence masks for optional properties
    mask = 0
    bit  = 0
    for prop, value in self._properties:
      if prop.optional:
        if bit == 0:
          bit = 1
        if value:
          mask |= bit
        bit = bit << 1
        if bit == 256:
          bit = 0
          codec.write_uint8(mask)
          mask = 0
    if bit != 0:
      codec.write_uint8(mask)    
    
    # encode properties
    for prop, value in self._properties:
      if value != None: 
        self._session._encodeValue(codec, value, prop.type)

    # encode statistics
    for stat, value in self._statistics:
      self._session._encodeValue(codec, value, stat.type)

  def _parsePresenceMasks(self, codec, schema):
    excludeList = []
    bit = 0
    for property in schema.getProperties():
      if property.optional:
        if bit == 0:
          mask = codec.read_uint8()
          bit = 1
        if (mask & bit) == 0:
          excludeList.append(property.name)
        bit *= 2
        if bit == 256:
          bit = 0
    return excludeList    

class Session:
  """
  An instance of the Session class represents a console session running
  against one or more QMF brokers.  A single instance of Session is needed
  to interact with the management framework as a console.
  """
  _CONTEXT_SYNC     = 1
  _CONTEXT_STARTUP  = 2
  _CONTEXT_MULTIGET = 3

  DEFAULT_GET_WAIT_TIME = 60

  ENCODINGS = {
    str: 7,
    timestamp: 8,
    datetime: 8,  
    int: 9,
    long: 9,
    float: 13,
    UUID: 14,      
    Object: 20,    
    list: 21
    }  

  def __init__(self, console=None, rcvObjects=True, rcvEvents=True, rcvHeartbeats=True,
               manageConnections=False, userBindings=False):
    """
    Initialize a session.  If the console argument is provided, the
    more advanced asynchronous features are available.  If console is
    defaulted, the session will operate in a simpler, synchronous manner.

    The rcvObjects, rcvEvents, and rcvHeartbeats arguments are meaningful only if 'console'
    is provided.  They control whether object updates, events, and agent-heartbeats are
    subscribed to.  If the console is not interested in receiving one or more of the above,
    setting the argument to False will reduce tha bandwidth used by the API.

    If manageConnections is set to True, the Session object will manage connections to
    the brokers.  This means that if a broker is unreachable, it will retry until a connection
    can be established.  If a connection is lost, the Session will attempt to reconnect.

    If manageConnections is set to False, the user is responsible for handing failures.  In
    this case, an unreachable broker will cause addBroker to raise an exception.

    If userBindings is set to False (the default) and rcvObjects is True, the console will
    receive data for all object classes.  If userBindings is set to True, the user must select
    which classes the console shall receive by invoking the bindPackage or bindClass methods.
    This allows the console to be configured to receive only information that is relavant to
    a particular application.  If rcvObjects id False, userBindings has no meaning.
    """
    self.console           = console
    self.brokers           = []
    self.packages          = {}
    self.seqMgr            = SequenceManager()
    self.cv                = Condition()
    self.syncSequenceList  = []
    self.getResult         = []
    self.getSelect         = []
    self.error             = None
    self.rcvObjects        = rcvObjects
    self.rcvEvents         = rcvEvents
    self.rcvHeartbeats     = rcvHeartbeats
    self.userBindings      = userBindings
    if self.console == None:
      self.rcvObjects    = False
      self.rcvEvents     = False
      self.rcvHeartbeats = False
    self.bindingKeyList    = self._bindingKeys()
    self.manageConnections = manageConnections

    if self.userBindings and not self.rcvObjects:
      raise Exception("userBindings can't be set unless rcvObjects is set and a console is provided")

  def __repr__(self):
    return "QMF Console Session Manager (brokers: %d)" % len(self.brokers)

  def addBroker(self, target="localhost", timeout=None, mechanisms=None):
    """ Connect to a Qpid broker.  Returns an object of type Broker. """
    url = BrokerURL(target)
    broker = Broker(self, url.host, url.port, mechanisms, url.authName, url.authPass,
                    ssl = url.scheme == URL.AMQPS, connTimeout=timeout)

    self.brokers.append(broker)
    if not self.manageConnections:
      self.getObjects(broker=broker, _class="agent")
    return broker

  def delBroker(self, broker):
    """ Disconnect from a broker.  The 'broker' argument is the object
    returned from the addBroker call """
    if self.console:
      for agent in broker.getAgents():
        self.console.delAgent(agent)
    broker._shutdown()
    self.brokers.remove(broker)
    del broker

  def getPackages(self):
    """ Get the list of known QMF packages """
    for broker in self.brokers:
      broker._waitForStable()
    list = []
    for package in self.packages:
      list.append(package)
    return list

  def getClasses(self, packageName):
    """ Get the list of known classes within a QMF package """
    for broker in self.brokers:
      broker._waitForStable()
    list = []
    if packageName in self.packages:
      for pkey in self.packages[packageName]:
        list.append(self.packages[packageName][pkey].getKey())
    return list

  def getSchema(self, classKey):
    """ Get the schema for a QMF class """
    for broker in self.brokers:
      broker._waitForStable()
    pname = classKey.getPackageName()
    pkey = classKey.getPackageKey()
    if pname in self.packages:
      if pkey in self.packages[pname]:
        return self.packages[pname][pkey]

  def bindPackage(self, packageName):
    """ Request object updates for all table classes within a package. """
    if not self.userBindings or not self.rcvObjects:
      raise Exception("userBindings option not set for Session")
    key = "console.obj.*.*.%s.#" % packageName
    self.bindingKeyList.append(key)
    for broker in self.brokers:
      if broker.isConnected():
        broker.amqpSession.exchange_bind(exchange="qpid.management", queue=broker.topicName,
                                         binding_key=key)

  def bindClass(self, pname, cname):
    """ Request object updates for a particular table class by package and class name. """
    if not self.userBindings or not self.rcvObjects:
      raise Exception("userBindings option not set for Session")
    key = "console.obj.*.*.%s.%s.#" % (pname, cname)
    self.bindingKeyList.append(key)
    for broker in self.brokers:
      if broker.isConnected():
        broker.amqpSession.exchange_bind(exchange="qpid.management", queue=broker.topicName,
                                         binding_key=key)
      
  def bindClassKey(self, classKey):
    """ Request object updates for a particular table class by class key. """
    pname = classKey.getPackageName()
    cname = classKey.getClassName()
    self.bindClass(pname, cname)

  def getAgents(self, broker=None):
    """ Get a list of currently known agents """
    brokerList = []
    if broker == None:
      for b in self.brokers:
        brokerList.append(b)
    else:
      brokerList.append(broker)

    for b in brokerList:
      b._waitForStable()
    agentList = []
    for b in brokerList:
      for a in b.getAgents():
        agentList.append(a)
    return agentList

  def makeObject(self, classKey, broker=None, **kwargs):
    """ Create a new, unmanaged object of the schema indicated by classKey """
    schema = self.getSchema(classKey)
    if schema == None:
      raise Exception("Schema not found for classKey")
    return Object(self, broker, schema, None, True, True, False, kwargs)

  def getObjects(self, **kwargs):
    """ Get a list of objects from QMF agents.
    All arguments are passed by name(keyword).

    The class for queried objects may be specified in one of the following ways:

    _schema = <schema> - supply a schema object returned from getSchema.
    _key = <key>       - supply a classKey from the list returned by getClasses.
    _class = <name>    - supply a class name as a string.  If the class name exists
                         in multiple packages, a _package argument may also be supplied.
    _objectId = <id>   - get the object referenced by the object-id

    If objects should be obtained from only one agent, use the following argument.
    Otherwise, the query will go to all agents.

    _agent = <agent> - supply an agent from the list returned by getAgents.

    If the get query is to be restricted to one broker (as opposed to all connected brokers),
    add the following argument:

    _broker = <broker> - supply a broker as returned by addBroker.

    The default timeout for this synchronous operation is 60 seconds.  To change the timeout,
    use the following argument:

    _timeout = <time in seconds>

    If additional arguments are supplied, they are used as property selectors.  For example,
    if the argument name="test" is supplied, only objects whose "name" property is "test"
    will be returned in the result.
    """
    if "_broker" in kwargs:
      brokerList = []
      brokerList.append(kwargs["_broker"])
    else:
      brokerList = self.brokers
    for broker in brokerList:
      broker._waitForStable()
      if broker.isConnected():
        if "_package" not in kwargs or "_class" not in kwargs or \
              kwargs["_package"] != "org.apache.qpid.broker" or \
              kwargs["_class"] != "agent":
          self.getObjects(_package = "org.apache.qpid.broker", _class = "agent",
                     _agent = broker.getAgent(1,0))

    agentList = []
    if "_agent" in kwargs:
      agent = kwargs["_agent"]
      if agent.broker not in brokerList:
        raise Exception("Supplied agent is not accessible through the supplied broker")
      if agent.broker.isConnected():
        agentList.append(agent)
    else:
      if "_objectId" in kwargs:
        oid = kwargs["_objectId"]
        for broker in brokerList:
          for agent in broker.getAgents():
            if agent.getBrokerBank() == oid.getBrokerBank() and agent.getAgentBank() == oid.getAgentBank():
              agentList.append(agent)
      else:
        for broker in brokerList:
          for agent in broker.getAgents():
            if agent.broker.isConnected():
              agentList.append(agent)

    if len(agentList) == 0:
      return []

    pname = None
    cname = None
    hash = None
    classKey = None
    if   "_schema" in kwargs: classKey = kwargs["_schema"].getKey()
    elif "_key"    in kwargs: classKey = kwargs["_key"]
    elif "_class"  in kwargs:
      cname = kwargs["_class"]
      if "_package" in kwargs:
        pname = kwargs["_package"]
    if cname == None and classKey == None and "_objectId" not in kwargs:
      raise Exception("No class supplied, use '_schema', '_key', '_class', or '_objectId' argument")

    map = {}
    self.getSelect = []
    if "_objectId" in kwargs:
      map["_objectid"] = kwargs["_objectId"].__repr__()
    else:
      if cname == None:
        cname = classKey.getClassName()
        pname = classKey.getPackageName()
        hash = classKey.getHash()
      map["_class"] = cname
      if pname != None: map["_package"] = pname
      if hash  != None: map["_hash"]    = hash
      for item in kwargs:
        if item[0] != '_':
          self.getSelect.append((item, kwargs[item]))

    self.getResult = []
    for agent in agentList:
      broker = agent.broker
      sendCodec = Codec()
      try:
        self.cv.acquire()
        seq = self.seqMgr._reserve(self._CONTEXT_MULTIGET)
        self.syncSequenceList.append(seq)
      finally:
        self.cv.release()
      broker._setHeader(sendCodec, 'G', seq)
      sendCodec.write_map(map)
      smsg = broker._message(sendCodec.encoded, "agent.%d.%d" % (agent.brokerBank, agent.agentBank))
      broker._send(smsg)

    starttime = time()
    timeout = False
    if "_timeout" in kwargs:
      waitTime = kwargs["_timeout"]
    else:
      waitTime = self.DEFAULT_GET_WAIT_TIME
    try:
      self.cv.acquire()
      while len(self.syncSequenceList) > 0 and self.error == None:
        self.cv.wait(waitTime)
        if time() - starttime > waitTime:
          for pendingSeq in self.syncSequenceList:
            self.seqMgr._release(pendingSeq)
          self.syncSequenceList = []
          timeout = True
    finally:
      self.cv.release()

    if self.error:
      errorText = self.error
      self.error = None
      raise Exception(errorText)

    if len(self.getResult) == 0 and timeout:
      raise RuntimeError("No agent responded within timeout period")
    return self.getResult

  def setEventFilter(self, **kwargs):
    """ """
    pass

  def _bindingKeys(self):
    keyList = []
    keyList.append("schema.#")
    if self.rcvObjects and self.rcvEvents and self.rcvHeartbeats and not self.userBindings:
      keyList.append("console.#")
    else:
      if self.rcvObjects and not self.userBindings:
        keyList.append("console.obj.#")
      else:
        keyList.append("console.obj.*.*.org.apache.qpid.broker.agent")
      if self.rcvEvents:
        keyList.append("console.event.#")
      if self.rcvHeartbeats:
        keyList.append("console.heartbeat.#")
    return keyList

  def _handleBrokerConnect(self, broker):
    if self.console:
      for agent in broker.getAgents():
        self.console.newAgent(agent)
      self.console.brokerConnected(broker)

  def _handleBrokerDisconnect(self, broker):
    if self.console:
      for agent in broker.getAgents():
        self.console.delAgent(agent)
      self.console.brokerDisconnected(broker)

  def _handleBrokerResp(self, broker, codec, seq):
    broker.brokerId = codec.read_uuid()
    if self.console != None:
      self.console.brokerInfo(broker)

    # Send a package request
    # (effectively inc and dec outstanding by not doing anything)
    sendCodec = Codec()
    seq = self.seqMgr._reserve(self._CONTEXT_STARTUP)
    broker._setHeader(sendCodec, 'P', seq)
    smsg = broker._message(sendCodec.encoded)
    broker._send(smsg)

  def _handlePackageInd(self, broker, codec, seq):
    pname = str(codec.read_str8())
    notify = False
    try:
      self.cv.acquire()
      if pname not in self.packages:
        self.packages[pname] = {}
        notify = True
    finally:
      self.cv.release()
    if notify and self.console != None:
      self.console.newPackage(pname)

    # Send a class request
    broker._incOutstanding()
    sendCodec = Codec()
    seq = self.seqMgr._reserve(self._CONTEXT_STARTUP)
    broker._setHeader(sendCodec, 'Q', seq)
    sendCodec.write_str8(pname)
    smsg = broker._message(sendCodec.encoded)
    broker._send(smsg)

  def _handleCommandComplete(self, broker, codec, seq):
    code = codec.read_uint32()
    text = codec.read_str8()
    context = self.seqMgr._release(seq)
    if context == self._CONTEXT_STARTUP:
      broker._decOutstanding()
    elif context == self._CONTEXT_SYNC and seq == broker.syncSequence:
      try:
        broker.cv.acquire()
        broker.syncInFlight = False
        broker.cv.notify()
      finally:
        broker.cv.release()
    elif context == self._CONTEXT_MULTIGET and seq in self.syncSequenceList:
      try:
        self.cv.acquire()
        self.syncSequenceList.remove(seq)
        if len(self.syncSequenceList) == 0:
          self.cv.notify()
      finally:
        self.cv.release()

  def _handleClassInd(self, broker, codec, seq):
    kind  = codec.read_uint8()
    classKey = ClassKey(codec)
    unknown = False

    try:
      self.cv.acquire()
      if classKey.getPackageName() in self.packages:
        if classKey.getPackageKey() not in self.packages[classKey.getPackageName()]:
          unknown = True
    finally:
      self.cv.release()

    if unknown:
      # Send a schema request for the unknown class
      broker._incOutstanding()
      sendCodec = Codec()
      seq = self.seqMgr._reserve(self._CONTEXT_STARTUP)
      broker._setHeader(sendCodec, 'S', seq)
      classKey.encode(sendCodec)
      smsg = broker._message(sendCodec.encoded)
      broker._send(smsg)

  def _handleMethodResp(self, broker, codec, seq):
    code = codec.read_uint32()
    text = codec.read_str16()
    outArgs = {}
    pair = self.seqMgr._release(seq)
    if pair == None:
      return
    method, synchronous = pair
    if code == 0:
      for arg in method.arguments:
        if arg.dir.find("O") != -1:
          outArgs[arg.name] = self._decodeValue(codec, arg.type, broker)
    result = MethodResult(code, text, outArgs)
    if synchronous:
      try:
        broker.cv.acquire()
        broker.syncResult = result
        broker.syncInFlight = False
        broker.cv.notify()
      finally:
        broker.cv.release()
    else:
      if self.console:
        self.console.methodResponse(broker, seq, result)

  def _handleHeartbeatInd(self, broker, codec, seq, msg):
    brokerBank = 1
    agentBank = 0
    dp = msg.get("delivery_properties")
    if dp:
      key = dp["routing_key"]
      keyElements = key.split(".")
      if len(keyElements) == 4:
        brokerBank = int(keyElements[2])
        agentBank = int(keyElements[3])

    agent = broker.getAgent(brokerBank, agentBank)
    timestamp = codec.read_uint64()
    if self.console != None and agent != None:
      self.console.heartbeat(agent, timestamp)

  def _handleEventInd(self, broker, codec, seq):
    if self.console != None:
      event = Event(self, broker, codec)
      self.console.event(broker, event)

  def _handleSchemaResp(self, broker, codec, seq):
    kind  = codec.read_uint8()
    classKey = ClassKey(codec)
    _class = SchemaClass(kind, classKey, codec, self)
    try:
      self.cv.acquire()
      self.packages[classKey.getPackageName()][classKey.getPackageKey()] = _class
    finally:
      self.cv.release()
      
    self.seqMgr._release(seq)
    broker._decOutstanding()
    if self.console != None:
      self.console.newClass(kind, classKey)

  def _handleContentInd(self, broker, codec, seq, prop=False, stat=False):
    classKey = ClassKey(codec)
    try:
      self.cv.acquire()
      pname = classKey.getPackageName()
      if pname not in self.packages:
        return
      pkey = classKey.getPackageKey()
      if pkey not in self.packages[pname]:
        return
      schema = self.packages[pname][pkey]
    finally:
      self.cv.release()

    object = Object(self, broker, schema, codec, prop, stat)
    if pname == "org.apache.qpid.broker" and classKey.getClassName() == "agent" and prop:
      broker._updateAgent(object)

    try:
      self.cv.acquire()
      if seq in self.syncSequenceList:
        if object.getTimestamps()[2] == 0 and self._selectMatch(object):
          self.getResult.append(object)
        return
    finally:
      self.cv.release()

    if self.console and self.rcvObjects:
      if prop:
        self.console.objectProps(broker, object)
      if stat:
        self.console.objectStats(broker, object)

  def _handleError(self, error):
    try:
      self.cv.acquire()
      if len(self.syncSequenceList) > 0:
        self.error = error
      self.syncSequenceList = []
      self.cv.notify()
    finally:
      self.cv.release()

  def _selectMatch(self, object):
    """ Check the object against self.getSelect to check for a match """
    for key, value in self.getSelect:
      for prop, propval in object.getProperties():
        if key == prop.name and value != propval:
          return False
    return True
  
  def _decodeValue(self, codec, typecode, broker=None):
    """ Decode, from the codec, a value based on its typecode. """
    if   typecode == 1:  data = codec.read_uint8()      # U8
    elif typecode == 2:  data = codec.read_uint16()     # U16
    elif typecode == 3:  data = codec.read_uint32()     # U32
    elif typecode == 4:  data = codec.read_uint64()     # U64
    elif typecode == 6:  data = codec.read_str8()       # SSTR
    elif typecode == 7:  data = codec.read_str16()      # LSTR
    elif typecode == 8:  data = codec.read_int64()      # ABSTIME
    elif typecode == 9:  data = codec.read_uint64()     # DELTATIME
    elif typecode == 10: data = ObjectId(codec)         # REF
    elif typecode == 11: data = codec.read_uint8() != 0 # BOOL
    elif typecode == 12: data = codec.read_float()      # FLOAT
    elif typecode == 13: data = codec.read_double()     # DOUBLE
    elif typecode == 14: data = codec.read_uuid()       # UUID
    elif typecode == 16: data = codec.read_int8()       # S8
    elif typecode == 17: data = codec.read_int16()      # S16
    elif typecode == 18: data = codec.read_int32()      # S32
    elif typecode == 19: data = codec.read_int64()      # S63
    elif typecode == 15: data = codec.read_map()        # FTABLE
    elif typecode == 20:                                # OBJECT
      # Peek at the type, and if it is still 20 pull it decode. If
      # Not, call back into self.
      inner_type_code = codec.read_uint8()
      if inner_type_code == 20:
          classKey = ClassKey(codec)
          try:
            self.cv.acquire()
            pname = classKey.getPackageName()
            if pname not in self.packages:
              return None
            pkey = classKey.getPackageKey()
            if pkey not in self.packages[pname]:
              return None
            schema = self.packages[pname][pkey]
          finally:
            self.cv.release()
          data = Object(self, broker, schema, codec, True, True, False)
      else:
          data = self._decodeValue(codec, inner_type_code, broker)
    elif typecode == 21:                                # List
        #taken from codec10.read_list
        sc = Codec(codec.read_vbin32())
        count = sc.read_uint32()
        data = []
        while count > 0:
          type = sc.read_uint8()
          data.append(self._decodeValue(sc,type,broker))
          count -= 1
    elif typecode == 22:                                #Array
        #taken from codec10.read_array
        sc = Codec(codec.read_vbin32()) 
        count = sc.read_uint32()
        type = sc.read_uint8()
        data = []
        while count > 0:
          data.append(self._decodeValue(sc,type,broker))
          count -= 1
    else:
      raise ValueError("Invalid type code: %d" % typecode)
    return data

  def _encodeValue(self, codec, value, typecode):
    """ Encode, into the codec, a value based on its typecode. """
    if   typecode == 1:  codec.write_uint8  (int(value))    # U8
    elif typecode == 2:  codec.write_uint16 (int(value))    # U16
    elif typecode == 3:  codec.write_uint32 (long(value))   # U32
    elif typecode == 4:  codec.write_uint64 (long(value))   # U64
    elif typecode == 6:  codec.write_str8   (value)         # SSTR
    elif typecode == 7:  codec.write_str16  (value)         # LSTR
    elif typecode == 8:  codec.write_int64  (long(value))   # ABSTIME
    elif typecode == 9:  codec.write_uint64 (long(value))   # DELTATIME
    elif typecode == 10: value.encode       (codec)         # REF
    elif typecode == 11: codec.write_uint8  (int(value))    # BOOL
    elif typecode == 12: codec.write_float  (float(value))  # FLOAT
    elif typecode == 13: codec.write_double (float(value))  # DOUBLE
    elif typecode == 14: codec.write_uuid   (value.bytes)   # UUID
    elif typecode == 16: codec.write_int8   (int(value))    # S8
    elif typecode == 17: codec.write_int16  (int(value))    # S16
    elif typecode == 18: codec.write_int32  (int(value))    # S32
    elif typecode == 19: codec.write_int64  (int(value))    # S64
    elif typecode == 20: value._encodeUnmanaged(codec)      # OBJECT
    elif typecode == 15: codec.write_map    (value)         # FTABLE
    elif typecode == 21:                                    # List
        sc = Codec()
        self._encodeValue(sc, len(value), 3)
        for o in value:
          ltype=self.encoding(o)
          self._encodeValue(sc,ltype,1)
          self._encodeValue(sc, o, ltype)
        codec.write_vbin32(sc.encoded)
    elif typecode == 22:                                    # Array
        sc = Codec()
        self._encodeValue(sc, len(value), 3)
        if len(value) > 0:
            ltype = self.encoding(value[0])
            self._encodeValue(sc,ltype,1)
            for o in value:
              self._encodeValue(sc, o, ltype)
        codec.write_vbin32(sc.encoded)
    else:
      raise ValueError ("Invalid type code: %d" % typecode)

  def encoding(self, value):
      return self._encoding(value.__class__)
      
  def _encoding(self, klass):
    if Session.ENCODINGS.has_key(klass):
      return self.ENCODINGS[klass]
    for base in klass.__bases__:
      result = self._encoding(base, obj)
      if result != None:
        return result
            
  def _displayValue(self, value, typecode):
    """ """
    if   typecode == 1:  return unicode(value)
    elif typecode == 2:  return unicode(value)
    elif typecode == 3:  return unicode(value)
    elif typecode == 4:  return unicode(value)
    elif typecode == 6:  return value
    elif typecode == 7:  return value
    elif typecode == 8:  return unicode(strftime("%c", gmtime(value / 1000000000)))
    elif typecode == 9:  return unicode(value)
    elif typecode == 10: return unicode(value.__repr__())
    elif typecode == 11:
      if value: return u"T"
      else:     return u"F"
    elif typecode == 12: return unicode(value)
    elif typecode == 13: return unicode(value)
    elif typecode == 14: return unicode(value.__repr__())
    elif typecode == 15: return unicode(value.__repr__())
    elif typecode == 16: return unicode(value)
    elif typecode == 17: return unicode(value)
    elif typecode == 18: return unicode(value)
    elif typecode == 19: return unicode(value)
    elif typecode == 20: return unicode(value.__repr__())
    elif typecode == 21: return unicode(value.__repr__())
    elif typecode == 22: return unicode(value.__repr__())
    else:
      raise ValueError ("Invalid type code: %d" % typecode)
    
  def _defaultValue(self, stype, broker=None, kwargs={}):
    """ """
    typecode = stype.type
    if   typecode == 1:  return 0
    elif typecode == 2:  return 0
    elif typecode == 3:  return 0
    elif typecode == 4:  return 0
    elif typecode == 6:  return ""
    elif typecode == 7:  return ""
    elif typecode == 8:  return 0
    elif typecode == 9:  return 0
    elif typecode == 10: return ObjectId(None)
    elif typecode == 11: return False
    elif typecode == 12: return 0.0
    elif typecode == 13: return 0.0
    elif typecode == 14: return UUID([0 for i in range(16)])
    elif typecode == 15: return {}
    elif typecode == 16: return 0
    elif typecode == 17: return 0
    elif typecode == 18: return 0
    elif typecode == 19: return 0
    elif typecode == 21: return []
    elif typecode == 22: return []
    elif typecode == 20:
      try:
        if "classKeys" in kwargs:
          keyList = kwargs["classKeys"]
        else:
          keyList = None
        classKey = self._bestClassKey(stype.refPackage, stype.refClass, keyList)
        if classKey:
          return self.makeObject(classKey, broker, kwargs)
      except:
        pass
      return None
    else:
      raise ValueError ("Invalid type code: %d" % typecode)

  def _bestClassKey(self, pname, cname, preferredList):
    """ """
    if pname == None or cname == None:
      if len(preferredList) == 0:
        return None
      return preferredList[0]
    for p in preferredList:
      if p.getPackageName() == pname and p.getClassName() == cname:
        return p
    clist = self.getClasses(pname)
    for c in clist:
      if c.getClassName() == cname:
        return c
    return None
    
  def _sendMethodRequest(self, broker, schemaKey, objectId, name, argList):
    """ This function can be used to send a method request to an object given only the
    broker, schemaKey, and objectId.  This is an uncommon usage pattern as methods are
    normally invoked on the object itself.
    """
    schema = self.getSchema(schemaKey)
    for method in schema.getMethods():
      if name == method.name:
        aIdx = 0
        sendCodec = Codec()
        seq = self.seqMgr._reserve((method, False))
        broker._setHeader(sendCodec, 'M', seq)
        objectId.encode(sendCodec)
        schemaKey.encode(sendCodec)
        sendCodec.write_str8(name)

        count = 0
        for arg in method.arguments:
          if arg.dir.find("I") != -1:
            count += 1
        if count != len(argList):
          raise Exception("Incorrect number of arguments: expected %d, got %d" % (count, len(argList)))

        for arg in method.arguments:
          if arg.dir.find("I") != -1:
            self._encodeValue(sendCodec, argList[aIdx], arg.type)
            aIdx += 1
        smsg = broker._message(sendCodec.encoded, "agent.%d.%d" %
                               (objectId.getBrokerBank(), objectId.getAgentBank()))
        broker._send(smsg)
        return seq
    return None

class Package:
  """ """
  def __init__(self, name):
    self.name = name

class ClassKey:
  """ A ClassKey uniquely identifies a class from the schema. """
  def __init__(self, constructor):
    if type(constructor) == str:
      # construct from __repr__ string
      try:
        self.pname, cls = constructor.split(":")
        self.cname, hsh = cls.split("(")
        hsh = hsh.strip(")")
        hexValues = hsh.split("-")
        h0 = int(hexValues[0], 16)
        h1 = int(hexValues[1], 16)
        h2 = int(hexValues[2], 16)
        h3 = int(hexValues[3], 16)
        self.hash = struct.pack("!LLLL", h0, h1, h2, h3)
      except:
        raise Exception("Invalid ClassKey format")
    else:
      # construct from codec
      codec = constructor
      self.pname = str(codec.read_str8())
      self.cname = str(codec.read_str8())
      self.hash  = codec.read_bin128()

  def encode(self, codec):
    codec.write_str8(self.pname)
    codec.write_str8(self.cname)
    codec.write_bin128(self.hash)

  def getPackageName(self):
    return self.pname

  def getClassName(self):
    return self.cname

  def getHash(self):
    return self.hash

  def getHashString(self):
    return "%08x-%08x-%08x-%08x" % struct.unpack ("!LLLL", self.hash)

  def getPackageKey(self):
    return (self.cname, self.hash)

  def __repr__(self):
    return self.pname + ":" + self.cname + "(" + self.getHashString() + ")"

class SchemaClass:
  """ """
  CLASS_KIND_TABLE = 1
  CLASS_KIND_EVENT = 2

  def __init__(self, kind, key, codec, session):
    self.kind = kind
    self.classKey = key
    self.properties = []
    self.statistics = []
    self.methods = []
    self.arguments = []
    self.session = session

    hasSupertype = 0  #codec.read_uint8()
    if self.kind == self.CLASS_KIND_TABLE:
      propCount   = codec.read_uint16()
      statCount   = codec.read_uint16()
      methodCount = codec.read_uint16()
      if hasSupertype == 1:
        self.superTypeKey = ClassKey(codec)
      else:
        self.superTypeKey = None ;
      for idx in range(propCount):
        self.properties.append(SchemaProperty(codec))
      for idx in range(statCount):
        self.statistics.append(SchemaStatistic(codec))
      for idx in range(methodCount):
        self.methods.append(SchemaMethod(codec))

    elif self.kind == self.CLASS_KIND_EVENT:
      argCount = codec.read_uint16()
      if (hasSupertype):
        self.superTypeKey = ClassKey(codec)
      else:
        self.superTypeKey = None ;      
      for idx in range(argCount):
        self.arguments.append(SchemaArgument(codec, methodArg=False))

  def __repr__(self):
    if self.kind == self.CLASS_KIND_TABLE:
      kindStr = "Table"
    elif self.kind == self.CLASS_KIND_EVENT:
      kindStr = "Event"
    else:
      kindStr = "Unsupported"
    result = "%s Class: %s " % (kindStr, self.classKey.__repr__())
    return result

  def getKey(self):
    """ Return the class-key for this class. """
    return self.classKey

  def getProperties(self):
    """ Return the list of properties for the class. """
    if (self.superTypeKey == None):
        return self.properties
    else:
        return self.properties + self.session.getSchema(self.superTypeKey).getProperties() 

  def getStatistics(self):
    """ Return the list of statistics for the class. """
    if (self.superTypeKey == None):
        return self.statistics
    else:
        return self.statistics + self.session.getSchema(self.superTypeKey).getStatistics()     

  def getMethods(self):
    """ Return the list of methods for the class. """
    if (self.superTypeKey == None):
        return self.methods
    else:
        return self.methods + self.session.getSchema(self.superTypeKey).getMethods()    

  def getArguments(self):
    """ Return the list of events for the class. """
    """ Return the list of methods for the class. """
    if (self.superTypeKey == None):
        return self.arguments
    else:
        return self.arguments + self.session.getSchema(self.superTypeKey).getArguments()  

class SchemaProperty:
  """ """
  def __init__(self, codec):
    map = codec.read_map()
    self.name     = str(map["name"])
    self.type     = map["type"]
    self.access   = str(map["access"])
    self.index    = map["index"] != 0
    self.optional = map["optional"] != 0
    self.refPackage = None
    self.refClass   = None
    self.unit       = None
    self.min        = None
    self.max        = None
    self.maxlen     = None
    self.desc       = None

    for key, value in map.items():
      if   key == "unit"       : self.unit = value
      elif key == "min"        : self.min = value
      elif key == "max"        : self.max = value
      elif key == "maxlen"     : self.maxlen = value
      elif key == "desc"       : self.desc = value
      elif key == "refPackage" : self.refPackage = value
      elif key == "refClass"   : self.refClass = value

  def __repr__(self):
    return self.name

class SchemaStatistic:
  """ """
  def __init__(self, codec):
    map = codec.read_map()
    self.name     = str(map["name"])
    self.type     = map["type"]
    self.unit     = None
    self.desc     = None

    for key, value in map.items():
      if   key == "unit" : self.unit = value
      elif key == "desc" : self.desc = value

  def __repr__(self):
    return self.name

class SchemaMethod:
  """ """
  def __init__(self, codec):
    map = codec.read_map()
    self.name = str(map["name"])
    argCount  = map["argCount"]
    if "desc" in map:
      self.desc = map["desc"]
    else:
      self.desc = None
    self.arguments = []

    for idx in range(argCount):
      self.arguments.append(SchemaArgument(codec, methodArg=True))

  def __repr__(self):
    result = self.name + "("
    first = True
    for arg in self.arguments:
      if arg.dir.find("I") != -1:
        if first:
          first = False
        else:
          result += ", "
        result += arg.name
    result += ")"
    return result

class SchemaArgument:
  """ """
  def __init__(self, codec, methodArg):
    map = codec.read_map()
    self.name    = str(map["name"])
    self.type    = map["type"]
    if methodArg:
      self.dir   = str(map["dir"]).upper()
    self.unit    = None
    self.min     = None
    self.max     = None
    self.maxlen  = None
    self.desc    = None
    self.default = None
    self.refPackage = None
    self.refClass   = None

    for key, value in map.items():
      if   key == "unit"    : self.unit    = value
      elif key == "min"     : self.min     = value
      elif key == "max"     : self.max     = value
      elif key == "maxlen"  : self.maxlen  = value
      elif key == "desc"    : self.desc    = value
      elif key == "default" : self.default = value
      elif key == "refPackage" : self.refPackage = value
      elif key == "refClass"   : self.refClass = value

class ObjectId:
  """ Object that represents QMF object identifiers """
  def __init__(self, codec, first=0, second=0):
    if codec:
      self.first  = codec.read_uint64()
      self.second = codec.read_uint64()
    else:
      self.first = first
      self.second = second

  def __cmp__(self, other):    
    if other == None or not isinstance(other, ObjectId) :
      return 1
    if self.first < other.first:
      return -1
    if self.first > other.first:
      return 1
    if self.second < other.second:
      return -1
    if self.second > other.second:
      return 1
    return 0

  def __repr__(self):
    return "%d-%d-%d-%d-%d" % (self.getFlags(), self.getSequence(),
                               self.getBrokerBank(), self.getAgentBank(), self.getObject())

  def index(self):
    return (self.first, self.second)

  def getFlags(self):
    return (self.first & 0xF000000000000000) >> 60

  def getSequence(self):
    return (self.first & 0x0FFF000000000000) >> 48

  def getBrokerBank(self):
    return (self.first & 0x0000FFFFF0000000) >> 28

  def getAgentBank(self):
    return self.first & 0x000000000FFFFFFF

  def getObject(self):
    return self.second

  def isDurable(self):
    return self.getSequence() == 0

  def encode(self, codec):
    codec.write_uint64(self.first)
    codec.write_uint64(self.second)

  def __hash__(self):
    return (self.first, self.second).__hash__()

  def __eq__(self, other):
    return (self.first, self.second).__eq__(other)

class MethodResult(object):
  """ """
  def __init__(self, status, text, outArgs):
    """ """
    self.status  = status
    self.text    = text
    self.outArgs = outArgs

  def __getattr__(self, name):
    if name in self.outArgs:
      return self.outArgs[name]

  def __repr__(self):
    return "%s (%d) - %s" % (self.text, self.status, self.outArgs)

class ManagedConnection(Thread):
  """ Thread class for managing a connection. """
  DELAY_MIN = 1
  DELAY_MAX = 128
  DELAY_FACTOR = 2

  def __init__(self, broker):
    Thread.__init__(self)
    self.broker = broker
    self.cv = Condition()
    self.canceled = False

  def stop(self):
    """ Tell this thread to stop running and return. """
    try:
      self.cv.acquire()
      self.canceled = True
      self.cv.notify()
    finally:
      self.cv.release()

  def disconnected(self):
    """ Notify the thread that the connection was lost. """
    try:
      self.cv.acquire()
      self.cv.notify()
    finally:
      self.cv.release()

  def run(self):
    """ Main body of the running thread. """
    delay = self.DELAY_MIN
    while True:
      try:
        self.broker._tryToConnect()
        try:
          self.cv.acquire()
          while (not self.canceled) and self.broker.connected:
            self.cv.wait()
          if self.canceled:
            return
          delay = self.DELAY_MIN
        finally:
          self.cv.release()
      except socket.error:
        if delay < self.DELAY_MAX:
          delay *= self.DELAY_FACTOR
      except SessionDetached:
        if delay < self.DELAY_MAX:
          delay *= self.DELAY_FACTOR
      except Closed:
        if delay < self.DELAY_MAX:
          delay *= self.DELAY_FACTOR

      try:
        self.cv.acquire()
        self.cv.wait(delay)
        if self.canceled:
          return
      finally:
        self.cv.release()

class Broker:
  """ This object represents a connection (or potential connection) to a QMF broker. """
  SYNC_TIME = 60
  nextSeq = 1

  def __init__(self, session, host, port, authMechs, authUser, authPass, ssl=False, connTimeout=None):
    self.session  = session
    self.host = host
    self.port = port
    self.mechanisms = authMechs
    self.ssl = ssl
    self.connTimeout = connTimeout
    self.authUser = authUser
    self.authPass = authPass
    self.cv = Condition()
    self.error = None
    self.brokerId = None
    self.connected = False
    self.amqpSessionId = "%s.%d.%d" % (platform.uname()[1], os.getpid(), Broker.nextSeq)
    Broker.nextSeq += 1
    if self.session.manageConnections:
      self.thread = ManagedConnection(self)
      self.thread.start()
    else:
      self.thread = None
      self._tryToConnect()

  def isConnected(self):
    """ Return True if there is an active connection to the broker. """
    return self.connected

  def getError(self):
    """ Return the last error message seen while trying to connect to the broker. """
    return self.error

  def getBrokerId(self):
    """ Get broker's unique identifier (UUID) """
    return self.brokerId

  def getBrokerBank(self):
    """ Return the broker-bank value.  This is the value that the broker assigns to
        objects within its control.  This value appears as a field in the ObjectId
        of objects created by agents controlled by this broker. """
    return 1

  def getAgent(self, brokerBank, agentBank):
    """ Return the agent object associated with a particular broker and agent bank value."""
    bankKey = (brokerBank, agentBank)
    if bankKey in self.agents:
      return self.agents[bankKey]
    return None

  def getSessionId(self):
    """ Get the identifier of the AMQP session to the broker """
    return self.amqpSessionId

  def getAgents(self):
    """ Get the list of agents reachable via this broker """
    return self.agents.values()

  def getAmqpSession(self):
    """ Get the AMQP session object for this connected broker. """
    return self.amqpSession

  def getUrl(self):
    """ """
    return "%s:%d" % (self.host, self.port)

  def getFullUrl(self, noAuthIfGuestDefault=True):
    """ """
    ssl = ""
    if self.ssl:
      ssl = "s"
    auth = "%s/%s@" % (self.authUser, self.authPass)
    if self.authUser == "" or \
          (noAuthIfGuestDefault and self.authUser == "guest" and self.authPass == "guest"):
      auth = ""
    return "amqp%s://%s%s:%d" % (ssl, auth, self.host, self.port or 5672)

  def __repr__(self):
    if self.connected:
      return "Broker connected at: %s" % self.getUrl()
    else:
      return "Disconnected Broker"

  def _tryToConnect(self):
    try:
      self.agents = {}
      self.agents[(1,0)] = Agent(self, 0, "BrokerAgent")
      self.topicBound = False
      self.syncInFlight = False
      self.syncRequest = 0
      self.syncResult = None
      self.reqsOutstanding = 1

      sock = connect(self.host, self.port)
      sock.settimeout(5)
      oldTimeout = sock.gettimeout()
      sock.settimeout(self.connTimeout)
      if self.ssl:
        connSock = ssl(sock)
      else:
        connSock = sock
      self.conn = Connection(connSock, username=self.authUser, password=self.authPass,
                             mechanism = self.mechanisms, host=self.host, service="qpidd")
      def aborted():
        raise Timeout("Waiting for connection to be established with broker")
      oldAborted = self.conn.aborted
      self.conn.aborted = aborted
      self.conn.start()
      sock.settimeout(oldTimeout)
      self.conn.aborted = oldAborted

      self.replyName = "reply-%s" % self.amqpSessionId
      self.amqpSession = self.conn.session(self.amqpSessionId)
      self.amqpSession.auto_sync = True
      self.amqpSession.queue_declare(queue=self.replyName, exclusive=True, auto_delete=True)
      self.amqpSession.exchange_bind(exchange="amq.direct",
                                     queue=self.replyName, binding_key=self.replyName)
      self.amqpSession.message_subscribe(queue=self.replyName, destination="rdest",
                                         accept_mode=self.amqpSession.accept_mode.none,
                                         acquire_mode=self.amqpSession.acquire_mode.pre_acquired)
      self.amqpSession.incoming("rdest").listen(self._replyCb, self._exceptionCb)
      self.amqpSession.message_set_flow_mode(destination="rdest", flow_mode=1)
      self.amqpSession.message_flow(destination="rdest", unit=0, value=0xFFFFFFFFL)
      self.amqpSession.message_flow(destination="rdest", unit=1, value=0xFFFFFFFFL)

      self.topicName = "topic-%s" % self.amqpSessionId
      self.amqpSession.queue_declare(queue=self.topicName, exclusive=True, auto_delete=True)
      self.amqpSession.message_subscribe(queue=self.topicName, destination="tdest",
                                         accept_mode=self.amqpSession.accept_mode.none,
                                         acquire_mode=self.amqpSession.acquire_mode.pre_acquired)
      self.amqpSession.incoming("tdest").listen(self._replyCb)
      self.amqpSession.message_set_flow_mode(destination="tdest", flow_mode=1)
      self.amqpSession.message_flow(destination="tdest", unit=0, value=0xFFFFFFFFL)
      self.amqpSession.message_flow(destination="tdest", unit=1, value=0xFFFFFFFFL)

      self.connected = True
      self.session._handleBrokerConnect(self)

      codec = Codec()
      self._setHeader(codec, 'B')
      msg = self._message(codec.encoded)
      self._send(msg)

    except socket.error, e:
      self.error = "Socket Error %s - %s" % (e.__class__.__name__, e)
      raise
    except Closed, e:
      self.error = "Connect Failed %s - %s" % (e.__class__.__name__, e)
      raise
    except ConnectionFailed, e:
      self.error = "Connect Failed %s - %s" % (e.__class__.__name__, e)
      raise

  def _updateAgent(self, obj):
    bankKey = (obj.brokerBank, obj.agentBank)
    if obj._deleteTime == 0:
      if bankKey not in self.agents:
        agent = Agent(self, obj.agentBank, obj.label)
        self.agents[bankKey] = agent
        if self.session.console != None:
          self.session.console.newAgent(agent)
    else:
      agent = self.agents.pop(bankKey, None)
      if agent != None and self.session.console != None:
        self.session.console.delAgent(agent)

  def _setHeader(self, codec, opcode, seq=0):
    """ Compose the header of a management message. """
    codec.write_uint8(ord('A'))
    codec.write_uint8(ord('M'))
    codec.write_uint8(ord('2'))
    codec.write_uint8(ord(opcode))
    codec.write_uint32(seq)

  def _checkHeader(self, codec):
    """ Check the header of a management message and extract the opcode and class. """
    try:
      octet = chr(codec.read_uint8())
      if octet != 'A':
        return None, None
      octet = chr(codec.read_uint8())
      if octet != 'M':
        return None, None
      octet = chr(codec.read_uint8())
      if octet != '2':
        return None, None
      opcode = chr(codec.read_uint8())
      seq    = codec.read_uint32()
      return opcode, seq
    except:
      return None, None

  def _message (self, body, routing_key="broker", ttl=None):
    dp = self.amqpSession.delivery_properties()
    dp.routing_key = routing_key
    if ttl:
      dp.ttl = ttl
    mp = self.amqpSession.message_properties()
    mp.content_type = "x-application/qmf"
    mp.user_id = self.authUser
    mp.reply_to = self.amqpSession.reply_to("amq.direct", self.replyName)
    return Message(dp, mp, body)

  def _send(self, msg, dest="qpid.management"):
    self.amqpSession.message_transfer(destination=dest, message=msg)

  def _shutdown(self):
    if self.thread:
      self.thread.stop()
      self.thread.join()
    if self.connected:
      self.amqpSession.incoming("rdest").stop()
      if self.session.console != None:
        self.amqpSession.incoming("tdest").stop()
      self.amqpSession.close()
      self.conn.close()
      self.connected = False

  def _waitForStable(self):
    try:
      self.cv.acquire()
      if not self.connected:
        return
      if self.reqsOutstanding == 0:
        return
      self.syncInFlight = True
      starttime = time()
      while self.reqsOutstanding != 0:
        self.cv.wait(self.SYNC_TIME)
        if time() - starttime > self.SYNC_TIME:
          raise RuntimeError("Timed out waiting for broker to synchronize")
    finally:
      self.cv.release()

  def _incOutstanding(self):
    try:
      self.cv.acquire()
      self.reqsOutstanding += 1
    finally:
      self.cv.release()

  def _decOutstanding(self):
    try:
      self.cv.acquire()
      self.reqsOutstanding -= 1
      if self.reqsOutstanding == 0 and not self.topicBound:
        self.topicBound = True
        for key in self.session.bindingKeyList:
          self.amqpSession.exchange_bind(exchange="qpid.management",
                                         queue=self.topicName, binding_key=key)
      if self.reqsOutstanding == 0 and self.syncInFlight:
        self.syncInFlight = False
        self.cv.notify()
    finally:
      self.cv.release()

  def _replyCb(self, msg):
    codec = Codec(msg.body)
    while True:
      opcode, seq = self._checkHeader(codec)
      if   opcode == None: return
      if   opcode == 'b': self.session._handleBrokerResp      (self, codec, seq)
      elif opcode == 'p': self.session._handlePackageInd      (self, codec, seq)
      elif opcode == 'z': self.session._handleCommandComplete (self, codec, seq)
      elif opcode == 'q': self.session._handleClassInd        (self, codec, seq)
      elif opcode == 'm': self.session._handleMethodResp      (self, codec, seq)
      elif opcode == 'h': self.session._handleHeartbeatInd    (self, codec, seq, msg)
      elif opcode == 'e': self.session._handleEventInd        (self, codec, seq)
      elif opcode == 's': self.session._handleSchemaResp      (self, codec, seq)
      elif opcode == 'c': self.session._handleContentInd      (self, codec, seq, prop=True)
      elif opcode == 'i': self.session._handleContentInd      (self, codec, seq, stat=True)
      elif opcode == 'g': self.session._handleContentInd      (self, codec, seq, prop=True, stat=True)

  def _exceptionCb(self, data):
    self.connected = False
    self.error = data
    try:
      self.cv.acquire()
      if self.syncInFlight:
        self.cv.notify()
    finally:
      self.cv.release()
    self.session._handleError(self.error)
    self.session._handleBrokerDisconnect(self)
    if self.thread:
      self.thread.disconnected()

class Agent:
  """ """
  def __init__(self, broker, agentBank, label):
    self.broker = broker
    self.brokerBank = broker.getBrokerBank()
    self.agentBank = agentBank
    self.label = label

  def __repr__(self):
    return "Agent at bank %d.%d (%s)" % (self.brokerBank, self.agentBank, self.label)

  def getBroker(self):
    return self.broker

  def getBrokerBank(self):
    return self.brokerBank

  def getAgentBank(self):
    return self.agentBank

class Event:
  """ """
  def __init__(self, session, broker, codec):
    self.session = session
    self.broker  = broker
    self.classKey = ClassKey(codec)
    self.timestamp = codec.read_int64()
    self.severity = codec.read_uint8()
    self.schema = None
    pname = self.classKey.getPackageName()
    pkey = self.classKey.getPackageKey()
    if pname in session.packages:
      if pkey in session.packages[pname]:
        self.schema = session.packages[pname][pkey]
        self.arguments = {}
        for arg in self.schema.arguments:
          self.arguments[arg.name] = session._decodeValue(codec, arg.type, broker)

  def __repr__(self):
    if self.schema == None:
      return "<uninterpretable>"
    out = strftime("%c", gmtime(self.timestamp / 1000000000))
    out += " " + self._sevName() + " " + self.classKey.getPackageName() + ":" + self.classKey.getClassName()
    out += " broker=" + self.broker.getUrl()
    for arg in self.schema.arguments:
      disp = self.session._displayValue(self.arguments[arg.name], arg.type).encode("utf8")
      if " " in disp:
        disp = "\"" + disp + "\""
      out += " " + arg.name + "=" + disp
    return out

  def _sevName(self):
    if self.severity == 0 : return "EMER "
    if self.severity == 1 : return "ALERT"
    if self.severity == 2 : return "CRIT "
    if self.severity == 3 : return "ERROR"
    if self.severity == 4 : return "WARN "
    if self.severity == 5 : return "NOTIC"
    if self.severity == 6 : return "INFO "
    if self.severity == 7 : return "DEBUG"
    return "INV-%d" % self.severity

  def getClassKey(self):
    return self.classKey

  def getArguments(self):
    return self.arguments

  def getTimestamp(self):
    return self.timestamp

  def getName(self):
    return self.name

  def getSchema(self):
    return self.schema

class SequenceManager:
  """ Manage sequence numbers for asynchronous method calls """
  def __init__(self):
    self.lock     = Lock()
    self.sequence = 0
    self.pending  = {}

  def _reserve(self, data):
    """ Reserve a unique sequence number """
    try:
      self.lock.acquire()
      result = self.sequence
      self.sequence = self.sequence + 1
      self.pending[result] = data
    finally:
      self.lock.release()
    return result

  def _release(self, seq):
    """ Release a reserved sequence number """
    data = None
    try:
      self.lock.acquire()
      if seq in self.pending:
        data = self.pending[seq]
        del self.pending[seq]
    finally:
      self.lock.release()
    return data


class DebugConsole(Console):
  """ """
  def brokerConnected(self, broker):
    print "brokerConnected:", broker

  def brokerDisconnected(self, broker):
    print "brokerDisconnected:", broker

  def newPackage(self, name):
    print "newPackage:", name

  def newClass(self, kind, classKey):
    print "newClass:", kind, classKey

  def newAgent(self, agent):
    print "newAgent:", agent

  def delAgent(self, agent):
    print "delAgent:", agent

  def objectProps(self, broker, record):
    print "objectProps:", record

  def objectStats(self, broker, record):
    print "objectStats:", record

  def event(self, broker, event):
    print "event:", event

  def heartbeat(self, agent, timestamp):
    print "heartbeat:", agent

  def brokerInfo(self, broker):
    print "brokerInfo:", broker

