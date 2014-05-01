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

###############################################################################
## This file is being obsoleted by qmf/console.py
###############################################################################

"""
Management API for Qpid
"""

import qpid
import struct
import socket
from threading    import Thread
from datatypes    import Message, RangedSet
from time         import time
from cStringIO    import StringIO
from codec010     import StringCodec as Codec
from threading    import Lock, Condition


class SequenceManager:
  """ Manage sequence numbers for asynchronous method calls """
  def __init__ (self):
    self.lock     = Lock ()
    self.sequence = 0
    self.pending  = {}

  def reserve (self, data):
    """ Reserve a unique sequence number """
    self.lock.acquire ()
    result = self.sequence
    self.sequence = self.sequence + 1
    self.pending[result] = data
    self.lock.release ()
    return result

  def release (self, seq):
    """ Release a reserved sequence number """
    data = None
    self.lock.acquire ()
    if seq in self.pending:
      data = self.pending[seq]
      del self.pending[seq]
    self.lock.release ()
    return data


class mgmtObject (object):
  """ Generic object that holds the contents of a management object with its
      attributes set as object attributes. """

  def __init__ (self, classKey, timestamps, row):
    self.classKey   = classKey
    self.timestamps = timestamps
    for cell in row:
      setattr (self, cell[0], cell[1])

class objectId(object):
  """ Object that represents QMF object identifiers """

  def __init__(self, codec, first=0, second=0):
    if codec:
      self.first  = codec.read_uint64()
      self.second = codec.read_uint64()
    else:
      self.first = first
      self.second = second

  def __cmp__(self, other):
    if other == None:
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


  def index(self):
    return (self.first, self.second)

  def getFlags(self):
    return (self.first & 0xF000000000000000) >> 60

  def getSequence(self):
    return (self.first & 0x0FFF000000000000) >> 48

  def getBroker(self):
    return (self.first & 0x0000FFFFF0000000) >> 28

  def getBank(self):
    return self.first & 0x000000000FFFFFFF

  def getObject(self):
    return self.second

  def isDurable(self):
    return self.getSequence() == 0

  def encode(self, codec):
    codec.write_uint64(self.first)
    codec.write_uint64(self.second)


class methodResult:
  """ Object that contains the result of a method call """

  def __init__ (self, status, sText, args):
    self.status     = status
    self.statusText = sText
    for arg in args:
      setattr (self, arg, args[arg])

class brokerInfo:
  """ Object that contains information about a broker and the session to it """

  def __init__ (self, brokerId, sessionId):
    self.brokerId = brokerId
    self.sessionId = sessionId

class managementChannel:
  """ This class represents a connection to an AMQP broker. """

  def __init__ (self, ssn, topicCb, replyCb, exceptionCb, cbContext, _detlife=0):
    """ Given a channel on an established AMQP broker connection, this method
    opens a session and performs all of the declarations and bindings needed
    to participate in the management protocol. """
    self.enabled     = True
    self.ssn         = ssn
    self.sessionId   = ssn.name
    self.topicName   = "mgmt-%s" % self.sessionId
    self.replyName   = "repl-%s" % self.sessionId
    self.qpidChannel = ssn
    self.tcb         = topicCb
    self.rcb         = replyCb
    self.ecb         = exceptionCb
    self.context     = cbContext
    self.reqsOutstanding = 0
    self.brokerInfo  = None

    ssn.auto_sync = False
    ssn.queue_declare (queue=self.topicName, exclusive=True, auto_delete=True)
    ssn.queue_declare (queue=self.replyName, exclusive=True, auto_delete=True)

    ssn.exchange_bind (exchange="amq.direct",
                       queue=self.replyName, binding_key=self.replyName)
    ssn.message_subscribe (queue=self.topicName, destination="tdest",
                           accept_mode=ssn.accept_mode.none,
                           acquire_mode=ssn.acquire_mode.pre_acquired)
    ssn.message_subscribe (queue=self.replyName, destination="rdest",
                           accept_mode=ssn.accept_mode.none,
                           acquire_mode=ssn.acquire_mode.pre_acquired)

    ssn.incoming ("tdest").listen (self.topicCb, self.exceptionCb)
    ssn.incoming ("rdest").listen (self.replyCb)

    ssn.message_set_flow_mode (destination="tdest", flow_mode=1)
    ssn.message_flow (destination="tdest", unit=0, value=0xFFFFFFFFL)
    ssn.message_flow (destination="tdest", unit=1, value=0xFFFFFFFFL)

    ssn.message_set_flow_mode (destination="rdest", flow_mode=1)
    ssn.message_flow (destination="rdest", unit=0, value=0xFFFFFFFFL)
    ssn.message_flow (destination="rdest", unit=1, value=0xFFFFFFFFL)

  def setBrokerInfo (self, data):
    self.brokerInfo = data

  def shutdown (self):
    self.enabled = False
    self.ssn.incoming("tdest").stop()
    self.ssn.incoming("rdest").stop()

  def topicCb (self, msg):
    """ Receive messages via the topic queue on this channel. """
    if self.enabled:
      self.tcb (self, msg)

  def replyCb (self, msg):
    """ Receive messages via the reply queue on this channel. """
    if self.enabled:
      self.rcb (self, msg)

  def exceptionCb (self, data):
    if self.ecb != None:
      self.ecb (self, data)

  def send (self, exchange, msg):
    if self.enabled:
      self.qpidChannel.message_transfer (destination=exchange, message=msg)

  def message (self, body, routing_key="broker"):
    dp = self.qpidChannel.delivery_properties()
    dp.routing_key  = routing_key
    mp = self.qpidChannel.message_properties()
    mp.content_type = "application/octet-stream"
    mp.reply_to     = self.qpidChannel.reply_to("amq.direct", self.replyName)
    return Message(dp, mp, body)


class managementClient:
  """ This class provides an API for access to management data on the AMQP
  network.  It implements the management protocol and manages the management
  schemas as advertised by the various management agents in the network. """

  CTRL_BROKER_INFO   = 1
  CTRL_SCHEMA_LOADED = 2
  CTRL_USER          = 3
  CTRL_HEARTBEAT     = 4

  SYNC_TIME = 10.0

  #========================================================
  # User API - interacts with the class's user
  #========================================================
  def __init__ (self, unused=None, ctrlCb=None, configCb=None, instCb=None, methodCb=None, closeCb=None):
    self.ctrlCb   = ctrlCb
    self.configCb = configCb
    self.instCb   = instCb
    self.methodCb = methodCb
    self.closeCb  = closeCb
    self.schemaCb = None
    self.eventCb  = None
    self.channels = []
    self.seqMgr   = SequenceManager ()
    self.schema   = {}
    self.packages = {}
    self.cv       = Condition ()
    self.syncInFlight = False
    self.syncSequence = 0
    self.syncResult   = None

  def schemaListener (self, schemaCb):
    """ Optionally register a callback to receive details of the schema of
    managed objects in the network. """
    self.schemaCb = schemaCb

  def eventListener (self, eventCb):
    """ Optionally register a callback to receive events from managed objects
    in the network. """
    self.eventCb = eventCb

  def addChannel (self, channel, cbContext=None):
    """ Register a new channel. """
    mch = managementChannel (channel, self.topicCb, self.replyCb, self.exceptCb, cbContext)

    self.channels.append (mch)
    self.incOutstanding (mch)
    codec = Codec ()
    self.setHeader (codec, ord ('B'))
    msg = mch.message(codec.encoded)
    mch.send ("qpid.management", msg)
    return mch

  def removeChannel (self, mch):
    """ Remove a previously added channel from management. """
    mch.shutdown ()
    self.channels.remove (mch)

  def callMethod (self, channel, userSequence, objId, className, methodName, args=None):
    """ Invoke a method on a managed object. """
    self.method (channel, userSequence, objId, className, methodName, args)

  def getObjects (self, channel, userSequence, className, bank=0):
    """ Request immediate content from broker """
    codec = Codec ()
    self.setHeader (codec, ord ('G'), userSequence)
    ft = {}
    ft["_class"] = className
    codec.write_map (ft)
    msg = channel.message(codec.encoded, routing_key="agent.1.%d" % bank)
    channel.send ("qpid.management", msg)

  def syncWaitForStable (self, channel):
    """ Synchronous (blocking) call to wait for schema stability on a channel """
    self.cv.acquire ()
    if channel.reqsOutstanding == 0:
      self.cv.release ()
      return channel.brokerInfo

    self.syncInFlight = True
    starttime = time ()
    while channel.reqsOutstanding != 0:
      self.cv.wait (self.SYNC_TIME)
      if time () - starttime > self.SYNC_TIME:
        self.cv.release ()
        raise RuntimeError ("Timed out waiting for response on channel")
    self.cv.release ()
    return channel.brokerInfo

  def syncCallMethod (self, channel, objId, className, methodName, args=None):
    """ Synchronous (blocking) method call """
    self.cv.acquire ()
    self.syncInFlight = True
    self.syncResult   = None
    self.syncSequence = self.seqMgr.reserve ("sync")
    self.cv.release ()
    self.callMethod (channel, self.syncSequence, objId, className, methodName, args)
    self.cv.acquire ()
    starttime = time ()
    while self.syncInFlight:
      self.cv.wait (self.SYNC_TIME)
      if time () - starttime > self.SYNC_TIME:
        self.cv.release ()
        raise RuntimeError ("Timed out waiting for response on channel")
    result = self.syncResult
    self.cv.release ()
    return result

  def syncGetObjects (self, channel, className, bank=0):
    """ Synchronous (blocking) get call """
    self.cv.acquire ()
    self.syncInFlight = True
    self.syncResult   = []
    self.syncSequence = self.seqMgr.reserve ("sync")
    self.cv.release ()
    self.getObjects (channel, self.syncSequence, className, bank)
    self.cv.acquire ()
    starttime = time ()
    while self.syncInFlight:
      self.cv.wait (self.SYNC_TIME)
      if time () - starttime > self.SYNC_TIME:
        self.cv.release ()
        raise RuntimeError ("Timed out waiting for response on channel")
    result = self.syncResult
    self.cv.release ()
    return result

  #========================================================
  # Channel API - interacts with registered channel objects
  #========================================================
  def topicCb (self, ch, msg):
    """ Receive messages via the topic queue of a particular channel. """
    codec = Codec (msg.body)
    while True:
      hdr = self.checkHeader (codec)
      if hdr == None:
        return

      if hdr[0] == 'p':
        self.handlePackageInd (ch, codec)
      elif hdr[0] == 'q':
        self.handleClassInd (ch, codec)
      elif hdr[0] == 'h':
        self.handleHeartbeat (ch, codec)
      elif hdr[0] == 'e':
        self.handleEvent (ch, codec)
      else:
        self.parse (ch, codec, hdr[0], hdr[1])

  def replyCb (self, ch, msg):
    """ Receive messages via the reply queue of a particular channel. """
    codec = Codec (msg.body)
    hdr   = self.checkHeader (codec)
    if hdr == None:
      return

    if   hdr[0] == 'm':
      self.handleMethodReply (ch, codec, hdr[1])
    elif hdr[0] == 'z':
      self.handleCommandComplete (ch, codec, hdr[1])
    elif hdr[0] == 'b':
      self.handleBrokerResponse (ch, codec)
    elif hdr[0] == 'p':
      self.handlePackageInd (ch, codec)
    elif hdr[0] == 'q':
      self.handleClassInd (ch, codec)
    else:
      self.parse (ch, codec, hdr[0], hdr[1])

  def exceptCb (self, ch, data):
    if self.closeCb != None:
      self.closeCb (ch.context, data)

  #========================================================
  # Internal Functions
  #========================================================
  def setHeader (self, codec, opcode, seq = 0):
    """ Compose the header of a management message. """
    codec.write_uint8 (ord ('A'))
    codec.write_uint8 (ord ('M'))
    codec.write_uint8 (ord ('2'))
    codec.write_uint8 (opcode)
    codec.write_uint32  (seq)

  def checkHeader (self, codec):
    """ Check the header of a management message and extract the opcode and class. """
    try:
      octet = chr (codec.read_uint8 ())
      if octet != 'A':
        return None
      octet = chr (codec.read_uint8 ())
      if octet != 'M':
        return None
      octet = chr (codec.read_uint8 ())
      if octet != '2':
        return None
      opcode = chr (codec.read_uint8 ())
      seq    = codec.read_uint32 ()
      return (opcode, seq)
    except:
      return None

  def encodeValue (self, codec, value, typecode):
    """ Encode, into the codec, a value based on its typecode. """
    if   typecode == 1:
      codec.write_uint8  (int  (value))
    elif typecode == 2:
      codec.write_uint16 (int  (value))
    elif typecode == 3:
      codec.write_uint32 (long (value))
    elif typecode == 4:
      codec.write_uint64 (long (value))
    elif typecode == 5:
      codec.write_uint8  (int  (value))
    elif typecode == 6:
      codec.write_str8   (value)
    elif typecode == 7:
      codec.write_str16 (value)
    elif typecode == 8:  # ABSTIME
      codec.write_uint64 (long (value))
    elif typecode == 9:  # DELTATIME
      codec.write_uint64 (long (value))
    elif typecode == 10: # REF
      value.encode(codec)
    elif typecode == 11: # BOOL
      codec.write_uint8  (int  (value))
    elif typecode == 12: # FLOAT
      codec.write_float  (float (value))
    elif typecode == 13: # DOUBLE
      codec.write_double (float (value))
    elif typecode == 14: # UUID
      codec.write_uuid   (value)
    elif typecode == 15: # FTABLE
      codec.write_map    (value)
    elif typecode == 16:
      codec.write_int8 (int(value))
    elif typecode == 17:
      codec.write_int16 (int(value))
    elif typecode == 18:
      codec.write_int32 (int(value))
    elif typecode == 19:
      codec.write_int64 (int(value))
    else:
      raise ValueError ("Invalid type code: %d" % typecode)

  def decodeValue (self, codec, typecode):
    """ Decode, from the codec, a value based on its typecode. """
    if   typecode == 1:
      data = codec.read_uint8 ()
    elif typecode == 2:
      data = codec.read_uint16 ()
    elif typecode == 3:
      data = codec.read_uint32 ()
    elif typecode == 4:
      data = codec.read_uint64 ()
    elif typecode == 5:
      data = codec.read_uint8 ()
    elif typecode == 6:
      data = codec.read_str8 ()
    elif typecode == 7:
      data = codec.read_str16 ()
    elif typecode == 8:  # ABSTIME
      data = codec.read_uint64 ()
    elif typecode == 9:  # DELTATIME
      data = codec.read_uint64 ()
    elif typecode == 10: # REF
      data = objectId(codec)
    elif typecode == 11: # BOOL
      data = codec.read_uint8 ()
    elif typecode == 12: # FLOAT
      data = codec.read_float ()
    elif typecode == 13: # DOUBLE
      data = codec.read_double ()
    elif typecode == 14: # UUID
      data = codec.read_uuid ()
    elif typecode == 15: # FTABLE
      data = codec.read_map ()
    elif typecode == 16:
      data = codec.read_int8 ()
    elif typecode == 17:
      data = codec.read_int16 ()
    elif typecode == 18:
      data = codec.read_int32 ()
    elif typecode == 19:
      data = codec.read_int64 ()
    else:
      raise ValueError ("Invalid type code: %d" % typecode)
    return data

  def incOutstanding (self, ch):
    self.cv.acquire ()
    ch.reqsOutstanding = ch.reqsOutstanding + 1
    self.cv.release ()

  def decOutstanding (self, ch):
    self.cv.acquire ()
    ch.reqsOutstanding = ch.reqsOutstanding - 1
    if ch.reqsOutstanding == 0 and self.syncInFlight:
      self.syncInFlight = False
      self.cv.notify ()
    self.cv.release ()

    if ch.reqsOutstanding == 0:
      if self.ctrlCb != None:
        self.ctrlCb (ch.context, self.CTRL_SCHEMA_LOADED, None)
      ch.ssn.exchange_bind (exchange="qpid.management",
                            queue=ch.topicName, binding_key="console.#")
      ch.ssn.exchange_bind (exchange="qpid.management",
                            queue=ch.topicName, binding_key="schema.#")


  def handleMethodReply (self, ch, codec, sequence):
    status = codec.read_uint32 ()
    sText  = codec.read_str16 ()

    data = self.seqMgr.release (sequence)
    if data == None:
      return

    (userSequence, classId, methodName) = data
    args = {}
    context = self.seqMgr.release (userSequence)

    if status == 0:
      schemaClass = self.schema[classId]
      ms = schemaClass['M']
      arglist = None
      for mname in ms:
        (mdesc, margs) = ms[mname]
        if mname == methodName:
          arglist = margs
      if arglist == None:
        return

      for arg in arglist:
        if arg[2].find("O") != -1:
          args[arg[0]] = self.decodeValue (codec, arg[1])

    if context == "sync" and userSequence == self.syncSequence:
      self.cv.acquire ()
      self.syncInFlight = False
      self.syncResult   = methodResult (status, sText, args)
      self.cv.notify  ()
      self.cv.release ()
    elif self.methodCb != None:
      self.methodCb (ch.context, userSequence, status, sText, args)

  def handleCommandComplete (self, ch, codec, seq):
    code = codec.read_uint32 ()
    text = codec.read_str8 ()
    data = (seq, code, text)
    context = self.seqMgr.release (seq)
    if context == "outstanding":
      self.decOutstanding (ch)
    elif context == "sync" and seq == self.syncSequence:
      self.cv.acquire ()
      self.syncInFlight = False
      self.cv.notify  ()
      self.cv.release ()
    elif self.ctrlCb != None:
      self.ctrlCb (ch.context, self.CTRL_USER, data)

  def handleBrokerResponse (self, ch, codec):
    uuid = codec.read_uuid ()
    ch.brokerInfo = brokerInfo (uuid, ch.sessionId)
    if self.ctrlCb != None:
      self.ctrlCb (ch.context, self.CTRL_BROKER_INFO, ch.brokerInfo)

    # Send a package request
    sendCodec = Codec ()
    seq = self.seqMgr.reserve ("outstanding")
    self.setHeader (sendCodec, ord ('P'), seq)
    smsg = ch.message(sendCodec.encoded)
    ch.send ("qpid.management", smsg)

  def handlePackageInd (self, ch, codec):
    pname = codec.read_str8 ()
    if pname not in self.packages:
      self.packages[pname] = {}

      # Send a class request
      sendCodec = Codec ()
      seq = self.seqMgr.reserve ("outstanding")
      self.setHeader (sendCodec, ord ('Q'), seq)
      self.incOutstanding (ch)
      sendCodec.write_str8 (pname)
      smsg = ch.message(sendCodec.encoded)
      ch.send ("qpid.management", smsg)

  def handleClassInd (self, ch, codec):
    kind  = codec.read_uint8()
    if kind != 1:  # This API doesn't handle new-style events
      return
    pname = codec.read_str8()
    cname = codec.read_str8()
    hash  = codec.read_bin128()
    if pname not in self.packages:
      return

    if (cname, hash) not in self.packages[pname]:
      # Send a schema request
      sendCodec = Codec ()
      seq = self.seqMgr.reserve ("outstanding")
      self.setHeader (sendCodec, ord ('S'), seq)
      self.incOutstanding (ch)
      sendCodec.write_str8 (pname)
      sendCodec.write_str8 (cname)
      sendCodec.write_bin128   (hash)
      smsg = ch.message(sendCodec.encoded)
      ch.send ("qpid.management", smsg)

  def handleHeartbeat (self, ch, codec):
    timestamp = codec.read_uint64()
    if self.ctrlCb != None:
      self.ctrlCb (ch.context, self.CTRL_HEARTBEAT, timestamp)

  def handleEvent (self, ch, codec):
    if self.eventCb == None:
      return
    timestamp = codec.read_uint64()
    objId = objectId(codec)
    packageName = codec.read_str8()
    className   = codec.read_str8()
    hash        = codec.read_bin128()
    name        = codec.read_str8()
    classKey    = (packageName, className, hash)
    if classKey not in self.schema:
      return;
    schemaClass = self.schema[classKey]
    row = []
    es = schemaClass['E']
    arglist = None
    for ename in es:
      (edesc, eargs) = es[ename]
      if ename == name:
        arglist = eargs
    if arglist == None:
      return
    for arg in arglist:
      row.append((arg[0], self.decodeValue(codec, arg[1])))
    self.eventCb(ch.context, classKey, objId, name, row)

  def parseSchema (self, ch, codec):
    """ Parse a received schema-description message. """
    self.decOutstanding (ch)
    kind  = codec.read_uint8()
    if kind != 1:  # This API doesn't handle new-style events
      return
    packageName = codec.read_str8 ()
    className   = codec.read_str8 ()
    hash        = codec.read_bin128 ()
    hasSupertype = 0 #codec.read_uint8()
    configCount = codec.read_uint16 ()
    instCount   = codec.read_uint16 ()
    methodCount = codec.read_uint16 ()
    if hasSupertype != 0:
      supertypePackage = codec.read_str8()
      supertypeClass   = codec.read_str8()
      supertypeHash    = codec.read_bin128()

    if packageName not in self.packages:
      return
    if (className, hash) in self.packages[packageName]:
      return

    classKey = (packageName, className, hash)
    if classKey in self.schema:
      return

    configs = []
    insts   = []
    methods = {}

    configs.append (("id", 4, "", "", 1, 1, None, None, None, None, None))
    insts.append   (("id", 4, None, None))

    for idx in range (configCount):
      ft = codec.read_map ()
      name     = str (ft["name"])
      type     = ft["type"]
      access   = ft["access"]
      index    = ft["index"]
      optional = ft["optional"]
      unit     = None
      min      = None
      max      = None
      maxlen   = None
      desc     = None

      for key, value in ft.items ():
        if   key == "unit":
          unit = str (value)
        elif key == "min":
          min = value
        elif key == "max":
          max = value
        elif key == "maxlen":
          maxlen = value
        elif key == "desc":
          desc = str (value)

      config = (name, type, unit, desc, access, index, min, max, maxlen, optional)
      configs.append (config)

    for idx in range (instCount):
      ft = codec.read_map ()
      name   = str (ft["name"])
      type   = ft["type"]
      unit   = None
      desc   = None

      for key, value in ft.items ():
        if   key == "unit":
          unit = str (value)
        elif key == "desc":
          desc = str (value)

      inst = (name, type, unit, desc)
      insts.append (inst)

    for idx in range (methodCount):
      ft = codec.read_map ()
      mname    = str (ft["name"])
      argCount = ft["argCount"]
      if "desc" in ft:
        mdesc = str (ft["desc"])
      else:
        mdesc = None

      args = []
      for aidx in range (argCount):
        ft = codec.read_map ()
        name    = str (ft["name"])
        type    = ft["type"]
        dir     = str (ft["dir"].upper ())
        unit    = None
        min     = None
        max     = None
        maxlen  = None
        desc    = None
        default = None

        for key, value in ft.items ():
          if   key == "unit":
            unit = str (value)
          elif key == "min":
            min = value
          elif key == "max":
            max = value
          elif key == "maxlen":
            maxlen = value
          elif key == "desc":
            desc = str (value)
          elif key == "default":
            default = str (value)

        arg = (name, type, dir, unit, desc, min, max, maxlen, default)
        args.append (arg)
      methods[mname] = (mdesc, args)

    schemaClass = {}
    schemaClass['C'] = configs
    schemaClass['I'] = insts
    schemaClass['M'] = methods
    self.schema[classKey] = schemaClass

    if self.schemaCb != None:
      self.schemaCb (ch.context, classKey, configs, insts, methods, {})

  def parsePresenceMasks(self, codec, schemaClass):
    """ Generate a list of not-present properties """
    excludeList = []
    bit = 0
    for element in schemaClass['C'][1:]:
      if element[9] == 1:
        if bit == 0:
          mask = codec.read_uint8()
          bit  = 1
        if (mask & bit) == 0:
          excludeList.append(element[0])
        bit = bit * 2
        if bit == 256:
          bit = 0
    return excludeList

  def parseContent (self, ch, cls, codec, seq=0):
    """ Parse a received content message. """
    if (cls == 'C' or (cls == 'B' and seq == 0)) and self.configCb == None:
      return
    if cls == 'I' and self.instCb == None:
      return

    packageName = codec.read_str8 ()
    className   = codec.read_str8 ()
    hash        = codec.read_bin128 ()
    classKey    = (packageName, className, hash)

    if classKey not in self.schema:
      return

    row        = []
    timestamps = []

    timestamps.append (codec.read_uint64 ())  # Current Time
    timestamps.append (codec.read_uint64 ())  # Create Time
    timestamps.append (codec.read_uint64 ())  # Delete Time
    objId = objectId(codec)
    schemaClass = self.schema[classKey]
    if cls == 'C' or cls == 'B':
      notPresent = self.parsePresenceMasks(codec, schemaClass)

    if cls == 'C' or cls == 'B':
      row.append(("id", objId))
      for element in schemaClass['C'][1:]:
        tc   = element[1]
        name = element[0]
        if name in notPresent:
          row.append((name, None))
        else:
          data = self.decodeValue(codec, tc)
          row.append((name, data))

    if cls == 'I' or cls == 'B':
      if cls == 'I':
        row.append(("id", objId))
      for element in schemaClass['I'][1:]:
        tc   = element[1]
        name = element[0]
        data = self.decodeValue (codec, tc)
        row.append ((name, data))

    if   cls == 'C' or (cls == 'B' and seq != self.syncSequence):
      self.configCb (ch.context, classKey, row, timestamps)
    elif cls == 'B' and seq == self.syncSequence:
      if timestamps[2] == 0:
        obj = mgmtObject (classKey, timestamps, row)
        self.syncResult.append (obj)
    elif cls == 'I':
      self.instCb   (ch.context, classKey, row, timestamps)

  def parse (self, ch, codec, opcode, seq):
    """ Parse a message received from the topic queue. """
    if opcode   == 's':
      self.parseSchema  (ch, codec)
    elif opcode == 'c':
      self.parseContent (ch, 'C', codec)
    elif opcode == 'i':
      self.parseContent (ch, 'I', codec)
    elif opcode == 'g':
      self.parseContent (ch, 'B', codec, seq)
    else:
      raise ValueError ("Unknown opcode: %c" % opcode);

  def method (self, channel, userSequence, objId, classId, methodName, args):
    """ Invoke a method on an object """
    codec = Codec ()
    sequence = self.seqMgr.reserve ((userSequence, classId, methodName))
    self.setHeader (codec, ord ('M'), sequence)
    objId.encode(codec)
    codec.write_str8 (classId[0])
    codec.write_str8 (classId[1])
    codec.write_bin128 (classId[2])
    codec.write_str8 (methodName)
    bank = "%d.%d" % (objId.getBroker(), objId.getBank())

    # Encode args according to schema
    if classId not in self.schema:
      self.seqMgr.release (sequence)
      raise ValueError ("Unknown class name: %s" % classId)
    
    schemaClass = self.schema[classId]
    ms          = schemaClass['M']
    arglist     = None
    for mname in ms:
      (mdesc, margs) = ms[mname]
      if mname == methodName:
        arglist = margs
    if arglist == None:
      self.seqMgr.release (sequence)
      raise ValueError ("Unknown method name: %s" % methodName)

    for arg in arglist:
      if arg[2].find("I") != -1:
        value = arg[8]  # default
        if arg[0] in args:
          value = args[arg[0]]
          if value == None:
            self.seqMgr.release (sequence)
            raise ValueError ("Missing non-defaulted argument: %s" % arg[0])
          self.encodeValue (codec, value, arg[1])

    packageName = classId[0]
    className   = classId[1]
    msg = channel.message(codec.encoded, "agent." + bank)
    channel.send ("qpid.management", msg)
