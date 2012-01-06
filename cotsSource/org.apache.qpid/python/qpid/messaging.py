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
A candidate high level messaging API for python.

Areas that still need work:

  - asynchronous send
  - asynchronous error notification
  - definition of the arguments for L{Session.sender} and L{Session.receiver}
  - standard L{Message} properties
  - L{Message} content encoding
  - protocol negotiation/multiprotocol impl
"""

from codec010 import StringCodec
from concurrency import synchronized, Waiter, Condition
from datatypes import timestamp, uuid4, Serial
from logging import getLogger
from ops import PRIMITIVE
from threading import Thread, RLock
from util import default

log = getLogger("qpid.messaging")

static = staticmethod

AMQP_PORT = 5672
AMQPS_PORT = 5671

class Constant:

  def __init__(self, name, value=None):
    self.name = name
    self.value = value

  def __repr__(self):
    return self.name

UNLIMITED = Constant("UNLIMITED", 0xFFFFFFFFL)

class ConnectionError(Exception):
  """
  The base class for all connection related exceptions.
  """
  pass

class ConnectError(ConnectionError):
  """
  Exception raised when there is an error connecting to the remote
  peer.
  """
  pass

class Connection:

  """
  A Connection manages a group of L{Sessions<Session>} and connects
  them with a remote endpoint.
  """

  @static
  def open(host, port=None, username="guest", password="guest",
           mechanism="PLAIN", heartbeat=None, **options):
    """
    Creates an AMQP connection and connects it to the given host and port.

    @type host: str
    @param host: the name or ip address of the remote host
    @type port: int
    @param port: the port number of the remote host
    @rtype: Connection
    @return: a connected Connection
    """
    conn = Connection(host, port, username, password, mechanism, heartbeat, **options)
    conn.connect()
    return conn

  def __init__(self, host, port=None, username="guest", password="guest",
               mechanism="PLAIN", heartbeat=None, **options):
    """
    Creates a connection. A newly created connection must be connected
    with the Connection.connect() method before it can be used.

    @type host: str
    @param host: the name or ip address of the remote host
    @type port: int
    @param port: the port number of the remote host
    @rtype: Connection
    @return: a disconnected Connection
    """
    self.host = host
    self.port = default(port, AMQP_PORT)
    self.username = username
    self.password = password
    self.mechanism = mechanism
    self.heartbeat = heartbeat

    self.id = str(uuid4())
    self.session_counter = 0
    self.sessions = {}
    self.reconnect = options.get("reconnect", False)
    self._connected = False
    self._lock = RLock()
    self._condition = Condition(self._lock)
    self._waiter = Waiter(self._condition)
    self._modcount = Serial(0)
    self.error = None
    from driver import Driver
    self._driver = Driver(self)
    self._driver.start()

  def _wait(self, predicate, timeout=None):
    return self._waiter.wait(predicate, timeout=timeout)

  def _wakeup(self):
    self._modcount += 1
    self._driver.wakeup()

  def _check_error(self, exc=ConnectionError):
    if self.error:
      raise exc(*self.error)

  def _ewait(self, predicate, timeout=None, exc=ConnectionError):
    result = self._wait(lambda: self.error or predicate(), timeout)
    self._check_error(exc)
    return result

  @synchronized
  def session(self, name=None, transactional=False):
    """
    Creates or retrieves the named session. If the name is omitted or
    None, then a unique name is chosen based on a randomly generated
    uuid.

    @type name: str
    @param name: the session name
    @rtype: Session
    @return: the named Session
    """

    if name is None:
      name = "%s:%s" % (self.id, self.session_counter)
      self.session_counter += 1
    else:
      name = "%s:%s" % (self.id, name)

    if self.sessions.has_key(name):
      return self.sessions[name]
    else:
      ssn = Session(self, name, transactional)
      self.sessions[name] = ssn
      self._wakeup()
      return ssn

  @synchronized
  def _remove_session(self, ssn):
    del self.sessions[ssn.name]

  @synchronized
  def connect(self):
    """
    Connect to the remote endpoint.
    """
    self._connected = True
    self._wakeup()
    self._ewait(lambda: self._driver._connected, exc=ConnectError)

  @synchronized
  def disconnect(self):
    """
    Disconnect from the remote endpoint.
    """
    self._connected = False
    self._wakeup()
    self._ewait(lambda: not self._driver._connected)

  @synchronized
  def connected(self):
    """
    Return true if the connection is connected, false otherwise.
    """
    return self._connected

  @synchronized
  def close(self):
    """
    Close the connection and all sessions.
    """
    for ssn in self.sessions.values():
      ssn.close()
    self.disconnect()

class Pattern:
  """
  The pattern filter matches the supplied wildcard pattern against a
  message subject.
  """

  def __init__(self, value):
    self.value = value

  # XXX: this should become part of the driver
  def _bind(self, sst, exchange, queue):
    from qpid.ops import ExchangeBind
    sst.write_cmd(ExchangeBind(exchange=exchange, queue=queue,
                               binding_key=self.value.replace("*", "#")))

class SessionError(Exception):
  pass

class Disconnected(SessionError):
  """
  Exception raised when an operation is attempted that is illegal when
  disconnected.
  """
  pass

class NontransactionalSession(SessionError):
  """
  Exception raised when commit or rollback is attempted on a non
  transactional session.
  """
  pass

class TransactionAborted(SessionError):
  pass

class Session:

  """
  Sessions provide a linear context for sending and receiving
  messages, and manage various Senders and Receivers.
  """

  def __init__(self, connection, name, transactional):
    self.connection = connection
    self.name = name

    self.transactional = transactional

    self.committing = False
    self.committed = True
    self.aborting = False
    self.aborted = False

    self.senders = []
    self.receivers = []
    self.outgoing = []
    self.incoming = []
    self.unacked = []
    self.acked = []
    # XXX: I hate this name.
    self.ack_capacity = UNLIMITED

    self.error = None
    self.closing = False
    self.closed = False

    self._lock = connection._lock

  def __repr__(self):
    return "<Session %s>" % self.name

  def _wait(self, predicate, timeout=None):
    return self.connection._wait(predicate, timeout=timeout)

  def _wakeup(self):
    self.connection._wakeup()

  def _check_error(self, exc=SessionError):
    self.connection._check_error(exc)
    if self.error:
      raise exc(*self.error)

  def _ewait(self, predicate, timeout=None, exc=SessionError):
    result = self.connection._ewait(lambda: self.error or predicate(), timeout, exc)
    self._check_error(exc)
    return result

  @synchronized
  def sender(self, target, **options):
    """
    Creates a L{Sender} that may be used to send L{Messages<Message>}
    to the specified target.

    @type target: str
    @param target: the target to which messages will be sent
    @rtype: Sender
    @return: a new Sender for the specified target
    """
    sender = Sender(self, len(self.senders), target, options)
    self.senders.append(sender)
    self._wakeup()
    # XXX: because of the lack of waiting here we can end up getting
    # into the driver loop with messages sent for senders that haven't
    # been linked yet, something similar can probably happen for
    # receivers
    return sender

  @synchronized
  def receiver(self, source, **options):
    """
    Creates a receiver that may be used to fetch L{Messages<Message>}
    from the specified source.

    @type source: str
    @param source: the source of L{Messages<Message>}
    @rtype: Receiver
    @return: a new Receiver for the specified source
    """
    receiver = Receiver(self, len(self.receivers), source, options)
    self.receivers.append(receiver)
    self._wakeup()
    return receiver

  @synchronized
  def _count(self, predicate):
    result = 0
    for msg in self.incoming:
      if predicate(msg):
        result += 1
    return result

  def _peek(self, predicate):
    for msg in self.incoming:
      if predicate(msg):
        return msg

  def _pop(self, predicate):
    i = 0
    while i < len(self.incoming):
      msg = self.incoming[i]
      if predicate(msg):
        del self.incoming[i]
        return msg
      else:
        i += 1

  @synchronized
  def _get(self, predicate, timeout=None):
    if self._ewait(lambda: ((self._peek(predicate) is not None) or self.closing),
                   timeout):
      msg = self._pop(predicate)
      if msg is not None:
        msg._receiver.returned += 1
        self.unacked.append(msg)
        log.debug("RETR [%s] %s", self, msg)
        return msg
    return None

  @synchronized
  def next_receiver(self, timeout=None):
    if self._ewait(lambda: self.incoming, timeout):
      return self.incoming[0]._receiver
    else:
      raise Empty

  @synchronized
  def acknowledge(self, message=None, sync=True):
    """
    Acknowledge the given L{Message}. If message is None, then all
    unacknowledged messages on the session are acknowledged.

    @type message: Message
    @param message: the message to acknowledge or None
    @type sync: boolean
    @param sync: if true then block until the message(s) are acknowledged
    """
    if message is None:
      messages = self.unacked[:]
    else:
      messages = [message]

    for m in messages:
      if self.ack_capacity is not UNLIMITED:
        if self.ack_capacity <= 0:
          # XXX: this is currently a SendError, maybe it should be a SessionError?
          raise InsufficientCapacity("ack_capacity = %s" % self.ack_capacity)
        self._wakeup()
        self._ewait(lambda: len(self.acked) < self.ack_capacity)
      self.unacked.remove(m)
      self.acked.append(m)

    self._wakeup()
    if sync:
      self._ewait(lambda: not [m for m in messages if m in self.acked])

  @synchronized
  def commit(self):
    """
    Commit outstanding transactional work. This consists of all
    message sends and receives since the prior commit or rollback.
    """
    if not self.transactional:
      raise NontransactionalSession()
    self.committing = True
    self._wakeup()
    self._ewait(lambda: not self.committing)
    if self.aborted:
      raise TransactionAborted()
    assert self.committed

  @synchronized
  def rollback(self):
    """
    Rollback outstanding transactional work. This consists of all
    message sends and receives since the prior commit or rollback.
    """
    if not self.transactional:
      raise NontransactionalSession()
    self.aborting = True
    self._wakeup()
    self._ewait(lambda: not self.aborting)
    assert self.aborted

  @synchronized
  def close(self):
    """
    Close the session.
    """
    # XXX: should be able to express this condition through API calls
    self._ewait(lambda: not self.outgoing and not self.acked)

    for link in self.receivers + self.senders:
      link.close()

    self.closing = True
    self._wakeup()
    self._ewait(lambda: self.closed)
    self.connection._remove_session(self)

class SendError(SessionError):
  pass

class InsufficientCapacity(SendError):
  pass

class Sender:

  """
  Sends outgoing messages.
  """

  def __init__(self, session, index, target, options):
    self.session = session
    self.index = index
    self.target = target
    self.options = options
    self.capacity = options.get("capacity", UNLIMITED)
    self.durable = options.get("durable")
    self.queued = Serial(0)
    self.acked = Serial(0)
    self.error = None
    self.linked = False
    self.closing = False
    self.closed = False
    self._lock = self.session._lock

  def _wakeup(self):
    self.session._wakeup()

  def _check_error(self, exc=SendError):
    self.session._check_error(exc)
    if self.error:
      raise exc(*self.error)

  def _ewait(self, predicate, timeout=None, exc=SendError):
    result = self.session._ewait(lambda: self.error or predicate(), timeout, exc)
    self._check_error(exc)
    return result

  @synchronized
  def pending(self):
    """
    Returns the number of messages awaiting acknowledgment.
    @rtype: int
    @return: the number of unacknowledged messages
    """
    return self.queued - self.acked

  @synchronized
  def send(self, object, sync=True, timeout=None):
    """
    Send a message. If the object passed in is of type L{unicode},
    L{str}, L{list}, or L{dict}, it will automatically be wrapped in a
    L{Message} and sent. If it is of type L{Message}, it will be sent
    directly. If the sender capacity is not L{UNLIMITED} then send
    will block until there is available capacity to send the message.
    If the timeout parameter is specified, then send will throw an
    L{InsufficientCapacity} exception if capacity does not become
    available within the specified time.

    @type object: unicode, str, list, dict, Message
    @param object: the message or content to send

    @type sync: boolean
    @param sync: if true then block until the message is sent

    @type timeout: float
    @param timeout: the time to wait for available capacity
    """

    if not self.session.connection._connected or self.session.closing:
      raise Disconnected()

    self._ewait(lambda: self.linked)

    if isinstance(object, Message):
      message = object
    else:
      message = Message(object)

    if message.durable is None:
      message.durable = self.durable

    if self.capacity is not UNLIMITED:
      if self.capacity <= 0:
        raise InsufficientCapacity("capacity = %s" % self.capacity)
      if not self._ewait(lambda: self.pending() < self.capacity, timeout=timeout):
        raise InsufficientCapacity("capacity = %s" % self.capacity)

    # XXX: what if we send the same message to multiple senders?
    message._sender = self
    self.session.outgoing.append(message)
    self.queued += 1

    self._wakeup()

    if sync:
      self.sync()
      assert message not in self.session.outgoing

  @synchronized
  def sync(self):
    mno = self.queued
    self._ewait(lambda: self.acked >= mno)

  @synchronized
  def close(self):
    """
    Close the Sender.
    """
    self.closing = True
    self._wakeup()
    try:
      self.session._ewait(lambda: self.closed)
    finally:
      self.session.senders.remove(self)

class ReceiveError(SessionError):
  pass

class Empty(ReceiveError):
  """
  Exception raised by L{Receiver.fetch} when there is no message
  available within the alloted time.
  """
  pass

class Receiver(object):

  """
  Receives incoming messages from a remote source. Messages may be
  fetched with L{fetch}.
  """

  def __init__(self, session, index, source, options):
    self.session = session
    self.index = index
    self.destination = str(self.index)
    self.source = source
    self.options = options

    self.granted = Serial(0)
    self.draining = False
    self.impending = Serial(0)
    self.received = Serial(0)
    self.returned = Serial(0)

    self.error = None
    self.linked = False
    self.closing = False
    self.closed = False
    self._lock = self.session._lock
    self._capacity = 0
    self._set_capacity(options.get("capacity", 0), False)

  @synchronized
  def _set_capacity(self, c, wakeup=True):
    if c is UNLIMITED:
      self._capacity = c.value
    else:
      self._capacity = c
    self._grant()
    if wakeup:
      self._wakeup()

  def _get_capacity(self):
    if self._capacity == UNLIMITED.value:
      return UNLIMITED
    else:
      return self._capacity

  capacity = property(_get_capacity, _set_capacity)

  def _wakeup(self):
    self.session._wakeup()

  def _check_error(self, exc=ReceiveError):
    self.session._check_error(exc)
    if self.error:
      raise exc(*self.error)

  def _ewait(self, predicate, timeout=None, exc=ReceiveError):
    result = self.session._ewait(lambda: self.error or predicate(), timeout, exc)
    self._check_error(exc)
    return result

  @synchronized
  def pending(self):
    """
    Returns the number of messages available to be fetched by the
    application.

    @rtype: int
    @return: the number of available messages
    """
    return self.received - self.returned

  def _pred(self, msg):
    return msg._receiver == self

  @synchronized
  def fetch(self, timeout=None):
    """
    Fetch and return a single message. A timeout of None will block
    forever waiting for a message to arrive, a timeout of zero will
    return immediately if no messages are available.

    @type timeout: float
    @param timeout: the time to wait for a message to be available
    """

    self._ewait(lambda: self.linked)

    if self._capacity == 0:
      self.granted = self.returned + 1
      self._wakeup()
    self._ewait(lambda: self.impending >= self.granted)
    msg = self.session._get(self._pred, timeout=timeout)
    if msg is None:
      self.draining = True
      self._wakeup()
      self._ewait(lambda: not self.draining)
      self._grant()
      self._wakeup()
      msg = self.session._get(self._pred, timeout=0)
      if msg is None:
        raise Empty()
    elif self._capacity not in (0, UNLIMITED.value):
      self.granted += 1
      self._wakeup()
    return msg

  def _grant(self):
    if self._capacity == UNLIMITED.value:
      self.granted = UNLIMITED
    else:
      self.granted = self.received + self._capacity

  @synchronized
  def close(self):
    """
    Close the receiver.
    """
    self.closing = True
    self._wakeup()
    try:
      self.session._ewait(lambda: self.closed)
    finally:
      self.session.receivers.remove(self)

def codec(name):
  type = PRIMITIVE[name]

  def encode(x):
    sc = StringCodec()
    sc.write_primitive(type, x)
    return sc.encoded

  def decode(x):
    sc = StringCodec(x)
    return sc.read_primitive(type)

  return encode, decode

# XXX: need to correctly parse the mime type and deal with
# content-encoding header

TYPE_MAPPINGS={
  dict: "amqp/map",
  list: "amqp/list",
  unicode: "text/plain; charset=utf8",
  unicode: "text/plain",
  buffer: None,
  str: None,
  None.__class__: None
  }

TYPE_CODEC={
  "amqp/map": codec("map"),
  "amqp/list": codec("list"),
  "text/plain; charset=utf8": (lambda x: x.encode("utf8"), lambda x: x.decode("utf8")),
  "text/plain": (lambda x: x.encode("utf8"), lambda x: x.decode("utf8")),
  "": (lambda x: x, lambda x: x),
  None: (lambda x: x, lambda x: x)
  }

def get_type(content):
  return TYPE_MAPPINGS[content.__class__]

def get_codec(content_type):
  return TYPE_CODEC[content_type]

UNSPECIFIED = object()

class Message:

  """
  A message consists of a standard set of fields, an application
  defined set of properties, and some content.

  @type id: str
  @ivar id: the message id
  @type user_id: ???
  @ivar user_id: the user-id of the message producer
  @type to: ???
  @ivar to: ???
  @type reply_to: ???
  @ivar reply_to: ???
  @type correlation_id: str
  @ivar correlation_id: a correlation-id for the message
  @type properties: dict
  @ivar properties: application specific message properties
  @type content_type: str
  @ivar content_type: the content-type of the message
  @type content: str, unicode, buffer, dict, list
  @ivar content: the message content
  """

  def __init__(self, content=None, content_type=UNSPECIFIED, id=None,
               subject=None, to=None, user_id=None, reply_to=None,
               correlation_id=None, durable=None, properties=None):
    """
    Construct a new message with the supplied content. The
    content-type of the message will be automatically inferred from
    type of the content parameter.

    @type content: str, unicode, buffer, dict, list
    @param content: the message content

    @type content_type: str
    @param content_type: the content-type of the message
    """
    self.id = id
    self.subject = subject
    self.to = to
    self.user_id = user_id
    self.reply_to = reply_to
    self.correlation_id = correlation_id
    self.durable = durable
    self.redelivered = False
    if properties is None:
      self.properties = {}
    else:
      self.properties = properties
    if content_type is UNSPECIFIED:
      self.content_type = get_type(content)
    else:
      self.content_type = content_type
    self.content = content

  def __repr__(self):
    args = []
    for name in ["id", "subject", "to", "user_id", "reply_to",
                 "correlation_id"]:
      value = self.__dict__[name]
      if value is not None: args.append("%s=%r" % (name, value))
    for name in ["durable", "properties"]:
      value = self.__dict__[name]
      if value: args.append("%s=%r" % (name, value))
    if self.content_type != get_type(self.content):
      args.append("content_type=%r" % self.content_type)
    if self.content is not None:
      if args:
        args.append("content=%r" % self.content)
      else:
        args.append(repr(self.content))
    return "Message(%s)" % ", ".join(args)

__all__ = ["Connection", "Session", "Sender", "Receiver", "Pattern", "Message",
           "ConnectionError", "ConnectError", "SessionError", "Disconnected",
           "SendError", "InsufficientCapacity", "ReceiveError", "Empty",
           "timestamp", "uuid4", "UNLIMITED", "AMQP_PORT", "AMQPS_PORT"]
