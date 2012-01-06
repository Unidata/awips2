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

from threading import Condition, RLock, Lock, currentThread
from spec import SPEC
from generator import command_invoker
from datatypes import RangedSet, Struct, Future
from codec010 import StringCodec
from queue import Queue
from datatypes import Message, serial
from ops import Command, MessageTransfer
from util import wait, notify
from exceptions import *
from logging import getLogger

log = getLogger("qpid.io.cmd")
msg = getLogger("qpid.io.msg")

class SessionException(Exception): pass
class SessionClosed(SessionException): pass
class SessionDetached(SessionException): pass

def client(*args):
  return Client(*args)

def server(*args):
  return Server(*args)

INCOMPLETE = object()

class Session(command_invoker()):

  def __init__(self, name, auto_sync=True, timeout=10, delegate=client):
    self.name = name
    self.auto_sync = auto_sync
    self.need_sync = True
    self.timeout = timeout
    self.channel = None
    self.invoke_lock = Lock()
    self._closing = False
    self._closed = False

    self.condition = Condition()

    self.send_id = True
    self.receiver = Receiver(self)
    self.sender = Sender(self)

    self.lock = RLock()
    self._incoming = {}
    self.results = {}
    self.exceptions = []

    self.delegate = delegate(self)

  def incoming(self, destination):
    self.lock.acquire()
    try:
      queue = self._incoming.get(destination)
      if queue == None:
        queue = Incoming(self, destination)
        self._incoming[destination] = queue
      return queue
    finally:
      self.lock.release()

  def error(self):
    exc = self.exceptions[:]
    if len(exc) == 0:
      return None
    elif len(exc) == 1:
      return exc[0]
    else:
      return tuple(exc)

  def sync(self, timeout=None):
    ch = self.channel
    if ch is not None and currentThread() == ch.connection.thread:
      raise SessionException("deadlock detected")
    if self.need_sync:
      self.execution_sync(sync=True)
    last = self.sender.next_id - 1
    if not wait(self.condition, lambda:
                  last in self.sender._completed or self.exceptions,
                timeout):
      raise Timeout()
    if self.exceptions:
      raise SessionException(self.error())

  def close(self, timeout=None):
    self.invoke_lock.acquire()
    try:
      self._closing = True
      self.channel.session_detach(self.name)
    finally:
      self.invoke_lock.release()
    if not wait(self.condition, lambda: self._closed, timeout):
      raise Timeout()

  def closed(self):
    self.lock.acquire()
    try:
      if self._closed: return

      error = self.error()
      for id in self.results:
        f = self.results[id]
        f.error(error)
      self.results.clear()

      for q in self._incoming.values():
        q.close(error)

      self._closed = True
      notify(self.condition)
    finally:
      self.lock.release()

  def invoke(self, op, args, kwargs):
    if issubclass(op, Command):
      self.invoke_lock.acquire()
      try:
        return self.do_invoke(op, args, kwargs)
      finally:
        self.invoke_lock.release()
    else:
      return op(*args, **kwargs)

  def do_invoke(self, op, args, kwargs):
    if self._closing:
      raise SessionClosed()

    ch = self.channel
    if ch == None:
      raise SessionDetached()

    if op == MessageTransfer:
      if len(args) == len(op.FIELDS) + 1:
        message = args[-1]
        args = args[:-1]
      else:
        message = kwargs.pop("message", None)
      if message is not None:
        kwargs["headers"] = message.headers
        kwargs["payload"] = message.body

    cmd = op(*args, **kwargs)
    cmd.sync = self.auto_sync or cmd.sync
    self.need_sync = not cmd.sync
    cmd.channel = ch.id

    if op.RESULT:
      result = Future(exception=SessionException)
      self.results[self.sender.next_id] = result

    self.send(cmd)

    log.debug("SENT %s", cmd)
    if op == MessageTransfer:
      msg.debug("SENT %s", cmd)

    if op.RESULT:
      if self.auto_sync:
        return result.get(self.timeout)
      else:
        return result
    elif self.auto_sync:
      self.sync(self.timeout)

  def received(self, cmd):
    self.receiver.received(cmd)
    self.dispatch(cmd)

  def dispatch(self, cmd):
    log.debug("RECV %s", cmd)

    result = getattr(self.delegate, cmd.NAME)(cmd)
    if result is INCOMPLETE:
      return
    elif result is not None:
      self.execution_result(cmd.id, result)

    self.receiver.completed(cmd)
    # XXX: don't forget to obey sync for manual completion as well
    if cmd.sync:
      self.channel.session_completed(self.receiver._completed)

  def send(self, cmd):
    self.sender.send(cmd)

  def __repr__(self):
    return '<Session: %s, %s>' % (self.name, self.channel)

class Receiver:

  def __init__(self, session):
    self.session = session
    self.next_id = None
    self._completed = RangedSet()

  def received(self, cmd):
    if self.next_id == None:
      raise Exception("todo")
    cmd.id = self.next_id
    self.next_id += 1

  def completed(self, cmd):
    if cmd.id == None:
      raise ValueError("cannot complete unidentified command")
    self._completed.add(cmd.id)

  def known_completed(self, commands):
    completed = RangedSet()
    for c in self._completed.ranges:
      for kc in commands.ranges:
        if c.lower in kc and c.upper in kc:
          break
      else:
        completed.add_range(c)
    self._completed = completed

class Sender:

  def __init__(self, session):
    self.session = session
    self.next_id = serial(0)
    self.commands = []
    self._completed = RangedSet()

  def send(self, cmd):
    ch = self.session.channel
    if ch is None:
      raise SessionDetached()
    cmd.id = self.next_id
    self.next_id += 1
    if self.session.send_id:
      self.session.send_id = False
      ch.session_command_point(cmd.id, 0)
    self.commands.append(cmd)
    ch.connection.write_op(cmd)

  def completed(self, commands):
    idx = 0
    while idx < len(self.commands):
      cmd = self.commands[idx]
      if cmd.id in commands:
        del self.commands[idx]
      else:
        idx += 1
    for range in commands.ranges:
      self._completed.add(range.lower, range.upper)

class Incoming(Queue):

  def __init__(self, session, destination):
    Queue.__init__(self)
    self.session = session
    self.destination = destination

  def start(self):
    self.session.message_set_flow_mode(self.destination, self.session.flow_mode.credit)
    for unit in self.session.credit_unit.VALUES:
      self.session.message_flow(self.destination, unit, 0xFFFFFFFFL)

  def stop(self):
    self.session.message_cancel(self.destination)
    self.listen(None)

class Delegate:

  def __init__(self, session):
    self.session = session

  #XXX: do something with incoming accepts
  def message_accept(self, ma): None

  def execution_result(self, er):
    future = self.session.results.pop(er.command_id)
    future.set(er.value)

  def execution_exception(self, ex):
    self.session.exceptions.append(ex)

class Client(Delegate):

  def message_transfer(self, cmd):
    m = Message(cmd.payload)
    m.headers = cmd.headers
    m.id = cmd.id
    messages = self.session.incoming(cmd.destination)
    messages.put(m)
    msg.debug("RECV %s", m)
    return INCOMPLETE
