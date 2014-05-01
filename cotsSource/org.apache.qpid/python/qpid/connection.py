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

import datatypes, session
from threading import Thread, Condition, RLock
from util import wait, notify
from codec010 import StringCodec
from framing import *
from session import Session
from generator import control_invoker
from spec import SPEC
from exceptions import *
from logging import getLogger
import delegates, socket

class ChannelBusy(Exception): pass

class ChannelsBusy(Exception): pass

class SessionBusy(Exception): pass

class ConnectionFailed(Exception): pass

def client(*args, **kwargs):
  return delegates.Client(*args, **kwargs)

def server(*args, **kwargs):
  return delegates.Server(*args, **kwargs)

from framer import Framer

class Connection(Framer):

  def __init__(self, sock, delegate=client, **args):
    Framer.__init__(self, sock)
    self.lock = RLock()
    self.attached = {}
    self.sessions = {}

    self.condition = Condition()
    # XXX: we should combine this into a single comprehensive state
    # model (whatever that means)
    self.opened = False
    self.failed = False
    self.closed = False
    self.close_code = (None, "connection aborted")

    self.thread = Thread(target=self.run)
    self.thread.setDaemon(True)

    self.channel_max = 65535
    self.user_id = None

    self.op_enc = OpEncoder()
    self.seg_enc = SegmentEncoder()
    self.frame_enc = FrameEncoder()

    self.delegate = delegate(self, **args)

  def attach(self, name, ch, delegate, force=False):
    self.lock.acquire()
    try:
      ssn = self.attached.get(ch.id)
      if ssn is not None:
        if ssn.name != name:
          raise ChannelBusy(ch, ssn)
      else:
        ssn = self.sessions.get(name)
        if ssn is None:
          ssn = Session(name, delegate=delegate)
          self.sessions[name] = ssn
        elif ssn.channel is not None:
          if force:
            del self.attached[ssn.channel.id]
            ssn.channel = None
          else:
            raise SessionBusy(ssn)
        self.attached[ch.id] = ssn
        ssn.channel = ch
      ch.session = ssn
      return ssn
    finally:
      self.lock.release()

  def detach(self, name, ch):
    self.lock.acquire()
    try:
      self.attached.pop(ch.id, None)
      ssn = self.sessions.pop(name, None)
      if ssn is not None:
        ssn.channel = None
        ssn.closed()
        return ssn
    finally:
      self.lock.release()

  def __channel(self):
    for i in xrange(1, self.channel_max):
      if not self.attached.has_key(i):
        return i
    else:
      raise ChannelsBusy()

  def session(self, name, timeout=None, delegate=session.client):
    self.lock.acquire()
    try:
      ch = Channel(self, self.__channel())
      ssn = self.attach(name, ch, delegate)
      ssn.channel.session_attach(name)
      if wait(ssn.condition, lambda: ssn.channel is not None, timeout):
        return ssn
      else:
        self.detach(name, ch)
        raise Timeout()
    finally:
      self.lock.release()

  def detach_all(self):
    self.lock.acquire()
    try:
      for ssn in self.attached.values():
        if self.close_code[0] != 200:
          ssn.exceptions.append(self.close_code)
        self.detach(ssn.name, ssn.channel)
    finally:
      self.lock.release()

  def start(self, timeout=None):
    self.delegate.start()
    self.thread.start()
    if not wait(self.condition, lambda: self.opened or self.failed, timeout):
      raise Timeout()
    if self.failed:
      raise ConnectionFailed(*self.close_code)

  def run(self):
    frame_dec = FrameDecoder()
    seg_dec = SegmentDecoder()
    op_dec = OpDecoder()

    while not self.closed:
      try:
        data = self.sock.recv(64*1024)
        if self.security_layer_rx and data:
          status, data = self.security_layer_rx.decode(data)
        if not data:
          self.detach_all()
          break
      except socket.timeout:
        if self.aborted():
          self.detach_all()
          raise Closed("connection timed out")
        else:
          continue
      except socket.error, e:
        self.detach_all()
        raise Closed(e)
      frame_dec.write(data)
      seg_dec.write(*frame_dec.read())
      op_dec.write(*seg_dec.read())
      for op in op_dec.read():
        self.delegate.received(op)
    self.sock.close()

  def write_op(self, op):
    self.sock_lock.acquire()
    try:
      self.op_enc.write(op)
      self.seg_enc.write(*self.op_enc.read())
      self.frame_enc.write(*self.seg_enc.read())
      bytes = self.frame_enc.read()
      self.write(bytes)
      self.flush()
    finally:
      self.sock_lock.release()

  def close(self, timeout=None):
    if not self.opened: return
    Channel(self, 0).connection_close(200)
    if not wait(self.condition, lambda: not self.opened, timeout):
      raise Timeout()
    self.thread.join(timeout=timeout)

  def __str__(self):
    return "%s:%s" % self.sock.getsockname()

  def __repr__(self):
    return str(self)

log = getLogger("qpid.io.ctl")

class Channel(control_invoker()):

  def __init__(self, connection, id):
    self.connection = connection
    self.id = id
    self.session = None

  def invoke(self, op, args, kwargs):
    ctl = op(*args, **kwargs)
    ctl.channel = self.id
    self.connection.write_op(ctl)
    log.debug("SENT %s", ctl)

  def __str__(self):
    return "%s[%s]" % (self.connection, self.id)

  def __repr__(self):
    return str(self)
