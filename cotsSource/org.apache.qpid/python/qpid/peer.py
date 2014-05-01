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
This module contains a skeletal peer implementation useful for
implementing an AMQP server, client, or proxy. The peer implementation
sorts incoming frames to their intended channels, and dispatches
incoming method frames to a delegate.
"""

import thread, threading, traceback, socket, sys, logging
from connection08 import EOF, Method, Header, Body, Request, Response, VersionError
from message import Message
from queue import Queue, Closed as QueueClosed
from content import Content
from cStringIO import StringIO
from time import time

class Sequence:

  def __init__(self, start, step = 1):
    # we should keep start for wrap around
    self._next = start
    self.step = step
    self.lock = thread.allocate_lock()

  def next(self):
    self.lock.acquire()
    try:
      result = self._next
      self._next += self.step
      return result
    finally:
      self.lock.release()

class Peer:

  def __init__(self, conn, delegate, channel_factory=None):
    self.conn = conn
    self.delegate = delegate
    self.outgoing = Queue(0)
    self.work = Queue(0)
    self.channels = {}
    self.lock = thread.allocate_lock()
    if channel_factory:
      self.channel_factory = channel_factory
    else:
      self.channel_factory = Channel

  def channel(self, id):
    self.lock.acquire()
    try:
      try:
        ch = self.channels[id]
      except KeyError:
        ch = self.channel_factory(id, self.outgoing, self.conn.spec)
        self.channels[id] = ch
    finally:
      self.lock.release()
    return ch

  def start(self):
    thread.start_new_thread(self.writer, ())
    thread.start_new_thread(self.reader, ())
    thread.start_new_thread(self.worker, ())

  def fatal(self, message=None):
    """Call when an unexpected exception occurs that will kill a thread."""
    if message: print >> sys.stderr, message
    self.closed("Fatal error: %s\n%s" % (message or "", traceback.format_exc()))

  def reader(self):
    try:
      while True:
        try:
          frame = self.conn.read()
        except EOF, e:
          self.work.close()
          break
        ch = self.channel(frame.channel)
        ch.receive(frame, self.work)
    except VersionError, e:
      self.closed(e)
    except:
      self.fatal()

  def closed(self, reason):
    # We must close the delegate first because closing channels
    # may wake up waiting threads and we don't want them to see
    # the delegate as open.
    self.delegate.closed(reason)
    for ch in self.channels.values():
      ch.closed(reason)

  def writer(self):
    try:
      while True:
        try:
          message = self.outgoing.get()
          self.conn.write(message)
        except socket.error, e:
          self.closed(e)
          break
        self.conn.flush()
    except:
      self.fatal()

  def worker(self):
    try:
      while True:
        queue = self.work.get()
        frame = queue.get()
        channel = self.channel(frame.channel)
        if frame.method_type.content:
          content = read_content(queue)
        else:
          content = None

        self.delegate(channel, Message(channel, frame, content))
    except QueueClosed:
      self.closed("worker closed")
    except:
      self.fatal()

class Requester:

  def __init__(self, writer):
    self.write = writer
    self.sequence = Sequence(1)
    self.mark = 0
    # request_id -> listener
    self.outstanding = {}

  def request(self, method, listener, content = None):
    frame = Request(self.sequence.next(), self.mark, method)
    self.outstanding[frame.id] = listener
    self.write(frame, content)

  def receive(self, channel, frame):
    listener = self.outstanding.pop(frame.request_id)
    listener(channel, frame)

class Responder:

  def __init__(self, writer):
    self.write = writer
    self.sequence = Sequence(1)

  def respond(self, method, batch, request):
    if isinstance(request, Method):
      self.write(method)
    else:
      # allow batching from frame at either end
      if batch<0:
        frame = Response(self.sequence.next(), request.id+batch, -batch, method)
      else:
        frame = Response(self.sequence.next(), request.id, batch, method)
      self.write(frame)

class Closed(Exception): pass

class Channel:

  def __init__(self, id, outgoing, spec):
    self.id = id
    self.outgoing = outgoing
    self.spec = spec
    self.incoming = Queue(0)
    self.responses = Queue(0)
    self.queue = None
    self._closed = False
    self.reason = None

    self.requester = Requester(self.write)
    self.responder = Responder(self.write)

    self.completion = OutgoingCompletion()
    self.incoming_completion = IncomingCompletion(self)
    self.futures = {}
    self.control_queue = Queue(0)#used for incoming methods that appas may want to handle themselves

    self.invoker = self.invoke_method
    self.use_execution_layer = (spec.major == 0 and spec.minor == 10) or (spec.major == 99 and spec.minor == 0)
    self.synchronous = True

  def closed(self, reason):
    if self._closed:
      return
    self._closed = True
    self.reason = reason
    self.incoming.close()
    self.responses.close()
    self.completion.close()
    self.incoming_completion.reset()
    for f in self.futures.values():
      f.put_response(self, reason)

  def write(self, frame, content = None):
    if self._closed:
      raise Closed(self.reason)
    frame.channel = self.id
    self.outgoing.put(frame)
    if (isinstance(frame, (Method, Request))
        and content == None
        and frame.method_type.content):
      content = Content()
    if content != None:
      self.write_content(frame.method_type.klass, content)

  def write_content(self, klass, content):
    header = Header(klass, content.weight(), content.size(), content.properties)
    self.write(header)
    for child in content.children:
      self.write_content(klass, child)
    # should split up if content.body exceeds max frame size
    if content.body:
      self.write(Body(content.body))

  def receive(self, frame, work):
    if isinstance(frame, Method):
      if frame.method.response:
        self.queue = self.responses
      else:
        self.queue = self.incoming
        work.put(self.incoming)
    elif isinstance(frame, Request):
      self.queue = self.incoming
      work.put(self.incoming)
    elif isinstance(frame, Response):
      self.requester.receive(self, frame)
      if frame.method_type.content:
        self.queue = self.responses
      return
    self.queue.put(frame)

  def queue_response(self, channel, frame):
    channel.responses.put(frame.method)

  def request(self, method, listener, content = None):
    self.requester.request(method, listener, content)

  def respond(self, method, batch, request):
    self.responder.respond(method, batch, request)

  def invoke(self, type, args, kwargs):
    if (type.klass.name in ["channel", "session"]) and (type.name in ["close", "open", "closed"]):
      self.completion.reset()
      self.incoming_completion.reset()
    self.completion.next_command(type)

    content = kwargs.pop("content", None)
    frame = Method(type, type.arguments(*args, **kwargs))
    return self.invoker(frame, content)

  # used for 0-9
  def invoke_reliable(self, frame, content = None):
    if not self.synchronous:
      future = Future()
      self.request(frame, future.put_response, content)
      if not frame.method.responses: return None
      else: return future

    self.request(frame, self.queue_response, content)
    if not frame.method.responses:
      if self.use_execution_layer and frame.method_type.is_l4_command():
        self.execution_sync()
        self.completion.wait()
        if self._closed:
          raise Closed(self.reason)
      return None
    try:
      resp = self.responses.get()
      if resp.method_type.content:
        return Message(self, resp, read_content(self.responses))
      else:
        return Message(self, resp)
    except QueueClosed, e:
      if self._closed:
        raise Closed(self.reason)
      else:
        raise e

  # used for 0-8 and 0-10
  def invoke_method(self, frame, content = None):
    if frame.method.result:
      cmd_id = self.completion.command_id
      future = Future()
      self.futures[cmd_id] = future

    self.write(frame, content)

    try:
      # here we depend on all nowait fields being named nowait
      f = frame.method.fields.byname["nowait"]
      nowait = frame.args[frame.method.fields.index(f)]
    except KeyError:
      nowait = False

    try:
      if not nowait and frame.method.responses:
        resp = self.responses.get()
        if resp.method.content:
          content = read_content(self.responses)
        else:
          content = None
        if resp.method in frame.method.responses:
          return Message(self, resp, content)
        else:
          raise ValueError(resp)
      elif frame.method.result:
        if self.synchronous:
          fr = future.get_response(timeout=10)
          if self._closed:
            raise Closed(self.reason)
          return fr
        else:
          return future
      elif self.synchronous and not frame.method.response \
               and self.use_execution_layer and frame.method.is_l4_command():
        self.execution_sync()
        completed = self.completion.wait(timeout=10)
        if self._closed:
          raise Closed(self.reason)
        if not completed:
          self.closed("Timed-out waiting for completion of %s" % frame)
    except QueueClosed, e:
      if self._closed:
        raise Closed(self.reason)
      else:
        raise e

  def __getattr__(self, name):
    type = self.spec.method(name)
    if type == None: raise AttributeError(name)
    method = lambda *args, **kwargs: self.invoke(type, args, kwargs)
    self.__dict__[name] = method
    return method

def read_content(queue):
  header = queue.get()
  children = []
  for i in range(header.weight):
    children.append(read_content(queue))
  buf = StringIO()
  eof = header.eof
  while not eof:
    body = queue.get()
    eof = body.eof
    content = body.content
    buf.write(content)
  return Content(buf.getvalue(), children, header.properties.copy())

class Future:
  def __init__(self):
    self.completed = threading.Event()

  def put_response(self, channel, response):
    self.response = response
    self.completed.set()

  def get_response(self, timeout=None):
    self.completed.wait(timeout)
    if self.completed.isSet():
      return self.response
    else:
      return None

  def is_complete(self):
    return self.completed.isSet()

class OutgoingCompletion:
  """
  Manages completion of outgoing commands i.e. command sent by this peer
  """

  def __init__(self):
    self.condition = threading.Condition()

    #todo, implement proper wraparound
    self.sequence = Sequence(0) #issues ids for outgoing commands
    self.command_id = -1        #last issued id
    self.mark = -1              #commands up to this mark are known to be complete
    self._closed = False

  def next_command(self, method):
    #the following test is a hack until the track/sub-channel is available
    if method.is_l4_command():
      self.command_id = self.sequence.next()

  def reset(self):
    self.sequence = Sequence(0) #reset counter

  def close(self):
    self.reset()
    self.condition.acquire()
    try:
      self._closed = True
      self.condition.notifyAll()
    finally:
      self.condition.release()

  def complete(self, mark):
    self.condition.acquire()
    try:
      self.mark = mark
      #print "set mark to %s [%s] " % (self.mark, self)
      self.condition.notifyAll()
    finally:
      self.condition.release()

  def wait(self, point_of_interest=-1, timeout=None):
    if point_of_interest == -1: point_of_interest = self.command_id
    start_time = time()
    remaining = timeout
    self.condition.acquire()
    try:
      while not self._closed and point_of_interest > self.mark:
        #print "waiting for %s, mark = %s [%s]" % (point_of_interest, self.mark, self)
        self.condition.wait(remaining)
        if not self._closed and point_of_interest > self.mark and timeout:
          if (start_time + timeout) < time(): break
          else: remaining = timeout - (time() - start_time)
    finally:
      self.condition.release()
    return point_of_interest <= self.mark

class IncomingCompletion:
  """
  Manages completion of incoming commands i.e. command received by this peer
  """

  def __init__(self, channel):
    self.sequence = Sequence(0) #issues ids for incoming commands
    self.mark = -1               #id of last command of whose completion notification was sent to the other peer
    self.channel = channel

  def reset(self):
    self.sequence = Sequence(0) #reset counter

  def complete(self, mark, cumulative=True):
    if cumulative:
      if mark > self.mark:
        self.mark = mark
        self.channel.execution_complete(cumulative_execution_mark=self.mark)
    else:
      #TODO: record and manage the ranges properly
      range = [mark, mark]
      if (self.mark == -1):#hack until wraparound is implemented        
        self.channel.execution_complete(cumulative_execution_mark=0xFFFFFFFFL, ranged_execution_set=range)
      else:
        self.channel.execution_complete(cumulative_execution_mark=self.mark, ranged_execution_set=range)
