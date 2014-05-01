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

import os, socket, time, textwrap, re

try:
  from ssl import wrap_socket as ssl
except ImportError:
  from socket import ssl as wrap_socket
  class ssl:

    def __init__(self, sock):
      self.sock = sock
      self.ssl = wrap_socket(sock)

    def recv(self, n):
      return self.ssl.read(n)

    def send(self, s):
      return self.ssl.write(s)

    def close(self):
      self.sock.close()

def connect(host, port):
  sock = socket.socket()
  sock.connect((host, port))
  sock.setblocking(1)
  # XXX: we could use this on read, but we'd have to put write in a
  # loop as well
  # sock.settimeout(1)
  return sock

def listen(host, port, predicate = lambda: True, bound = lambda: None):
  sock = socket.socket()
  sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  sock.bind((host, port))
  sock.listen(5)
  bound()
  while predicate():
    s, a = sock.accept()
    yield s

def mtime(filename):
  return os.stat(filename).st_mtime

def wait(condition, predicate, timeout=None):
  condition.acquire()
  try:
    passed = 0
    start = time.time()
    while not predicate():
      if timeout is None:
        # using the timed wait prevents keyboard interrupts from being
        # blocked while waiting
        condition.wait(3)
      elif passed < timeout:
        condition.wait(timeout - passed)
      else:
        return False
      passed = time.time() - start
    return True
  finally:
    condition.release()

def notify(condition, action=lambda: None):
  condition.acquire()
  try:
    action()
    condition.notifyAll()
  finally:
    condition.release()

def fill(text, indent, heading = None):
  sub = indent * " "
  if heading:
    if not text:
      return (indent - 2) * " " + heading
    init = (indent - 2) * " " + heading + " -- "
  else:
    init = sub
  w = textwrap.TextWrapper(initial_indent = init, subsequent_indent = sub)
  return w.fill(" ".join(text.split()))

class URL:

  RE = re.compile(r"""
        # [   <scheme>://  ] [    <user>   [   / <password>   ] @]   <host>   [   :<port>   ]
        ^ (?: ([^:/@]+)://)? (?: ([^:/@]+) (?: / ([^:/@]+)   )? @)? ([^@:/]+) (?: :([0-9]+))?$
""", re.X)

  AMQPS = "amqps"
  AMQP = "amqp"

  def __init__(self, s):
    match = URL.RE.match(s)
    if match is None:
      raise ValueError(s)
    self.scheme, self.user, self.password, self.host, port = match.groups()
    if port is None:
      self.port = None
    else:
      self.port = int(port)

  def __repr__(self):
    return "URL(%r)" % str(self)

  def __str__(self):
    s = ""
    if self.scheme:
      s += "%s://" % self.scheme
    if self.user:
      s += self.user
      if self.password:
        s += "/%s" % self.password
      s += "@"
    s += self.host
    if self.port:
      s += ":%s" % self.port
    return s

def default(value, default):
  if value is None:
    return default
  else:
    return value
