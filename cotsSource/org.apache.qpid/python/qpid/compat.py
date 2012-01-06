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

import sys

try:
  set = set
except NameError:
  from sets import Set as set

try:
  from socket import SHUT_RDWR
except ImportError:
  SHUT_RDWR = 2

try:
  from traceback import format_exc
except ImportError:
  import traceback
  def format_exc():
    return "".join(traceback.format_exception(*sys.exc_info()))

if tuple(sys.version_info[0:2]) < (2, 4):
  from select import select as old_select
  def select(rlist, wlist, xlist, timeout=None):
    return old_select(list(rlist), list(wlist), list(xlist), timeout)
else:
  from select import select

class BaseWaiter:

  def wakeup(self):
    self._do_write()

  def wait(self, timeout=None):
    if timeout is not None:
      ready, _, _ = select([self], [], [], timeout)
    else:
      ready = True

    if ready:
      self._do_read()
      return True
    else:
      return False

  def reading(self):
    return True

  def readable(self):
    self._do_read()

if sys.platform in ('win32', 'cygwin'):
  import socket

  class SockWaiter(BaseWaiter):

    def __init__(self, read_sock, write_sock):
      self.read_sock = read_sock
      self.write_sock = write_sock

    def _do_write(self):
      self.write_sock.send("\0")

    def _do_read(self):
      self.read_sock.recv(65536)

    def fileno(self):
      return self.read_sock.fileno()

  def __repr__(self):
    return "SockWaiter(%r, %r)" % (self.read_sock, self.write_sock)

  def selectable_waiter():
    listener = socket.socket()
    listener.bind(('', 0))
    listener.listen(1)
    _, port = listener.getsockname()
    write_sock = socket.socket()
    write_sock.connect(("127.0.0.1", port))
    read_sock, _ = listener.accept()
    listener.close()
    return SockWaiter(read_sock, write_sock)
else:
  import os

  class PipeWaiter(BaseWaiter):

    def __init__(self, read_fd, write_fd):
      self.read_fd = read_fd
      self.write_fd = write_fd

    def _do_write(self):
      os.write(self.write_fd, "\0")

    def _do_read(self):
      os.read(self.read_fd, 65536)

    def fileno(self):
      return self.read_fd

    def __repr__(self):
      return "PipeWaiter(%r, %r)" % (self.read_fd, self.write_fd)

  def selectable_waiter():
    return PipeWaiter(*os.pipe())
