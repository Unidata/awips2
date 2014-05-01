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

import struct, socket
from exceptions import Closed
from packer import Packer
from threading import RLock
from logging import getLogger

raw = getLogger("qpid.io.raw")
frm = getLogger("qpid.io.frm")

class FramingError(Exception): pass

class Framer(Packer):

  HEADER="!4s4B"

  def __init__(self, sock):
    self.sock = sock
    self.sock_lock = RLock()
    self.tx_buf = ""
    self.rx_buf = ""
    self.security_layer_tx = None
    self.security_layer_rx = None
    self.maxbufsize = 65535

  def aborted(self):
    return False

  def write(self, buf):
    self.tx_buf += buf

  def flush(self):
    self.sock_lock.acquire()
    try:
      if self.security_layer_tx:
        status, cipher_buf = self.security_layer_tx.encode(self.tx_buf)
        if status == False:
          raise Closed(self.security_layer_tx.getError())
        self._write(cipher_buf)
      else:
        self._write(self.tx_buf)
      self.tx_buf = ""
      frm.debug("FLUSHED")
    finally:
      self.sock_lock.release()

  def _write(self, buf):
    while buf:
      try:
        n = self.sock.send(buf)
      except socket.timeout:
        if self.aborted():
          raise Closed()
        else:
          continue
      raw.debug("SENT %r", buf[:n])
      buf = buf[n:]

  ##
  ## Implementation Note:
  ##
  ##    This function was modified to use the SASL security layer for content
  ##    decryption.  As such, the socket read should read in "self.maxbufsize"
  ##    instead of "n" (the requested number of octets).  However, since this
  ##    is one of two places in the code where the socket is read, the read
  ##    size had to be left at "n".  This is because this function is
  ##    apparently only used to read the first 8 octets from a TCP socket.  If
  ##    we read beyond "n" octets, the remaing octets won't be processed and
  ##    the connection handshake will fail.
  ##
  def read(self, n):
    while len(self.rx_buf) < n:
      try:
        s = self.sock.recv(n) # NOTE: instead of "n", arg should be "self.maxbufsize"
        if self.security_layer_rx:
          status, s = self.security_layer_rx.decode(s)
          if status == False:
            raise Closed(self.security_layer_tx.getError())
      except socket.timeout:
        if self.aborted():
          raise Closed()
        else:
          continue
      except socket.error, e:
        if self.rx_buf != "":
          raise e
        else:
          raise Closed()
      if len(s) == 0:
        raise Closed()
      self.rx_buf += s
      raw.debug("RECV %r", s)
    data = self.rx_buf[0:n]
    self.rx_buf = self.rx_buf[n:]
    return data

  def read_header(self):
    return self.unpack(Framer.HEADER)

  def write_header(self, major, minor):
    self.sock_lock.acquire()
    try:
      self.pack(Framer.HEADER, "AMQP", 1, 1, major, minor)
      self.flush()
    finally:
      self.sock_lock.release()
