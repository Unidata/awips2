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
This module augments the standard python multithreaded Queue
implementation to add a close() method so that threads blocking on the
content of a queue can be notified if the queue is no longer in use.
"""

from Queue import Queue as BaseQueue, Empty, Full
from threading import Thread
from exceptions import Closed

class Queue(BaseQueue):

  END = object()
  STOP = object()

  def __init__(self, *args, **kwargs):
    BaseQueue.__init__(self, *args, **kwargs)
    self.error = None
    self.listener = None
    self.exc_listener = None
    self.thread = None

  def close(self, error = None):
    self.error = error
    self.put(Queue.END)
    if self.thread is not None:
      self.thread.join()
      self.thread = None

  def get(self, block = True, timeout = None):
    result = BaseQueue.get(self, block, timeout)
    if result == Queue.END:
      # this guarantees that any other waiting threads or any future
      # calls to get will also result in a Closed exception
      self.put(Queue.END)
      raise Closed(self.error)
    else:
      return result

  def listen(self, listener, exc_listener = None):
    if listener is None and exc_listener is not None:
      raise ValueError("cannot set exception listener without setting listener")

    if listener is None:
      if self.thread is not None:
        self.put(Queue.STOP)
        # loop and timed join permit keyboard interrupts to work
        while self.thread.isAlive():
          self.thread.join(3)
        self.thread = None

    self.listener = listener
    self.exc_listener = exc_listener

    if listener is not None and self.thread is None:
      self.thread = Thread(target = self.run)
      self.thread.setDaemon(True)
      self.thread.start()

  def run(self):
    while True:
      try:
        o = self.get()
        if o == Queue.STOP: break
        self.listener(o)
      except Closed, e:
        if self.exc_listener is not None:
          self.exc_listener(e)
        break
