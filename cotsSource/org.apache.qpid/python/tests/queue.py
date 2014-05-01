# Do not delete - marks this directory as a python package.

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
import threading, time
from unittest import TestCase
from qpid.queue import Queue, Empty, Closed


class QueueTest (TestCase):

  # The qpid queue class just provides sime simple extensions to
  # python's standard queue data structure, so we don't need to test
  # all the queue functionality.

  def test_listen(self):
    values = []
    heard = threading.Event()
    def listener(x):
      values.append(x)
      heard.set()

    q = Queue(0)
    q.listen(listener)
    heard.clear()
    q.put(1)
    heard.wait()
    assert values[-1] == 1
    heard.clear()
    q.put(2)
    heard.wait()
    assert values[-1] == 2

    q.listen(None)
    q.put(3)
    assert q.get(3) == 3
    q.listen(listener)

    heard.clear()
    q.put(4)
    heard.wait()
    assert values[-1] == 4

  def test_close(self):
    q = Queue(0)
    q.put(1); q.put(2); q.put(3); q.close()
    assert q.get() == 1
    assert q.get() == 2
    assert q.get() == 3
    for i in range(10):
      try:
        q.get()
        raise AssertionError("expected Closed")
      except Closed:
        pass
