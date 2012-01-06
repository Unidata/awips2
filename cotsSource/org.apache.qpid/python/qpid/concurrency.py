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

import compat, inspect, time

def synchronized(meth):
  args, vargs, kwargs, defs = inspect.getargspec(meth)
  scope = {}
  scope["meth"] = meth
  exec """
def %s%s:
  %s
  %s._lock.acquire()
  try:
    return meth%s
  finally:
    %s._lock.release()
""" % (meth.__name__, inspect.formatargspec(args, vargs, kwargs, defs),
       repr(inspect.getdoc(meth)), args[0],
       inspect.formatargspec(args, vargs, kwargs, defs,
                             formatvalue=lambda x: ""),
       args[0]) in scope
  return scope[meth.__name__]

class Waiter(object):

  def __init__(self, condition):
    self.condition = condition

  def wait(self, predicate, timeout=None):
    passed = 0
    start = time.time()
    while not predicate():
      if timeout is None:
        # XXX: this timed wait thing is not necessary for the fast
        # condition from this module, only for the condition impl from
        # the threading module

        # using the timed wait prevents keyboard interrupts from being
        # blocked while waiting
        self.condition.wait(3)
      elif passed < timeout:
        self.condition.wait(timeout - passed)
      else:
        return bool(predicate())
      passed = time.time() - start
    return True

  def notify(self):
    self.condition.notify()

  def notifyAll(self):
    self.condition.notifyAll()

class Condition:

  def __init__(self, lock):
    self.lock = lock
    self.waiters = []
    self.waiting = []

  def notify(self):
    assert self.lock._is_owned()
    if self.waiting:
      self.waiting[0].wakeup()

  def notifyAll(self):
    assert self.lock._is_owned()
    for w in self.waiting:
      w.wakeup()

  def wait(self, timeout=None):
    assert self.lock._is_owned()
    if not self.waiters:
      self.waiters.append(compat.selectable_waiter())
    sw = self.waiters.pop(0)
    self.waiting.append(sw)
    try:
      st = self.lock._release_save()
      sw.wait(timeout)
    finally:
      self.lock._acquire_restore(st)
      self.waiting.remove(sw)
      self.waiters.append(sw)
