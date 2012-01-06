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

import threading, traceback, signal, sys, time

def stackdump(sig, frm):
  code = []
  for threadId, stack in sys._current_frames().items():
    code.append("\n# ThreadID: %s" % threadId)
    for filename, lineno, name, line in traceback.extract_stack(stack):
      code.append('File: "%s", line %d, in %s' % (filename, lineno, name))
      if line:
        code.append("  %s" % (line.strip()))
  print "\n".join(code)

signal.signal(signal.SIGQUIT, stackdump)

class LoudLock:

  def __init__(self):
    self.lock = threading.RLock()

  def acquire(self, blocking=1):
    while not self.lock.acquire(blocking=0):
      time.sleep(1)
      print >> sys.out, "TRYING"
      traceback.print_stack(None, None, out)
      print >> sys.out, "TRYING"
    print >> sys.out, "ACQUIRED"
    traceback.print_stack(None, None, out)
    print >> sys.out, "ACQUIRED"
    return True

  def _is_owned(self):
    return self.lock._is_owned()

  def release(self):
    self.lock.release()

