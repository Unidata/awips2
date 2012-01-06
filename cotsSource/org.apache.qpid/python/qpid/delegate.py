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
Delegate implementation intended for use with the peer module.
"""

import threading, inspect, traceback, sys
from connection08 import Method, Request, Response

def _handler_name(method):
  return "%s_%s" % (method.klass.name, method.name)

class Delegate:

  def __init__(self):
    self.handlers = {}
    self.invokers = {}

  def __call__(self, channel, frame):
    method = frame.method

    try:
      handler = self.handlers[method]
    except KeyError:
      name = _handler_name(method)
      handler = getattr(self, name)
      self.handlers[method] = handler

    try:
      return handler(channel, frame)
    except:
      print >> sys.stderr, "Error in handler: %s\n\n%s" % \
            (_handler_name(method), traceback.format_exc())

  def closed(self, reason):
    print "Connection closed: %s" % reason
