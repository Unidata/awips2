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
A simple python representation for AMQP content.
"""

def default(val, defval):
  if val == None:
    return defval
  else:
    return val

class Content:

  def __init__(self, body = "", children = None, properties = None):
    self.body = body
    self.children = default(children, [])
    self.properties = default(properties, {})

  def size(self):
    return len(self.body)

  def weight(self):
    return len(self.children)

  def __getitem__(self, name):
    return self.properties[name]

  def __setitem__(self, name, value):
    self.properties[name] = value

  def __delitem__(self, name):
    del self.properties[name]

  def __str__(self):
    if self.children:
      return "%s [%s] %s" % (self.properties,
                             ", ".join(map(str, self.children)),
                             self.body)
    else:
      return "%s %s" % (self.properties, self.body)
