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
from connection08 import Method, Request

class Message:

  def __init__(self, channel, frame, content = None):
    self.channel = channel
    self.frame = frame
    self.method = frame.method_type
    self.content = content
    if self.method.is_l4_command():
      self.command_id = self.channel.incoming_completion.sequence.next()
      #print "allocated: ", self.command_id, "to ", self.method.klass.name, "_", self.method.name

  def __len__(self):
    return len(self.frame.args)

  def _idx(self, idx):
    if idx < 0: idx += len(self)
    if idx < 0 or idx > len(self):
      raise IndexError(idx)
    return idx

  def __getitem__(self, idx):
    return self.frame.args[idx]

  def __getattr__(self, attr):
    fields = self.method.fields.byname
    if fields.has_key(attr):
      f = fields[attr]
      result = self[self.method.fields.index(f)]
    else:
      for r in self.method.responses:
        if attr == r.name:
          def respond(*args, **kwargs):
            batch=0
            if kwargs.has_key("batchoffset"):
              batch=kwargs.pop("batchoffset")
            self.channel.respond(Method(r, r.arguments(*args, **kwargs)), batch, self.frame)
          result = respond
          break
      else:
        raise AttributeError(attr)
    return result

  STR = "%s %s content = %s"
  REPR = STR.replace("%s", "%r")

  def __str__(self):
    return Message.STR % (self.method, self.frame.args, self.content)

  def __repr__(self):
    return Message.REPR % (self.method, self.frame.args, self.content)

  def complete(self, cumulative=True):
    self.channel.incoming_completion.complete(mark=self.command_id, cumulative=cumulative)
