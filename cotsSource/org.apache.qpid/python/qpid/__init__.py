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

import spec, codec, connection, content, peer, delegate, client

class Struct:

  def __init__(self, type, *args, **kwargs):
    self.__dict__["type"] = type
    self.__dict__["_values"] = {}

    if len(args) > len(self.type.fields):
      raise TypeError("too many args")

    for a, f in zip(args, self.type.fields):
      self.set(f.name, a)

    for k, a in kwargs.items():
      self.set(k, a)

  def _check(self, attr):
    field = self.type.fields.byname.get(attr)
    if field == None:
      raise AttributeError(attr)
    return field

  def exists(self, attr):
    return self.type.fields.byname.has_key(attr)

  def has(self, attr):
    self._check(attr)
    return self._values.has_key(attr)

  def set(self, attr, value):
    self._check(attr)
    self._values[attr] = value

  def get(self, attr):
    field = self._check(attr)
    return self._values.get(attr, field.default())

  def clear(self, attr):
    self._check(attr)
    del self._values[attr]

  def __setattr__(self, attr, value):
    self.set(attr, value)

  def __getattr__(self, attr):
    return self.get(attr)

  def __delattr__(self, attr):
    self.clear(attr)

  def __setitem__(self, attr, value):
    self.set(attr, value)

  def __getitem__(self, attr):
    return self.get(attr)

  def __delitem__(self, attr):
    self.clear(attr)

  def __str__(self):
    return "%s %s" % (self.type, self._values)

  def __repr__(self):
    return str(self)
