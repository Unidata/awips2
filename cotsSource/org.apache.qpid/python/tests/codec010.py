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

import time

from unittest import TestCase
from qpid.codec010 import StringCodec
from qpid.datatypes import timestamp, uuid4
from qpid.ops import PRIMITIVE

class CodecTest(TestCase):

  def check(self, type, value, compare=True):
    t = PRIMITIVE[type]
    sc = StringCodec()
    sc.write_primitive(t, value)
    decoded = sc.read_primitive(t)
    if compare:
      assert decoded == value, "%s, %s" % (decoded, value)
    return decoded

  def testMapString(self):
    self.check("map", {"string": "this is a test"})

  def testMapUnicode(self):
    self.check("map", {"unicode": u"this is a unicode test"})

  def testMapBinary(self):
    self.check("map", {"binary": "\x7f\xb4R^\xe5\xf0:\x89\x96E1\xf6\xfe\xb9\x1b\xf5"})

  def testMapBuffer(self):
    s = "\x7f\xb4R^\xe5\xf0:\x89\x96E1\xf6\xfe\xb9\x1b\xf5"
    dec = self.check("map", {"buffer": buffer(s)}, False)
    assert dec["buffer"] == s

  def testMapInt(self):
    self.check("map", {"int": 3})

  def testMapLong(self):
    self.check("map", {"long": 2**32})
    self.check("map", {"long": 1 << 34})
    self.check("map", {"long": -(1 << 34)})

  def testMapTimestamp(self):
    decoded = self.check("map", {"timestamp": timestamp(0)})
    assert isinstance(decoded["timestamp"], timestamp)

  def testMapDatetime(self):
    decoded = self.check("map", {"datetime": timestamp(0).datetime()}, compare=False)
    assert isinstance(decoded["datetime"], timestamp)
    assert decoded["datetime"] == 0.0

  def testMapNone(self):
    self.check("map", {"none": None})

  def testMapNested(self):
    self.check("map", {"map": {"string": "nested test"}})

  def testMapList(self):
    self.check("map", {"list": [1, "two", 3.0, -4]})

  def testMapUUID(self):
    self.check("map", {"uuid": uuid4()})

  def testMapAll(self):
    decoded = self.check("map", {"string": "this is a test",
                                 "unicode": u"this is a unicode test",
                                 "binary": "\x7f\xb4R^\xe5\xf0:\x89\x96E1\xf6\xfe\xb9\x1b\xf5",
                                 "int": 3,
                                 "long": 2**32,
                                 "timestamp": timestamp(0),
                                 "none": None,
                                 "map": {"string": "nested map"},
                                 "list": [1, "two", 3.0, -4],
                                 "uuid": uuid4()})
    assert isinstance(decoded["timestamp"], timestamp)

  def testMapEmpty(self):
    self.check("map", {})

  def testMapNone(self):
    self.check("map", None)

  def testList(self):
    self.check("list", [1, "two", 3.0, -4])

  def testListEmpty(self):
    self.check("list", [])

  def testListNone(self):
    self.check("list", None)

  def testArrayInt(self):
    self.check("array", [1, 2, 3, 4])

  def testArrayString(self):
    self.check("array", ["one", "two", "three", "four"])

  def testArrayEmpty(self):
    self.check("array", [])

  def testArrayNone(self):
    self.check("array", None)

  def testInt16(self):
    self.check("int16", 3)
    self.check("int16", -3)

  def testInt64(self):
    self.check("int64", 3)
    self.check("int64", -3)
    self.check("int64", 1<<34)
    self.check("int64", -(1<<34))

  def testDatetime(self):
    self.check("datetime", timestamp(0))
    self.check("datetime", timestamp(long(time.time())))
