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

from unittest import TestCase
from qpid.datatypes import *
from qpid.ops import DeliveryProperties, FragmentProperties, MessageProperties

class SerialTest(TestCase):

  def test(self):
    for s in (serial(0), serial(0x8FFFFFFFL), serial(0xFFFFFFFFL)):
      assert s + 1 > s
      assert s - 1 < s
      assert s < s + 1
      assert s > s - 1

    assert serial(0xFFFFFFFFL) + 1 == serial(0)

    assert min(serial(0xFFFFFFFFL), serial(0x0)) == serial(0xFFFFFFFFL)
    assert max(serial(0xFFFFFFFFL), serial(0x0)) == serial(0x0)

  def testIncr(self):
    s = serial(0)
    s += 1
    assert s == serial(1)

  def testIn(self):
    l = [serial(1), serial(2), serial(3), serial(4)]
    assert serial(1) in l
    assert serial(0xFFFFFFFFL + 2) in l
    assert 4 in l

  def testNone(self):
    assert serial(0) != None

  def testHash(self):
    d = {}
    d[serial(0)] = "zero"
    assert d[0] == "zero"

  def testAdd(self):
    assert serial(2) + 2 == serial(4)
    assert serial(2) + 2 == 4

  def testSub(self):
    delta = serial(4) - serial(2)
    assert isinstance(delta, int) or isinstance(delta, long)
    assert delta == 2

    delta = serial(4) - 2
    assert isinstance(delta, Serial)
    assert delta == serial(2)

class RangedSetTest(TestCase):

  def check(self, ranges):
    posts = []
    for range in ranges:
      posts.append(range.lower)
      posts.append(range.upper)

    sorted = posts[:]
    sorted.sort()

    assert posts == sorted

    idx = 1
    while idx + 1 < len(posts):
      assert posts[idx] + 1 != posts[idx+1]
      idx += 2

  def test(self):
    rs = RangedSet()

    self.check(rs.ranges)

    rs.add(1)

    assert 1 in rs
    assert 2 not in rs
    assert 0 not in rs
    self.check(rs.ranges)

    rs.add(2)

    assert 0 not in rs
    assert 1 in rs
    assert 2 in rs
    assert 3 not in rs
    self.check(rs.ranges)

    rs.add(0)

    assert -1 not in rs
    assert 0 in rs
    assert 1 in rs
    assert 2 in rs
    assert 3 not in rs
    self.check(rs.ranges)

    rs.add(37)

    assert -1 not in rs
    assert 0 in rs
    assert 1 in rs
    assert 2 in rs
    assert 3 not in rs
    assert 36 not in rs
    assert 37 in rs
    assert 38 not in rs
    self.check(rs.ranges)

    rs.add(-1)
    self.check(rs.ranges)

    rs.add(-3)
    self.check(rs.ranges)

    rs.add(1, 20)
    assert 21 not in rs
    assert 20 in rs
    self.check(rs.ranges)

  def testAddSelf(self):
    a = RangedSet()
    a.add(0, 8)
    self.check(a.ranges)
    a.add(0, 8)
    self.check(a.ranges)
    assert len(a.ranges) == 1
    range = a.ranges[0]
    assert range.lower == 0
    assert range.upper == 8

  def testEmpty(self):
    s = RangedSet()
    assert s.empty()
    s.add(0, -1)
    assert s.empty()
    s.add(0, 0)
    assert not s.empty()

  def testMinMax(self):
    s = RangedSet()
    assert s.max() is None
    assert s.min() is None
    s.add(0, 10)
    assert s.max() == 10
    assert s.min() == 0
    s.add(0, 5)
    assert s.max() == 10
    assert s.min() == 0
    s.add(0, 11)
    assert s.max() == 11
    assert s.min() == 0
    s.add(15, 20)
    assert s.max() == 20
    assert s.min() == 0
    s.add(-10, -5)
    assert s.max() == 20
    assert s.min() == -10

class RangeTest(TestCase):

  def testIntersect1(self):
    a = Range(0, 10)
    b = Range(9, 20)
    i1 = a.intersect(b)
    i2 = b.intersect(a)
    assert i1.upper == 10
    assert i2.upper == 10
    assert i1.lower == 9
    assert i2.lower == 9

  def testIntersect2(self):
    a = Range(0, 10)
    b = Range(11, 20)
    assert a.intersect(b) == None
    assert b.intersect(a) == None

  def testIntersect3(self):
    a = Range(0, 10)
    b = Range(3, 5)
    i1 = a.intersect(b)
    i2 = b.intersect(a)
    assert i1.upper == 5
    assert i2.upper == 5
    assert i1.lower == 3
    assert i2.lower == 3

class UUIDTest(TestCase):

  def test(self):
    # this test is kind of lame, but it does excercise the basic
    # functionality of the class
    u = uuid4()
    for i in xrange(1024):
      assert u != uuid4()

class MessageTest(TestCase):

  def setUp(self):
    self.mp = MessageProperties()
    self.dp = DeliveryProperties()
    self.fp = FragmentProperties()

  def testHas(self):
    m = Message(self.mp, self.dp, self.fp, "body")
    assert m.has("message_properties")
    assert m.has("delivery_properties")
    assert m.has("fragment_properties")

  def testGet(self):
    m = Message(self.mp, self.dp, self.fp, "body")
    assert m.get("message_properties") == self.mp
    assert m.get("delivery_properties") == self.dp
    assert m.get("fragment_properties") == self.fp

  def testSet(self):
    m = Message(self.mp, self.dp, "body")
    assert m.get("fragment_properties") is None
    m.set(self.fp)
    assert m.get("fragment_properties") == self.fp

  def testSetOnEmpty(self):
    m = Message("body")
    assert m.get("delivery_properties") is None
    m.set(self.dp)
    assert m.get("delivery_properties") == self.dp

  def testSetReplace(self):
    m = Message(self.mp, self.dp, self.fp, "body")
    dp = DeliveryProperties()
    assert m.get("delivery_properties") == self.dp
    assert m.get("delivery_properties") != dp
    m.set(dp)
    assert m.get("delivery_properties") != self.dp
    assert m.get("delivery_properties") == dp

  def testClear(self):
    m = Message(self.mp, self.dp, self.fp, "body")
    assert m.get("message_properties") == self.mp
    assert m.get("delivery_properties") == self.dp
    assert m.get("fragment_properties") == self.fp
    m.clear("fragment_properties")
    assert m.get("fragment_properties") is None
    assert m.get("message_properties") == self.mp
    assert m.get("delivery_properties") == self.dp

class TimestampTest(TestCase):

  def check(self, expected, *values):
    for v in values:
      assert isinstance(v, timestamp)
      assert v == expected
      assert v == timestamp(expected)

  def testAdd(self):
    self.check(4.0,
               timestamp(2.0) + 2.0,
               2.0 + timestamp(2.0))

  def testSub(self):
    self.check(2.0,
               timestamp(4.0) - 2.0,
               4.0 - timestamp(2.0))

  def testNeg(self):
    self.check(-4.0, -timestamp(4.0))

  def testPos(self):
    self.check(+4.0, +timestamp(4.0))

  def testAbs(self):
    self.check(4.0, abs(timestamp(-4.0)))

  def testConversion(self):
    dt = timestamp(0).datetime()
    t = timestamp(dt)
    assert t == 0
