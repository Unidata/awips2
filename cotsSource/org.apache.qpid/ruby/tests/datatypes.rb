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

require 'test/unit'
require 'qpid'
require 'tests/util'

class TestSerial < Test::Unit::TestCase

  def test_cmp
    [0, 0x8FFFFFFF, 0xFFFFFFFF].each do |s|
      s = s.to_serial
      assert(s + 1 > s)
      assert(s - 1 < s)
      assert(s < s + 1)
      assert(s > s - 1)
    end
    last = 0xFFFFFFFF.to_serial
    zero = 0.to_serial
    assert_equal(zero, last + 1)

    assert_equal(last, [last, zero].min)
    assert_equal(zero, [last, zero].max)
  end

  def test_incr
    s = 0.to_serial
    s += 1
    assert_equal(1.to_serial, s)
  end

  def test_in
    l = [1, 2, 3, 4].collect { |i| i.to_serial }
    assert(l.include?(1.to_serial))
    assert(l.include?((0xFFFFFFFF + 2).to_serial))
    assert(l.include?(4))
  end

  def test_none
    assert_not_equal(nil, 0.to_serial)
  end

  def test_hash
    zero = 0.to_serial
    d = { zero => :zero }
    # FIXME: this does not work, since Ruby looks up the key and does
    # a 0.eql?(zero), which bypasses the Qpid::Serial::eql?
    # assert_equal(:zero, d[0])
  end
end

class TestRangedSet < Test::Unit::TestCase

  def assert_contains(rset, elts, nonelts = [])
    assert_equal(elts, elts.select { |e| rset.include?(e) })
    assert_equal(nonelts, nonelts.select { |e| ! rset.include?(e) })
  end

  def assert_ranges(rs, *ranges)
    assert_equal(ranges.size, rs.ranges.size)
    assert( ranges.all? { |rng| rs.include?(rng) } )
  end

  def test_simple
    rs = Qpid::RangedSet.new

    assert(rs.ranges.empty?)

    rs.add(1)
    assert_contains(rs, [1], [0,2])
    assert_ranges(rs, 1..1)

    rs.add(2)
    assert_contains(rs, [1,2], [0,3])
    assert_ranges(rs, 1..2)

    rs.add(0)
    assert_contains(rs, [0,1,2], [-1, 3])
    assert_ranges(rs, 0..2)

    rs.add(37)
    assert_contains(rs, [0,1,2,37], [-1, 3, 36, 38])
    assert_ranges(rs, 0..2, 37..37)

    rs.add(-1)
    assert_ranges(rs, -1..2, 37..37)

    rs.add(-3)
    assert_ranges(rs, -1..2, 37..37, -3..-3)

    rs.add(1, 20)
    assert_contains(rs, [20], [21])
    assert_ranges(rs, -1..20, 37..37, -3..-3)

    rs.add(21,36)
    assert_ranges(rs, -1..37, -3..-3)

    rs.add(-3, 5)
    assert_ranges(rs, -3..37)
  end

  def test_add_self
    a = Qpid::RangedSet.new
    a.add(0, 8)
    assert_ranges(a, 0..8)

    a.add(0, 8)
    assert_ranges(a, 0..8)
  end
end

class TestRange < Test::Unit::TestCase

  def test_intersect1
    a = Range.new(0, 10)
    b = Range.new(9, 20)
    i1 = a.intersect(b)
    i2 = b.intersect(a)
    assert_equal(9..10, i1)
    assert_equal(9..10, i2)
  end

  def test_intersect2
    a = Range.new(0, 10)
    b = Range.new(11, 20)
    assert_equal(nil, a.intersect(b))
    assert_equal(nil, b.intersect(a))
  end

  def test_intersect3
    a = Range.new(0, 10)
    b = Range.new(3, 5)
    i1 = a.intersect(b)
    i2 = b.intersect(a)
    assert_equal(3..5, i1)
    assert_equal(3..5, i2)
  end
end

class TestUUIDTest < Test::Unit::TestCase

  def test_simple
    # this test is kind of lame, but it does excercise the basic
    # functionality of the class
    u = Qpid::UUID::uuid4
    1024.times { |i| assert_not_equal(u, Qpid::UUID::uuid4) }
    assert_raise NotImplementedError do
      u == 0
    end
  end
end

class TestMessage < Test::Unit::TestCase

  def setup
    @@spec ||= Qpid::Spec010::load()
    @mp = Qpid::struct(@@spec["message_properties"])
    @dp = Qpid::struct(@@spec["delivery_properties"])
    @fp = Qpid::struct(@@spec["fragment_properties"])
  end

  def test_has
    m = Qpid::Message.new(@mp, @dp, @fp, "body")
    assert m.has("message_properties")
    assert m.has("delivery_properties")
    assert m.has("fragment_properties")
  end

  def test_get
    m = Qpid::Message.new(@mp, @dp, @fp, "body")
    assert_same(@mp, m.get("message_properties"))
    assert_same(@dp, m.get("delivery_properties"))
    assert_same(@fp, m.get("fragment_properties"))
  end

  def test_set
    m = Qpid::Message.new(@mp, @dp, "body")
    assert_nil m.get("fragment_properties")
    m.set(@fp)
    assert_same(@fp, m.get("fragment_properties"), "4")
  end

  def test_set_on_empty
    m = Qpid::Message.new("body")
    assert_nil m.get("delivery_properties")
    m.set(@dp)
    assert_same(@dp, m.get("delivery_properties"), "5")
  end

  def test_set_replace
    m = Qpid::Message.new(@mp, @dp, @fp, "body")
    dp = Qpid::struct(@@spec["delivery_properties"])
    assert_same(@dp, m.get("delivery_properties"), "6")
    m.set(dp)
    assert_same(dp, m.get("delivery_properties"), "7")
  end

  def test_clear
    m = Qpid::Message.new(@mp, @dp, @fp, "body")
    assert_same(@mp, m.get("message_properties"), "8")
    assert_same(@dp, m.get("delivery_properties"), "9")
    assert_same(@fp, m.get("fragment_properties"), "10")
    m.clear("fragment_properties")
    assert_nil m.get("fragment_properties")
    assert_same(@mp, m.get("message_properties"), "11")
    assert_same(@dp, m.get("delivery_properties"), "12")
  end
end
