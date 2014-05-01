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

require "test/unit"
require "qpid"
require "tests/util"
require "socket"

class CodecTest < Test::Unit::TestCase

  def setup
    @spec = Qpid::Spec010::load
  end

  def check(type, value)
    t = @spec[type]
    sc = Qpid::StringCodec.new(@spec)
    t.encode(sc, value)
    decoded = t.decode(sc)
    assert_equal(value, decoded)
  end


  def testMapString
    check("map", {"string" => "this is a test"})
  end

  def testMapInt
    check("map", {"int" => 3})
  end

  def testMapLong
    check("map", {"long" => 2**32})
  end

  def testMapNone
    check("map", {"none" => None})
  end

  def testMapNested
    check("map", {"map" => {"string" => "nested test"}})
  end

  def testMapList
    check("map", {"list" => [1, "two", 3.0, -4]})
  end

  def testMapAll
    check("map", {"string" => "this is a test",
            "int" => 3,
            "long" => 2**32,
            "nil" => nil,
            "map" => {"string" => "nested map"},
            "list" => [1, "two", 3.0, -4]})
  end

  def testMapEmpty
    check("map", {})
  end

  def testMapNone
    check("map", nil)
  end

  def testList
    check("list", [1, "two", 3.0, -4])
  end

  def testListEmpty
    check("list", [])
  end

  def testListNone
    check("list", nil)
  end

  def testArrayInt
    check("array", [1, 2, 3, 4])
  end

  def testArrayString
    check("array", ["one", "two", "three", "four"])
  end

  def testArrayEmpty
    check("array", [])
  end

  def testArrayNone
    check("array", nil)
  end

  def testInt64
    check("int64", 2 ** 40 * -1 + 43)
  end

  def testUint64
    check("int64", 2 ** 42)
  end

  def testReadNone
    sc = Qpid::StringCodec.new(@spec)
    # Python behaves this way
    assert_equal("", sc.read(nil))
  end
end
