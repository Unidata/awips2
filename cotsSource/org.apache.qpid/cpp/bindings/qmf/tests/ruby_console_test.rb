#!/usr/bin/ruby

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

require 'test_base'

class ConsoleTest < ConsoleTestBase

  def test_A_agent_presence
    assert(@connection.connected?, "Connection not connected")

    agents = []
    count = 0
    while agents.size == 0
      agents = @qmfc.objects(Qmf::Query.new(:class => "agent"))
      sleep(1)
      count += 1
      fail("Timed out waiting for remote agent") if count > 10
    end

    agentList = @qmfc.agents
    assert_equal(agentList.size, 2, "Number of agents reported by Console")
  end

  def test_A_connection_settings
    begin
      @settings.bogusAttribute = 25
      fail("Connection settings accepted bogus attribute")
    rescue
    end
  end

  def test_B_basic_method_invocation
    parent = @qmfc.object(:class => "parent")
    assert(parent, "Number of 'parent' objects")
    for seq in 0...10
      result = parent.echo(seq)
      assert_equal(result.status, 0, "Method Response Status")
      assert_equal(result.text, "OK", "Method Response Text")
      assert_equal(result.args.sequence, seq, "Echo Response Sequence")
    end

    result = parent.set_numerics("bogus")
    assert_equal(result.status, 1)
    assert_equal(result.text, "Invalid argument value for test")
  end

  def test_C_basic_types_numeric_big
    parent = @qmfc.object(:class =>"parent")
    assert(parent, "Number of parent objects")

    result = parent.set_numerics("big")
    assert_equal(result.status, 0, "Method Response Status")
    assert_equal(result.text, "OK", "Method Response Text")

    parent.update

    assert_equal(parent.uint64val, 0x9494949449494949)
    assert_equal(parent.uint32val, 0xA5A55A5A)
    assert_equal(parent.uint16val, 0xB66B)
    assert_equal(parent.uint8val,  0xC7)

    assert_equal(parent.int64val, 1000000000000000000)
    assert_equal(parent.int32val, 1000000000)
    assert_equal(parent.int16val, 10000)
    assert_equal(parent.int8val,  100)
  end

  def test_C_basic_types_numeric_small
    parent = @qmfc.object(:class =>"parent")
    assert(parent, "Number of parent objects")

    result = parent.set_numerics("small")
    assert_equal(result.status, 0, "Method Response Status")
    assert_equal(result.text, "OK", "Method Response Text")

    parent.update

    assert_equal(parent.uint64val, 4)
    assert_equal(parent.uint32val, 5)
    assert_equal(parent.uint16val, 6)
    assert_equal(parent.uint8val,  7)

    assert_equal(parent.int64val, 8)
    assert_equal(parent.int32val, 9)
    assert_equal(parent.int16val, 10)
    assert_equal(parent.int8val,  11)
  end

  def test_C_basic_types_numeric_negative
    parent = @qmfc.object(:class =>"parent")
    assert(parent, "Number of parent objects")

    result = parent.set_numerics("negative")
    assert_equal(result.status, 0, "Method Response Status")
    assert_equal(result.text, "OK", "Method Response Text")

    parent.update

    assert_equal(parent.uint64val, 0)
    assert_equal(parent.uint32val, 0)
    assert_equal(parent.uint16val, 0)
    assert_equal(parent.uint8val,  0)

    assert_equal(parent.int64val, -10000000000)
    assert_equal(parent.int32val, -100000)
    assert_equal(parent.int16val, -1000)
    assert_equal(parent.int8val,  -100)
  end

  def test_C_basic_types_string_short
    parent = @qmfc.object(:class =>"parent")
    assert(parent, "Number of parent objects")

    strings = []
    strings << ""
    strings << "A"
    strings << "BC"
    strings << "DEF"
    strings << "GHIJKLMNOPQRSTUVWXYZ"
    big = "a"
    for i in 0...270 
      big << "X"
    end
    strings << big

    strings.each do |str|
      result = parent.set_short_string(str)
      assert_equal(result.status, 0, "Method Response Status")
      compare = str
      compare = compare[0..254] if compare.size > 255
      assert_equal(result.args.value, compare, "Value returned by method")
      parent.update
      assert_equal(parent.sstrval, compare, "Value stored in the object")
    end
  end

  def test_C_basic_types_string_long
    parent = @qmfc.object(:class =>"parent")
    assert(parent, "Number of parent objects")

    strings = []
    strings << ""
    strings << "A"
    strings << "BC"
    strings << "DEF"
    strings << "GHIJKLMNOPQRSTUVWXYZ"
    big = "a"
    for i in 0...270 
      big << "X"
    end
    strings << big

    strings.each do |str|
      result = parent.set_long_string(str)
      assert_equal(result.status, 0, "Method Response Status")
      assert_equal(result.args.value, str, "Value returned by method")
      parent.update
      assert_equal(parent.lstrval, str, "Value stored in the object")
    end
  end

  def test_D_userid_for_method
    parent = @qmfc.object(:class => "parent")
    assert(parent, "Number of parent objects")

    result = parent.probe_userid
    assert_equal(result.status, 0, "Method Response Status")
    assert_equal(result.args.userid, "anonymous")
  end

  def test_D_get_by_object_id
    parent = @qmfc.object(:class => "parent")
    assert(parent, "Number of parent objects")

    list = @qmfc.objects(:object_id => parent.object_id)
    assert_equal(list.size, 1)

    bad_oid = Qmf::ObjectId.new
    list = @qmfc.objects(:object_id => bad_oid)
    assert_equal(list.size, 0)

    # TODO: test a bad_oid that has an agent-bank that is not associated with an attached agent.
    
  end

  def test_D_get_with_agent
    agents = @qmfc.agents
    agents.each do |agent|
      if agent.label == "qmfa"
        parent = @qmfc.object(:class => "parent", :agent => agent)
        assert(parent, "Number of parent objects")
        return
      end
    end

    fail("Didn't find a non-broker agent")
  end

  def test_E_filter_by_object_id
    mgmt_exchange = @qmfc.object(:class => "exchange", 'name' => "qpid.management")
    assert(mgmt_exchange, "No Management Exchange")

    bindings = @qmfc.objects(:class => "binding", 'exchangeRef' => mgmt_exchange.object_id)
    if bindings.size == 0
      fail("No bindings found on management exchange")
    end

    bindings.each do |binding|
      assert_equal(binding.exchangeRef, mgmt_exchange.object_id)
    end
  end

end

app = ConsoleTest.new

