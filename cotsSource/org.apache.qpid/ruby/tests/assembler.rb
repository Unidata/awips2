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
require 'tests/util'

require 'logger'

class TestAssembler< Test::Unit::TestCase

  Segment = Qpid::Segment
  Assembler = Qpid::Assembler

  def setup
    # Qpid::asm_logger = Logger.new(STDOUT)

    @server = Util::ServerThread.new do |socket|
      asm = Assembler.new(socket)
      begin
        header = asm.read_header
        asm.write_header(header[-2], header[-1])
        loop do
          seg = asm.read_segment
          asm.write_segment(seg)
        end
      rescue Qpid::Closed
        nil # Ignore
      end
    end
  end

  def teardown
    @server.finish
    @server.join
  end

  def test_assembler
    asm = Assembler.new(@server.client, max_payload = 1)
    asm.write_header(0, 10)
    asm.write_segment(Segment.new(true, false, 1, 2, 3, "TEST"))
    asm.write_segment(Segment.new(false, true, 1, 2, 3, "ING"))

    assert_equal( ["AMQP", 1, 1, 0, 10], asm.read_header)

    seg = asm.read_segment
    assert_equal(true, seg.first_segment?)
    assert_equal(false, seg.last_segment?)
    assert_equal(1, seg.type)
    assert_equal(2, seg.track)
    assert_equal(3, seg.channel)
    assert_equal("TEST", seg.payload)

    seg = asm.read_segment
    assert_equal(false, seg.first_segment?)
    assert_equal(true, seg.last_segment?)
    assert_equal(1, seg.type)
    assert_equal(2, seg.track)
    assert_equal(3, seg.channel)
    assert_equal("ING", seg.payload)
  end
end
