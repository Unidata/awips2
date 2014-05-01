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

class TestFramer < Test::Unit::TestCase

  include Test

  def setup
    #Qpid::raw_logger = Logger.new(STDOUT)
    #Qpid::frm_logger = Logger.new(STDOUT)

    @server = Util::ServerThread.new do |socket|
      conn = Qpid::Framer.new(socket)
      begin
        h = conn.read_header
        conn.write_header(h[-2], h[-1])
        loop do
          frame = conn.read_frame
          conn.write_frame(frame)
          conn.flush
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

  Frame = Qpid::Frame

  def test_framer
    c = Qpid::Framer.new(@server.client)

    c.write_header(0, 10)
    assert_equal( ["AMQP", 1, 1, 0, 10], c.read_header())

    c.write_frame(Frame.new(Qpid::FIRST_FRM, 1, 2, 3, "THIS"))
    c.write_frame(Frame.new(0, 1, 2, 3, "IS"))
    c.write_frame(Frame.new(0, 1, 2, 3, "A"))
    c.write_frame(Frame.new(Qpid::LAST_FRM, 1, 2, 3, "TEST"))
    c.flush()

    f = c.read_frame
    assert(f.first_frame?)
    assert(! f.last_frame?)
    assert_equal(1, f.type)
    assert_equal(2, f.track)
    assert_equal(3, f.channel)
    assert_equal("THIS", f.payload)

    f = c.read_frame
    assert_equal(0, f.flags)
    assert_equal(1, f.type)
    assert_equal(2, f.track)
    assert_equal(3, f.channel)
    assert_equal("IS", f.payload)

    f = c.read_frame
    assert_equal(0, f.flags)
    assert_equal(1, f.type)
    assert_equal(2, f.track)
    assert_equal(3, f.channel)
    assert_equal("A", f.payload)

    f = c.read_frame
    assert(f.last_frame?)
    assert(! f.first_frame?)
    assert_equal(1, f.type)
    assert_equal(2, f.track)
    assert_equal(3, f.channel)
    assert_equal("TEST", f.payload)
  end
end
