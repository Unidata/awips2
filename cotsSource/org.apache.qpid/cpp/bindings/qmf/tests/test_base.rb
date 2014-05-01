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

require 'qmf'
require 'socket'

class ConsoleTestBase < Qmf::ConsoleHandler
  def initialize
    @settings = Qmf::ConnectionSettings.new
    @settings.host = ARGV[0] if ARGV.size > 0
    @settings.port = ARGV[1].to_i if ARGV.size > 1
    @connection = Qmf::Connection.new(@settings)
    @qmfc = Qmf::Console.new

    @broker = @qmfc.add_connection(@connection)
    @broker.wait_for_stable

    tests = []
    methods.each do |m|
      name = m.to_s
      tests << name if name[0..4] == "test_"
    end

    failures = 0

    tests.sort.each do |t|
      begin
        print "#{t}..."
        $stdout.flush
        send(t) 
        puts " Pass"
      rescue
        puts " Fail: #{$!}"
        failures += 1
      end
    end

    @qmfc.del_connection(@broker)
    exit(1) if failures > 0
  end

  def assert_equal(left, right, in_text=nil)
    text = " (#{in_text})" if in_text
    raise "Assertion failed: #{left} != #{right}#{text}" unless left == right
  end

  def assert(condition, in_text=nil)
    text = " (#{in_text})" if in_text
    raise "Assertion failed: #{left} != #{right}#{text}" unless condition
  end

  def fail(text)
    raise text
  end
end
