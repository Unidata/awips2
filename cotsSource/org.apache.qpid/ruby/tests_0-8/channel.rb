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
require "qpid/test"
require "qpid"

class Channel < Test::Unit::TestCase

  include Qpid08::Test

  def test_channel_open_close()
    c = connect()
    ch = c.channel(1)
    msg = ch.channel_open()
    assert msg.method.qname == :channel_open_ok
    msg = ch.channel_close()
    assert msg.method.qname == :channel_close_ok
  end

  def test_channel_close()
    c = connect()
    ch = c.channel(1)
    begin
      ch.channel_close()
    rescue Qpid::Closed => e
      assert c.code.method.qname == :connection_close
      assert c.code.reply_code == 504
    end
  end

end
