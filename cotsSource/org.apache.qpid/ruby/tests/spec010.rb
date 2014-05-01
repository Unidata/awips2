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
require "qpid/spec010"

class SpecTest < Test::Unit::TestCase

  def setup()
    @spec = Qpid::Spec010.load()
  end

  def testSessionHeader()
    hdr = @spec[:header]
    sc = Qpid::StringCodec.new(@spec)
    hdr.encode(sc, Qpid::struct(hdr, :sync=>true))
    assert sc.encoded == "\x01\x01"

    sc = Qpid::StringCodec.new(@spec)
    hdr.encode(sc, Qpid::struct(hdr, :sync=>false))
    assert sc.encoded == "\x01\x00"
  end

  def encdec(type, value)
    sc = Qpid::StringCodec.new(@spec)
    type.encode(sc, value)
    decoded = type.decode(sc)
    return decoded
  end

  def testMessageProperties()
    mp = @spec[:message_properties]
    rt = @spec[:reply_to]

    props = Qpid::struct(mp,
                         :content_length=>3735928559,
                         :reply_to=>Qpid::struct(rt,
                                                 :exchange=>"the exchange name",
                                                 :routing_key=>"the routing key"))
    dec = encdec(mp, props)
    assert props.content_length == dec.content_length
    assert props.reply_to.exchange == dec.reply_to.exchange
    assert props.reply_to.routing_key == dec.reply_to.routing_key
  end

  def testMessageSubscribe()
    ms = @spec[:message_subscribe]
    cmd = Qpid::struct(ms, :exclusive=>true, :destination=>"this is a test")
    dec = encdec(@spec[:message_subscribe], cmd)
    assert cmd.exclusive == dec.exclusive
    assert cmd.destination == dec.destination
  end

  def testXid()
    xid = @spec[:xid]
    sc = Qpid::StringCodec.new(@spec)
    st = Qpid::struct(xid, :format=>0, :global_id=>"gid", :branch_id=>"bid")
    xid.encode(sc, st)
    assert sc.encoded == "\x00\x00\x00\x10\x06\x04\x07\x00\x00\x00\x00\x00\x03gid\x03bid"
    assert xid.decode(sc) == st
  end

end
