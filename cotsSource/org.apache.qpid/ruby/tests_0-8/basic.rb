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

class Basic < Test::Unit::TestCase

  include Qpid08::Test

  def publish(body, headers = {})
    cli = connect()
    ch = cli.channel(1)
    ch.channel_open()
    content = Qpid08::Content.new(headers, body)
    ch.basic_publish(:content => content)
    msg = ch.channel_close()
    assert msg.method.qname == :channel_close_ok
  end

  def consume(body, headers = {})
    cli = connect()
    ch = cli.channel(1)
    ch.channel_open()
    ch.queue_declare(:queue => "test-queue")
    ch.queue_bind(:queue_name => "test-queue")
    ch.basic_consume(:queue => "test-queue", :consumer_tag => "ctag")
    content = Qpid08::Content.new(headers, body)
    ch.basic_publish(:routing_key => "test-queue", :content => content)
    queue = cli.queue("ctag")
    msg = queue.pop()
    assert content.headers == msg.content.headers
    assert content.body == msg.content.body
    assert content.children == msg.content.children
    ch.basic_ack(msg.delivery_tag)
    msg = ch.channel_close()
    assert msg.method.qname == :channel_close_ok
  end

  def test_publish(); publish("hello world") end

  def test_publish_empty(); publish("") end

  def test_publish_headers(); publish("hello world", :content_type => "text/plain") end

  def test_consume(); consume("hello world") end

  def test_consume_empty(); consume("") end

  def test_consume_headers(); consume("hello_world", :content_type => "text/plain") end

end
