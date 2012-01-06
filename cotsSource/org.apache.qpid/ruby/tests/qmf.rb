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
require "monitor.rb"

class QmfTest < Test::Unit::TestCase

  class Handler < Qpid::Qmf::Console
    include MonitorMixin

    def initialize
      super()
      @xmt_list = {}
      @rcv_list = {}
    end

    def method_response(broker, seq, response)
      synchronize do
        @rcv_list[seq] = response
      end
    end

    def request(broker, count)
      @count = count
      for idx in 0...count
        synchronize do
          seq = broker.echo(idx, "Echo Message", :async => true)
          @xmt_list[seq] = idx
        end
      end
    end

    def check
      return "fail (attempted send=%d, actual sent=%d)" % [@count, @xmt_list.size] unless @count == @xmt_list.size
      lost = 0
      mismatched = 0
      @xmt_list.each do |seq, value|
        if @rcv_list.include?(seq)
          result = @rcv_list.delete(seq)
          mismatch += 1 unless result.sequence == value
        else
          lost += 1
        end
      end
      spurious = @rcv_list.size
      if lost == 0 and mismatched == 0 and spurious == 0
        return "pass"
      else
        return "fail (lost=%d, mismatch=%d, spurious=%d)" % [lost, mismatched, spurious]
      end
    end
  end

  def setup()
    # Make sure errors in threads lead to a noisy death of the test
    Thread.abort_on_exception = true

    @host = ENV.fetch("QMF_TEST_HOST", 'localhost')
    @port = ENV.fetch("QMF_TEST_PORT", 5672)

    sock = TCPSocket.new(@host, @port)

    @conn = Qpid::Connection.new(sock)
    @conn.start()

    @session = @conn.session("test-session")
  end

  def teardown
    unless @session.error?
      @session.close(10)
    end
    @conn.close(10)
    if @qmf
      @qmf.del_broker(@qmf_broker)
    end
  end

  def start_qmf(kwargs = {})
    @qmf = Qpid::Qmf::Session.new(kwargs)
    @qmf_broker = @qmf.add_broker("amqp://%s:%d" % [@host, @port])

    brokers = @qmf.objects(:class => "broker")
    assert_equal(1, brokers.length)
    @broker = brokers[0]
  end

  def test_methods_sync()
    start_qmf
    body = "Echo Message Body"
    for seq in 1..10
      res = @broker.echo(seq, body, :timeout => 10)
      assert_equal(0, res.status)
      assert_equal("OK", res.text)
      assert_equal(seq, res.sequence)
      assert_equal(body, res.body)
    end
  end

  def test_methods_async()
    handler = Handler.new
    start_qmf(:console => handler)
    handler.request(@broker, 20)
    sleep(1)
    assert_equal("pass", handler.check)
  end

  def test_move_queued_messages()
    """
        Test ability to move messages from the head of one queue to another.
        Need to test moveing all and N messages.
        """

    "Set up source queue"
    start_qmf
    @session.queue_declare(:queue => "src-queue", :exclusive => true, :auto_delete => true)
    @session.exchange_bind(:queue => "src-queue", :exchange => "amq.direct", :binding_key => "routing_key")

    props = @session.delivery_properties(:routing_key => "routing_key")
    for count in 1..20
      body = "Move Message %d" % count
      src_msg = Qpid::Message.new(props, body)
      @session.message_transfer(:destination => "amq.direct", :message => src_msg)
    end

    "Set up destination queue"
    @session.queue_declare(:queue => "dest-queue", :exclusive => true, :auto_delete => true)
    @session.exchange_bind(:queue => "dest-queue", :exchange => "amq.direct")

    queues = @qmf.objects(:class => "queue")

    "Move 10 messages from src-queue to dest-queue"
    result = @qmf.objects(:class => "broker")[0].queueMoveMessages("src-queue", "dest-queue", 10)
    assert_equal(0, result.status)

    sq = @qmf.objects(:class => "queue", "name" => "src-queue")[0]
    dq = @qmf.objects(:class => "queue", "name" => "dest-queue")[0]

    assert_equal(10, sq.msgDepth)
    assert_equal(10, dq.msgDepth)

    "Move all remaining messages to destination"
    result = @qmf.objects(:class => "broker")[0].queueMoveMessages("src-queue", "dest-queue", 0)
    assert_equal(0, result.status)

    sq = @qmf.objects(:class => "queue", 'name' => "src-queue")[0]
    dq = @qmf.objects(:class => "queue", 'name' => "dest-queue")[0]

    assert_equal(0, sq.msgDepth)
    assert_equal(20, dq.msgDepth)

    "Use a bad source queue name"
    result = @qmf.objects(:class => "broker")[0].queueMoveMessages("bad-src-queue", "dest-queue", 0)
    assert_equal(4, result.status)

    "Use a bad destination queue name"
    result = @qmf.objects(:class => "broker")[0].queueMoveMessages("src-queue", "bad-dest-queue", 0)
    assert_equal(4, result.status)

    " Use a large qty (40) to move from dest-queue back to "
    " src-queue- should move all "
    result = @qmf.objects(:class => "broker")[0].queueMoveMessages("dest-queue", "src-queue", 40)
    assert_equal(0, result.status)

    sq = @qmf.objects(:class => "queue", 'name' => "src-queue")[0]
    dq = @qmf.objects(:class => "queue", 'name' => "dest-queue")[0]

    assert_equal(20, sq.msgDepth)
    assert_equal(0, dq.msgDepth)

    "Consume the messages of the queue and check they are all there in order"
    @session.message_subscribe(:queue => "src-queue",
                               :destination => "tag")
    @session.message_flow(:destination => "tag",
                          :unit => @session.message_credit_unit.message,
                          :value => 0xFFFFFFFF)
    @session.message_flow(:destination => "tag",
                          :unit => @session.message_credit_unit.byte,
                          :value => 0xFFFFFFFF)
    queue = @session.incoming("tag")
    for count in 1..20
      consumed_msg = queue.get(timeout=1)
      body = "Move Message %d" % count
      assert_equal(body, consumed_msg.body)
    end
  end

  # Test ability to purge messages from the head of a queue. Need to test
  # moveing all, 1 (top message) and N messages.
  def test_purge_queue
    start_qmf
    # Set up purge queue"
    @session.queue_declare(:queue => "purge-queue",
                           :exclusive => true,
                           :auto_delete => true)
    @session.exchange_bind(:queue => "purge-queue",
                           :exchange => "amq.direct",
                           :binding_key => "routing_key")

    props = @session.delivery_properties(:routing_key => "routing_key")
    20.times do |count|
      body = "Purge Message %d" % count
      msg = Qpid::Message.new(props, body)
      @session.message_transfer(:destination => "amq.direct",
                                :message => msg)
    end

    pq = @qmf.objects(:class => "queue", 'name' => "purge-queue")[0]

    "Purge top message from purge-queue"
    result = pq.purge(1)
    assert_equal(0, result.status)
    pq = @qmf.objects(:class => "queue", 'name' => "purge-queue")[0]
    assert_equal(19, pq.msgDepth)

    "Purge top 9 messages from purge-queue"
    result = pq.purge(9)
    assert_equal(0, result.status)
    pq = @qmf.objects(:class => "queue", 'name' => "purge-queue")[0]
    assert_equal(10, pq.msgDepth)

    "Purge all messages from purge-queue"
    result = pq.purge(0)
    assert_equal(0, result.status)
    pq = @qmf.objects(:class => "queue", 'name' => "purge-queue")[0]
    assert_equal(0, pq.msgDepth)
  end
end
