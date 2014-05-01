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

class MockServer

  def initialize(queue)
    @queue = queue
  end

  def connection(conn, args={})
    return Qpid::Delegate::Server.new(conn, :delegate => method(:session))
  end

  def session(ssn, args={})
    ssn.auto_sync = false
    return MockSession.new(ssn, @queue)
  end
end

class MockSession < Qpid::Session::Delegate

  def initialize(session, queue)
    @session = session
    @queue = queue
  end

  def execution_sync(es)
    nil
  end

  def queue_query(qq)
    return qq.st_type.result.create(qq.queue)
  end

  def message_transfer(cmd, headers, body)
    if cmd.destination == "echo"
      m = Qpid::Message.new(body)
      m.headers = headers
      @session.message_transfer(cmd.destination, cmd.accept_mode,
                                cmd.acquire_mode, m)
    elsif cmd.destination == "abort"
      @session.channel.connection.sock.close()
    else
      @queue.put([cmd, headers, body])
    end
  end

  def exchange_declare(ed)
    # do nothing
  end
end

class TestConnectionTest < Test::Unit::TestCase

  def setup
    # Make sure errors in threads lead to a noisy death of the test
    Thread.abort_on_exception = true

    @queue = Qpid::Queue.new
    @running = true
    ts = MockServer.new(@queue)
    @server = Util::ServerThread.new do |socket|
      conn = Qpid::Connection.new(socket, :delegate => ts.method(:connection))
      begin
        conn.start(5)
      rescue Qpid::Closed
        # Ignore
      end
    end

    class << @server
      def finish
        @running.lock
        client.close
        @sockets.each { |sock| sock.close unless sock.closed? }
      end
    end

    @server[:name] = 'server'
    Thread.current[:name] = 'test'
  end

  def teardown
    @server.finish
    @server.join
  end

  def connect
    sock = @server.client
    return Qpid::Connection.new(sock)
  end

  def test_basic
    c = connect
    c.start(10)

    ssn1 = c.session("test1", :timeout => 10)
    ssn2 = c.session("test2", :timeout => 10)

    assert_equal(c.sessions["test1"], ssn1)
    assert_equal(c.sessions["test2"], ssn2)
    assert_not_nil ssn1.channel
    assert_not_nil ssn2.channel
    assert(c.attached.values.include?(ssn1))
    assert(c.attached.values.include?(ssn2))

    ssn1.close(5)

    assert_nil(ssn1.channel)
    assert(! c.attached.values.include?(ssn1))
    assert(c.sessions.values.include?(ssn2))

    ssn2.close(5)

    assert_nil(ssn2.channel)
    assert(! c.attached.values.include?(ssn2))
    assert(! c.sessions.values.include?(ssn2))

    ssn = c.session("session", :timeout => 10)

    assert_not_nil(ssn.channel)
    assert(c.sessions.values.include?(ssn))

    destinations = ["one", "two", "three"]

    destinations.each { |d| ssn.message_transfer(d) }

    destinations.each do |d|
      cmd, header, body = @queue.get(10)
      assert_equal(d, cmd.destination)
      assert_nil(header)
      assert_nil(body)
    end

    msg = Qpid::Message.new("this is a test")
    ssn.message_transfer("four", :message => msg)
    cmd, header, body = @queue.get(10)
    assert_equal("four", cmd.destination)
    assert_nil(header)
    assert_equal(msg.body, body)

    qq = ssn.queue_query("asdf")
    assert_equal("asdf", qq.queue)
    c.close(5)
  end

  def test_close_get
    c = connect
    c.start(10)
    ssn = c.session("test", :timeout => 10)
    echos = ssn.incoming("echo")

    10.times do |i|
      ssn.message_transfer("echo",
                           :message => Qpid::Message.new("test#{i}"))
    end

    ssn.auto_sync=false
    ssn.message_transfer("abort")

    10.times do |i|
      m = echos.get(timeout=10)
      assert_equal("test#{i}", m.body)
    end

    begin
      m = echos.get(timeout=10)
      flunk("Expected Closed")
    rescue Qpid::Closed
      # Ignore
    end
  end

  def test_close_listen
    c = connect
    c.start(10)
    ssn = c.session("test", :timeout => 10)
    echos = ssn.incoming("echo")

    messages = []
    exceptions = []
    lock = Monitor.new
    condition = lock.new_cond

    echos.exc_listen do |e|
      exceptions << e
      lock.synchronize { condition.signal }
    end
    echos.listen do |m|
      messages << m
    end

    10.times do |i|
      ssn.message_transfer("echo",
                           :message => Qpid::Message.new("test#{i}"))
    end
    ssn.auto_sync=false
    ssn.message_transfer("abort")

    lock.synchronize { condition.wait(10) }

    10.times do |i|
      m = messages.shift
      assert_equal("test#{i}", m.body)
    end

    assert_equal(1, exceptions.size)
  end

  def test_sync
    c = connect
    c.start(10)
    s = c.session("test")
    s.auto_sync = false
    s.message_transfer("echo",
                       :message => Qpid::Message.new("test"))
    s.sync(10)
  end

  def test_exchange_declare
    c = connect
    c.start(10)
    s = c.session("test")
    s.exchange_declare("test-exchange")
  end
end
