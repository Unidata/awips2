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

require 'thread'
require 'socket'

module Util

  TOPDIR = File::dirname(File::dirname(File::expand_path(__FILE__)))
  SPEC = File::join(TOPDIR, "specs", "amqp.0-10-qpid-errata.xml")

  PORT = 1234
  HOST = "0.0.0.0"

  def self.connect(host = HOST, port = PORT)
    TCPSocket.new(host, port)
  end

  class ServerThread < Thread
    def initialize(&block)
      @sockets = []
      @running = Mutex.new
      started = Qpid::Util::Event.new
      super(started, @running) do |started, running|
        tcp_srv = TCPServer.new(HOST, PORT)
        begin
          started.set
          while ! running.locked? and (session = tcp_srv.accept)
            yield(session)
          end
        rescue Exception => e
          # Exceptions in the server thread are hard to see
          # Make sure they apear loudly on the console
          $stderr.puts "#{ "*" * 20} Server exception #{ "*" * 20}"
          $stderr.puts e.message
          $stderr.puts e.backtrace
          raise
        ensure
          tcp_srv.close
        end
      end
      started.wait
    end

    def finish
      @running.lock
      @sockets.each { |sock| sock.close unless sock.closed? }
    end

    def client(host = HOST, port = PORT)
      sock = Util::connect(host, port)
      @sockets << sock
      sock
    end
  end
end
