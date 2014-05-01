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

require "qpid"
require "socket"

broker = if ARGV.length > 0 then ARGV[0] else "localhost" end
port = if ARGV.length > 1 then ARGV[1].to_i else 5672 end
if ARGV.length > 2 then
  puts "usage: hello-world.rb [ <broker> [ <port> ] ]"
  exit 1
end

conn = Qpid::Connection.new(TCPSocket.new(broker, port))
conn.start(10)

ssn = conn.session("test")

# create a queue
ssn.queue_declare("test-queue")

ssn.exchange_declare("test-exchange", :type => "direct")

# Publish a message
dp = ssn.delivery_properties(:routing_key => "test-queue")
mp = ssn.message_properties(:content_type => "text/plain")
msg = Qpid::Message.new(dp, mp, "Hello World!")
ssn.message_transfer(:message => msg)

# subscribe to a queue
ssn.message_subscribe(:destination => "messages", :queue => "test-queue",
                      :accept_mode => ssn.message_accept_mode.none)
incoming = ssn.incoming("messages")

# start incoming message flow
incoming.start()

# grab a message from the queue
p incoming.get(10)

# cancel the subscription and close the session and connection
ssn.message_cancel(:destination => "messages")
ssn.close()
conn.close()
