#!/usr/bin/env python
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
"""
 client.py

 Client for a client/server example

"""

import qpid
import sys
import os
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
from qpid.queue import Empty

#----- Functions -------------------------------------------

def dump_queue(queue_name):

  print "Messages on queue: " + queue_name 

  message = 0

  while True:
    try:
      message = queue.get(timeout=10)
      content = message.body
      session.message_accept(RangedSet(message.id))
      print "Response: " + content
    except Empty:
      print "No more messages!"
      break
    except:
      print "Unexpected exception!"
      break


#----- Initialization --------------------------------------


#  Set parameters for login

host="127.0.0.1"
port=5672
user="guest"
password="guest"

# If an alternate host or port has been specified, use that instead
# (this is used in our unit tests)
if len(sys.argv) > 1 :
  host=sys.argv[1]
if len(sys.argv) > 2 :
  port=int(sys.argv[2])

#  Create a connection.
socket = connect(host, port)
connection = Connection (sock=socket, username=user, password=password)
connection.start()
session = connection.session(str(uuid4()))


#----- Main Body -- ----------------------------------------

# Create a response queue for the server to send responses to. Use the
# same string as the name of the queue and the name of the routing
# key.

reply_to = "reply_to:" + session.name
session.queue_declare(queue=reply_to, exclusive=True)
session.exchange_bind(exchange="amq.direct", queue=reply_to, binding_key=reply_to)

# Create a local queue and subscribe it to the response queue

local_queue_name = "local_queue"
queue = session.incoming(local_queue_name)

# Call message_subscribe() to tell the broker to deliver messages from
# the server's reply_to queue to our local client queue. The server
# will start delivering messages as soon as message credit is
# available.

session.message_subscribe(queue=reply_to, destination=local_queue_name)
queue.start()

# Send some messages to the server's request queue

lines = ["Twas brillig, and the slithy toves",
         "Did gyre and gimble in the wabe.",
         "All mimsy were the borogroves,",
         "And the mome raths outgrabe."]

# We will use the same reply_to and routing key
# for each message

message_properties = session.message_properties()
message_properties.reply_to = session.reply_to("amq.direct", reply_to)
delivery_properties = session.delivery_properties(routing_key="request")

for line in lines:
  print "Request: " + line
  session.message_transfer(destination="amq.direct", message=Message(message_properties, delivery_properties, line))

# Now see what messages the server sent to our reply_to queue

dump_queue(reply_to)


#----- Cleanup ------------------------------------------------

# Clean up before exiting so there are no open threads.

session.close(timeout=10)
