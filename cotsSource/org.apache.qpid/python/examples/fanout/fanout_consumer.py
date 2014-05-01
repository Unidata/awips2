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
 fanout_consumer.py

 This AMQP client reads messages from a message
 queue named "message_queue".
"""
import qpid
import sys
import os
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
from qpid.queue import Empty

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


#----- Main Body -------------------------------------------

# Create a server-side queue and route messages to it.
# The server-side queue must have a unique name. Use the
# session id for that.
server_queue_name = session.name
session.queue_declare(queue=server_queue_name)
session.exchange_bind(queue=server_queue_name, exchange="amq.fanout")

# Create a local queue to receive messages from the server-side
# queue.
local_queue_name = "local_queue"
local_queue = session.incoming(local_queue_name)

# Call message_subscribe() to tell the server to deliver messages
# from the AMQP queue to this local client queue. 

session.message_subscribe(queue=server_queue_name, destination=local_queue_name)
local_queue.start()

print "Subscribed to queue " + server_queue_name
sys.stdout.flush()

#  Initialize 'final' and 'content', variables used to identify the last message.
final = "That's all, folks!"   # In a message body, signals the last message
content = ""		       # Content of the last message read

# Read the messages - acknowledge each one
message = None
while content != final:
	message = local_queue.get(timeout=10)
	content = message.body          
        session.message_accept(RangedSet(message.id))
	print content


#----- Cleanup ------------------------------------------------

# Clean up before exiting so there are no open threads.
#

session.close(timeout=10)
