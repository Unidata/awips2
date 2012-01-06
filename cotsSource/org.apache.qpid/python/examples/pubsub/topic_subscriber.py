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
 topic_subscriber.py

 This subscriber creates private queues and binds them
 to the topics 'usa.#', 'europe.#', '#.news', and '#.weather'.
"""

import qpid
import sys
import os
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
from qpid.queue import Empty

#----- Functions -------------------------------------------

def dump_queue(queue):

  content = ""		         # Content of the last message read
  final = "That's all, folks!"   # In a message body, signals the last message
  message = 0

  while content != final:
    try:
      message = queue.get(timeout=10)
      content = message.body
      session.message_accept(RangedSet(message.id)) 
      print content
    except Empty:
      print "No more messages!"
      return



def subscribe_queue(server_queue_name, local_queue_name):

  print "Subscribing local queue '" + local_queue_name + "' to " + server_queue_name + "'"

  queue = session.incoming(local_queue_name)

  session.message_subscribe(queue=server_queue_name, destination=local_queue_name)
  queue.start()

  return queue

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

# declare queues on the server

news = "news-" + session.name
weather = "weather-" + session.name
usa = "usa-" + session.name
europe = "europe-" + session.name

session.queue_declare(queue=news, exclusive=True)
session.queue_declare(queue=weather, exclusive=True)
session.queue_declare(queue=usa, exclusive=True)
session.queue_declare(queue=europe, exclusive=True)

# Routing keys may be "usa.news", "usa.weather", "europe.news", or "europe.weather".

# The '#' symbol matches one component of a multipart name, e.g. "#.news" matches
# "europe.news" or "usa.news".

session.exchange_bind(exchange="amq.topic", queue=news, binding_key="#.news")
session.exchange_bind(exchange="amq.topic", queue=weather, binding_key="#.weather")
session.exchange_bind(exchange="amq.topic", queue=usa, binding_key="usa.#")
session.exchange_bind(exchange="amq.topic", queue=europe, binding_key="europe.#")

# Bind each queue to the control queue so we know when to stop

session.exchange_bind(exchange="amq.topic", queue=news, binding_key="control")
session.exchange_bind(exchange="amq.topic", queue=weather, binding_key="control")
session.exchange_bind(exchange="amq.topic", queue=usa, binding_key="control")
session.exchange_bind(exchange="amq.topic", queue=europe, binding_key="control")

# Remind the user to start the topic producer

print "Queues created - please start the topic producer"
sys.stdout.flush()

# Subscribe local queues to server queues

local_news = "local_news"
local_weather = "local_weather"
local_usa = "local_usa" 
local_europe = "local_europe"

local_news_queue = subscribe_queue(news, local_news)
local_weather_queue = subscribe_queue(weather, local_weather)
local_usa_queue = subscribe_queue(usa, local_usa)
local_europe_queue = subscribe_queue(europe, local_europe)

# Call dump_queue to print messages from each queue

print "Messages on 'news' queue:"
dump_queue(local_news_queue)

print "Messages on 'weather' queue:"
dump_queue(local_weather_queue)

print "Messages on 'usa' queue:"
dump_queue(local_usa_queue)

print "Messages on 'europe' queue:"
dump_queue(local_europe_queue)

#----- Cleanup ------------------------------------------------

# Clean up before exiting so there are no open threads.

session.close(timeout=10)
