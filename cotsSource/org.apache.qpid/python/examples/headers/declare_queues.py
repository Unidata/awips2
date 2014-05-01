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
 declare_queues.py 

 Creates and binds a queue on an AMQP headers exchange.

 All messages with an application header of {'class': 'first'} are sent to queue "first".
 All messages with an application header of {'class': 'second'} are sent to queue "second".
"""

# Common includes

import qpid
import sys
import os
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
from qpid.queue import Empty

#----- Initialization -----------------------------------

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

#----- Create queues -------------------------------------

# queue_declare() creates an AMQP queue, which is held
# on the broker. Published messages are sent to the AMQP queue, 
# from which messages are delivered to consumers. 
# 
# exchange_bind() determines which messages are routed to a queue. 

session.queue_declare(queue="first")
session.exchange_bind(exchange="amq.match", queue="first", arguments={'x-match':'any', 'class':'first'})

session.queue_declare(queue="second")
session.exchange_bind(exchange="amq.match", queue="second", arguments={'x-match':'any', 'class':'second'})

#----- Cleanup ---------------------------------------------

session.close(timeout=10)
