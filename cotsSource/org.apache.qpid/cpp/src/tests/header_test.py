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

import qpid
import sys
import os
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
from qpid.queue import Empty
from math import fabs

def getApplicationHeaders(msg):
    for h in msg.headers:
        if hasattr(h, 'application_headers'): return getattr(h, 'application_headers')
    return None            

#  Set parameters for login

host="127.0.0.1"
port=5672
user="guest"
password="guest"

if len(sys.argv) > 1 :
    host=sys.argv[1]
if len(sys.argv) > 2 :
    port=int(sys.argv[2])

#  Create a connection.
socket = connect(host, port)
connection = Connection (sock=socket)
connection.start()
session = connection.session(str(uuid4()))

q = "header_interop_test_queue"
session.queue_declare(queue=q)

session.message_subscribe(queue=q, destination="received")
queue = session.incoming("received")
queue.start()

msg = queue.get(timeout=10)
pi = 3.14159265
e = 2.71828

headers = getApplicationHeaders(msg)
pi_ = headers["pi"]
e_ = headers["e"]
session.close(timeout=10)

failed = False

if pi != pi_:
    print "got incorrect value for pi: ", pi_, " expected:", pi
    failed = True

if fabs(e - e_) > 0.0001:
    print "got incorrect value for e: ", e_, " expected:", e
    failed = True

if failed:
    sys.exit(1)
else:
    print "Correct header values received."
    sys.exit(0)
    


