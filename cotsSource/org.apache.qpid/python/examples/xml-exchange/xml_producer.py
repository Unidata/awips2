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
 xml_producer.py

 Publishes messages to an XML exchange, using
 the routing key "weather"
"""


import qpid
import sys
import os
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
from qpid.queue import Empty

#----- Functions ----------------------------------------

# Data for weather reports

station = ("Raleigh-Durham International Airport (KRDU)", 
           "New Bern, Craven County Regional Airport (KEWN)", 
           "Boone, Watauga County Hospital Heliport (KTNB)",
           "Hatteras, Mitchell Field (KHSE)")
wind_speed_mph = ( 0, 2, 5, 10, 16, 22, 28, 35, 42, 51, 61, 70, 80 )
temperature_f = ( 30, 40, 50, 60, 70, 80, 90, 100 )
dewpoint = ( 35, 40, 45, 50 )

def pick_one(list, i):
  return str( list [ i % len(list)] )

def report(i):
  return "<weather>" + "<station>" + pick_one(station,i)+ "</station>"  + "<wind_speed_mph>" + pick_one(wind_speed_mph,i) + "</wind_speed_mph>"  + "<temperature_f>" + pick_one(temperature_f,i) + "</temperature_f>" + "<dewpoint>" + pick_one(dewpoint,i) + "</dewpoint>" + "</weather>"


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

#----- Publish some messages ------------------------------

# Create some messages and put them on the broker.

props = session.delivery_properties(routing_key="weather")

for i in range(10):
  print report(i)
  session.message_transfer(destination="xml", message=Message(props, report(i)))


#----- Cleanup --------------------------------------------

# Clean up before exiting so there are no open threads.

session.close()
