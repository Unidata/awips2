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
examplesdir=$(pkgdatadir)/examples/messaging

MAKELDFLAGS=$(CLIENTFLAGS)
include $(top_srcdir)/examples/makedist.mk

noinst_PROGRAMS=drain spout queue_sender queue_receiver topic_sender topic_receiver client server map_sender map_receiver

drain_SOURCES=drain.cpp
drain_LDADD=$(CLIENT_LIB)

spout_SOURCES=spout.cpp
spout_LDADD=$(CLIENT_LIB)

queue_sender_SOURCES=queue_sender.cpp
queue_sender_LDADD=$(CLIENT_LIB)

queue_receiver_SOURCES=queue_receiver.cpp
queue_receiver_LDADD=$(CLIENT_LIB)

topic_sender_SOURCES=topic_sender.cpp
topic_sender_LDADD=$(CLIENT_LIB)

topic_receiver_SOURCES=topic_receiver.cpp
topic_receiver_LDADD=$(CLIENT_LIB)

client_SOURCES=client.cpp
client_LDADD=$(CLIENT_LIB)

server_SOURCES=server.cpp
server_LDADD=$(CLIENT_LIB)

map_sender_SOURCES=map_sender.cpp
map_sender_LDADD=$(CLIENT_LIB)

map_receiver_SOURCES=map_receiver.cpp
map_receiver_LDADD=$(CLIENT_LIB)

EXTRA_DIST=                  \
	CMakeLists.txt \
	messaging_client.vcproj \
	messaging_map_receiver.vcproj \
	messaging_map_sender.vcproj \
	messaging_queue_listener.vcproj \
	messaging_queue_receiver.vcproj \
	messaging_queue_sender.vcproj \
	messaging_server.vcproj \
	messaging_topic_listener.vcproj \
	messaging_topic_receiver.vcproj \
	messaging_topic_sender.vcproj
