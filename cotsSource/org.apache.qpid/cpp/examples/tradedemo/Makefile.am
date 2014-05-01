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
examplesdir=$(pkgdatadir)/examples/tradedemo

MAKELDFLAGS=$(CLIENTFLAGS)
include $(top_srcdir)/examples/makedist.mk

noinst_PROGRAMS=topic_listener topic_publisher declare_queues

topic_listener_SOURCES=topic_listener.cpp
topic_listener_LDADD=$(CLIENT_LIB)

topic_publisher_SOURCES=topic_publisher.cpp 
topic_publisher_LDADD=$(CLIENT_LIB)

declare_queues_SOURCES=declare_queues.cpp
declare_queues_LDADD=$(CLIENT_LIB)


examples_DATA=               \
	topic_listener.cpp   \
	topic_publisher.cpp  \
	declare_queues.cpp  \
	$(MAKEDIST)

EXTRA_DIST=                  \
	CMakeLists.txt       \
	tradedemo_declare_queues.vcproj \
	tradedemo_topic_listener.vcproj \
	tradedemo_topic_publisher.vcproj
