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
examplesdir=$(pkgdatadir)/examples/failover

MAKELDFLAGS=$(CLIENTFLAGS)
include $(top_srcdir)/examples/makedist.mk

noinst_PROGRAMS=declare_queues resuming_receiver replaying_sender

declare_queues_SOURCES=declare_queues.cpp
declare_queues_LDADD=$(CLIENT_LIB)

resuming_receiver_SOURCES=resuming_receiver.cpp
resuming_receiver_LDADD=$(CLIENT_LIB)

replaying_sender_SOURCES=replaying_sender.cpp
replaying_sender_LDADD=$(CLIENT_LIB)

examples_DATA=                \
	declare_queues.cpp    \
	resuming_receiver.cpp \
	replaying_sender.cpp  \
        $(MAKEDIST)

# FIXME aconway 2008-10-10: add verify scripts.

EXTRA_DIST=	             \
	CMakeLists.txt       \
	failover_declare_queues.vcproj \
	failover_replaying_sender.vcproj \
	failover_resuming_receiver.vcproj
