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

examplesdir=$(pkgdatadir)/examples/qmf-console

MAKELDFLAGS=$(CONSOLEFLAGS)
include $(top_srcdir)/examples/makedist.mk

noinst_PROGRAMS=console printevents ping queuestats cluster-qmon

console_SOURCES=console.cpp
console_LDADD=$(CONSOLE_LIB)

printevents_SOURCES=printevents.cpp
printevents_LDADD=$(CONSOLE_LIB)

ping_SOURCES=ping.cpp
ping_LDADD=$(CONSOLE_LIB)

queuestats_SOURCES=queuestats.cpp
queuestats_LDADD=$(CONSOLE_LIB)

cluster_qmon_SOURCES=cluster-qmon.cpp
cluster_qmon_LDADD=$(CONSOLE_LIB)

examples_DATA= \
	console.cpp \
	printevents.cpp \
	ping.cpp \
	queuestats.cpp \
	cluster-qmon.cpp \
	$(MAKEDIST)

EXTRA_DIST=                  \
	CMakeLists.txt \
	qmf-console_console.vcproj \
	qmf-console_ping.vcproj \
	qmf-console_printevents.vcproj \
	qmf-console_queuestats.vcproj
