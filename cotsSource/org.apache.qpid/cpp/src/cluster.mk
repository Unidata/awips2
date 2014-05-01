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
#
# Cluster library makefile fragment, to be included in Makefile.am
# 

# Optional CMAN support

# Distribute all sources.
EXTRA_DIST += qpid/cluster/Quorum_cman.h qpid/cluster/Quorum_cman.cpp qpid/cluster/Quorum_null.h

if HAVE_LIBCMAN
CMAN_SOURCES = qpid/cluster/Quorum_cman.h qpid/cluster/Quorum_cman.cpp
libcman = -lcman
else
CMAN_SOURCES = qpid/cluster/Quorum_null.h
endif

if HAVE_LIBCPG

dmodule_LTLIBRARIES += cluster.la

cluster_la_SOURCES =				\
  $(CMAN_SOURCES)				\
  qpid/cluster/Cluster.cpp			\
  qpid/cluster/Cluster.h			\
  qpid/cluster/Decoder.cpp			\
  qpid/cluster/Decoder.h			\
  qpid/cluster/PollableQueue.h			\
  qpid/cluster/ClusterMap.cpp			\
  qpid/cluster/ClusterMap.h			\
  qpid/cluster/ClusterPlugin.cpp		\
  qpid/cluster/ClusterSettings.h		\
  qpid/cluster/Connection.cpp			\
  qpid/cluster/Connection.h			\
  qpid/cluster/ConnectionCodec.cpp		\
  qpid/cluster/ConnectionCodec.h		\
  qpid/cluster/Cpg.cpp				\
  qpid/cluster/Cpg.h				\
  qpid/cluster/Dispatchable.h			\
  qpid/cluster/UpdateClient.cpp			\
  qpid/cluster/UpdateClient.h			\
  qpid/cluster/RetractClient.cpp		\
  qpid/cluster/RetractClient.h			\
  qpid/cluster/ErrorCheck.cpp			\
  qpid/cluster/ErrorCheck.h			\
  qpid/cluster/Event.cpp			\
  qpid/cluster/Event.h				\
  qpid/cluster/EventFrame.h			\
  qpid/cluster/EventFrame.cpp			\
  qpid/cluster/ExpiryPolicy.h			\
  qpid/cluster/ExpiryPolicy.cpp			\
  qpid/cluster/FailoverExchange.cpp		\
  qpid/cluster/FailoverExchange.h		\
  qpid/cluster/UpdateExchange.h			\
  qpid/cluster/UpdateExchange.cpp		\
  qpid/cluster/UpdateReceiver.h			\
  qpid/cluster/LockedConnectionMap.h		\
  qpid/cluster/Multicaster.cpp			\
  qpid/cluster/Multicaster.h			\
  qpid/cluster/McastFrameHandler.h		\
  qpid/cluster/NoOpConnectionOutputHandler.h	\
  qpid/cluster/Numbering.h			\
  qpid/cluster/OutputInterceptor.cpp		\
  qpid/cluster/OutputInterceptor.h		\
  qpid/cluster/PollerDispatch.cpp		\
  qpid/cluster/PollerDispatch.h			\
  qpid/cluster/ProxyInputHandler.h		\
  qpid/cluster/Quorum.h				\
  qpid/cluster/InitialStatusMap.h		\
  qpid/cluster/InitialStatusMap.cpp		\
  qpid/cluster/MemberSet.h			\
  qpid/cluster/MemberSet.cpp			\
  qpid/cluster/types.h				\
  qpid/cluster/StoreStatus.h			\
  qpid/cluster/StoreStatus.cpp

cluster_la_LIBADD=  -lcpg $(libcman) libqpidbroker.la libqpidclient.la
cluster_la_CXXFLAGS = $(AM_CXXFLAGS) -fno-strict-aliasing
cluster_la_LDFLAGS = $(PLUGINLDFLAGS)

# The watchdog plugin and helper executable
dmodule_LTLIBRARIES += watchdog.la
watchdog_la_SOURCES = qpid/cluster/WatchDogPlugin.cpp
watchdog_la_LIBADD = libqpidbroker.la
watchdog_la_LDFLAGS = $(PLUGINLDFLAGS)

qpidexec_PROGRAMS += qpidd_watchdog
qpidd_watchdog_SOURCES = qpid/cluster/qpidd_watchdog.cpp

endif				# HAVE_LIBCPG
