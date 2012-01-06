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
# qmf library makefile fragment, to be included in Makefile.am
# 
lib_LTLIBRARIES +=	\
  libqmf.la		\
  libqmfengine.la

#
# Public headers for the QMF API
#
QMF_API =					\
  ../include/qpid/agent/ManagementAgent.h	\
  ../include/qpid/agent/QmfAgentImportExport.h	\
  ../include/qmf/Agent.h			\
  ../include/qmf/Connection.h			\
  ../include/qmf/QmfImportExport.h		\
  ../include/qmf/ConnectionSettings.h		\
  ../include/qmf/AgentObject.h

#
# Public headers for the QMF Engine API
#
QMF_ENGINE_API =				\
  ../include/qmf/engine/Agent.h			\
  ../include/qmf/engine/ConnectionSettings.h	\
  ../include/qmf/engine/Console.h		\
  ../include/qmf/engine/Event.h			\
  ../include/qmf/engine/Message.h		\
  ../include/qmf/engine/Object.h		\
  ../include/qmf/engine/ObjectId.h		\
  ../include/qmf/engine/QmfEngineImportExport.h	\
  ../include/qmf/engine/Query.h			\
  ../include/qmf/engine/ResilientConnection.h	\
  ../include/qmf/engine/Schema.h		\
  ../include/qmf/engine/Typecode.h		\
  ../include/qmf/engine/Value.h

# Public header files
nobase_include_HEADERS +=	\
  $(QMF_API)			\
  $(QMF_ENGINE_API)

libqmf_la_SOURCES =			\
  $(QMF_API)				\
  qpid/agent/ManagementAgentImpl.cpp	\
  qpid/agent/ManagementAgentImpl.h

libqmfengine_la_SOURCES =			\
  $(QMF_ENGINE_API)				\
  qmf/engine/Agent.cpp				\
  qmf/engine/BrokerProxyImpl.cpp		\
  qmf/engine/BrokerProxyImpl.h			\
  qmf/engine/ConnectionSettingsImpl.cpp		\
  qmf/engine/ConnectionSettingsImpl.h		\
  qmf/engine/ConsoleImpl.cpp			\
  qmf/engine/ConsoleImpl.h			\
  qmf/engine/MessageImpl.cpp			\
  qmf/engine/MessageImpl.h			\
  qmf/engine/ObjectIdImpl.cpp			\
  qmf/engine/ObjectIdImpl.h			\
  qmf/engine/ObjectImpl.cpp			\
  qmf/engine/ObjectImpl.h			\
  qmf/engine/Protocol.cpp			\
  qmf/engine/Protocol.h				\
  qmf/engine/QueryImpl.cpp			\
  qmf/engine/QueryImpl.h			\
  qmf/engine/ResilientConnection.cpp		\
  qmf/engine/SequenceManager.cpp		\
  qmf/engine/SequenceManager.h			\
  qmf/engine/SchemaImpl.cpp			\
  qmf/engine/SchemaImpl.h			\
  qmf/engine/ValueImpl.cpp			\
  qmf/engine/ValueImpl.h

libqmf_la_LIBADD = libqmfengine.la
libqmfengine_la_LIBADD = libqpidclient.la

# Library Version Information:
#
#  CURRENT  => API/ABI version.  Bump this if the interface changes
#  REVISION => Version of underlying implementation.
#              Bump if implementation changes but API/ABI doesn't
#  AGE      => Number of API/ABI versions this is backward compatible with
#
QMF_CURRENT  = 1
QMF_REVISION = 0
QMF_AGE      = 0

QMF_ENGINE_CURRENT  = 1
QMF_ENGINE_REVISION = 1
QMF_ENGINE_AGE      = 0

libqmf_la_LDFLAGS = -version-info $(QMF_CURRENT):$(QMF_REVISION):$(QMF_AGE)
libqmfengine_la_LDFLAGS = -version-info $(QMF_ENGINE_CURRENT):$(QMF_ENGINE_REVISION):$(QMF_ENGINE_AGE)
