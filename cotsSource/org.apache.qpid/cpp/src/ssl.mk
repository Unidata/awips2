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
# Makefile fragment, conditionally included in Makefile.am
# 
libsslcommon_la_SOURCES = \
  qpid/sys/ssl/check.h \
  qpid/sys/ssl/check.cpp \
  qpid/sys/ssl/util.h \
  qpid/sys/ssl/util.cpp \
  qpid/sys/ssl/SslSocket.h \
  qpid/sys/ssl/SslSocket.cpp \
  qpid/sys/ssl/SslIo.h \
  qpid/sys/ssl/SslIo.cpp

libsslcommon_la_LIBADD= -lnss3 -lssl3 -lnspr4 libqpidcommon.la

libsslcommon_la_CXXFLAGS=$(AM_CXXFLAGS) $(SSL_CFLAGS)

lib_LTLIBRARIES +=  libsslcommon.la

ssl_la_SOURCES = \
  qpid/sys/SslPlugin.cpp \
  qpid/sys/ssl/SslHandler.h \
  qpid/sys/ssl/SslHandler.cpp

ssl_la_LIBADD= libqpidbroker.la libsslcommon.la

ssl_la_CXXFLAGS=$(AM_CXXFLAGS) $(SSL_CFLAGS)

ssl_la_LDFLAGS = $(PLUGINLDFLAGS)

dmodule_LTLIBRARIES += ssl.la


sslconnector_la_SOURCES = \
  qpid/client/SslConnector.cpp

sslconnector_la_LIBADD = \
  libqpidclient.la \
  libsslcommon.la

sslconnector_la_CXXFLAGS = $(AM_CXXFLAGS) -DQPIDC_CONF_FILE=\"$(confdir)/qpidc.conf\"  $(SSL_CFLAGS)

sslconnector_la_LDFLAGS = $(PLUGINLDFLAGS)

cmodule_LTLIBRARIES += \
  sslconnector.la
