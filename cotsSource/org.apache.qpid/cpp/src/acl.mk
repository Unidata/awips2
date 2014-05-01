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
# acl library makefile fragment, to be included in Makefile.am
# 
dmodule_LTLIBRARIES += acl.la

acl_la_SOURCES = \
  qpid/acl/Acl.cpp \
  qpid/acl/Acl.h \
  qpid/acl/AclData.cpp \
  qpid/acl/AclData.h \
  qpid/acl/AclPlugin.cpp \
  qpid/acl/AclReader.cpp \
  qpid/acl/AclReader.h

acl_la_LIBADD = libqpidbroker.la
if SUNOS
  acl_la_LIBADD +=  libqmfagent.la libqmfconsole.la libqpidcommon.la -lboost_program_options $(SUNCC_RUNTIME_LIBS)
endif

acl_la_LDFLAGS = $(PLUGINLDFLAGS)
