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

if HAVE_SWIG
if HAVE_PYTHON_DEVEL

EXTRA_DIST = python.i
INCLUDES = -I$(top_srcdir)/include

generated_file_list = saslwrapper.cpp saslwrapper.py
BUILT_SOURCES = $(generated_file_list)

$(generated_file_list): python.i $(top_srcdir)/src/saslwrapper.i
	$(SWIG) -c++ -python -Wall -I/usr/include $(INCLUDES) -o saslwrapper.cpp $(srcdir)/python.i

pylibdir = $(PYTHON_LIB)
lib_LTLIBRARIES = _saslwrapper.la

_saslwrapper_la_LDFLAGS = -avoid-version -module -shared
_saslwrapper_la_LIBADD = $(PYTHON_LIBS) $(top_builddir)/src/libsaslwrapper.la -lsasl2
_saslwrapper_la_CXXFLAGS = -I$(PYTHON_INC)
nodist__saslwrapper_la_SOURCES = saslwrapper.cpp

CLEANFILES = $(generated_file_list)

endif
endif
