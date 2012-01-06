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
SUBDIRS = direct fanout pub-sub request-response failover qmf-console tradedemo messaging
if HAVE_XML
  SUBDIRS += xml-exchange
  broker_args = "--no-module-dir --data-dir \"\" --auth no --load-module $(top_builddir)/src/.libs/xml.so"	
endif
if !HAVE_XML
  exclude_examples_regexp="xml"	# Exclude XML examples.
  broker_args = "--no-module-dir --data-dir \"\" --auth no"	
endif

MAKEDIST=.libs/Makefile

$(MAKEDIST): Makefile
	mkdir -p .libs
	@(echo 'all clean:' ; \
	echo '	for d in $(SUBDIRS) ; do $$(MAKE) -C $$$$d $$@ ; done' ; \
	) > $(MAKEDIST)

examplesdir=$(pkgdatadir)/examples
dist_examples_DATA = README.txt $(MAKEDIST)

EXTRA_DIST = README.verify verify verify_all examples.sln CMakeLists.txt

# For older versions of automake
abs_top_srcdir = @abs_top_srcdir@
abs_top_builddir = @abs_top_builddir@

# Verify the examples in the buid tree.
check-local: 
	$(srcdir)/verify_all $(abs_top_srcdir)/.. $(abs_top_builddir) $(broker_args) $(exclude_examples_regexp)

