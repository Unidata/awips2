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
dist_man_MANS = qpidd.1

man_aux = $(dist_man_MANS:.1=.x) 
EXTRA_DIST = $(man_aux) 
DISTCLEANFILES = $(dist_man_MANS)
CLEANFILES=qpidd.1

dist-hook: $(man_aux)

# Depend on configure.ac to get version number changes.
$(dist_man_MANS): $(top_srcdir)/configure.ac

if HAVE_HELP2MAN

SUFFIXES = .x .1
qpidd.1: $(srcdir)/qpidd.x $(top_builddir)/src/qpidd
.x.1:
	@rm -f $@
	@echo "Updating man page $@"
	$(HELP2MAN) --no-info --include=$(srcdir)/$*.x --output=$@-t --help-option="--help --no-module-dir" --version-option="--version --no-module-dir" ../../src/$*
	@chmod a-w $@-t
	@mv $@-t $@
else
qpidd.1:
	@echo "Warning: help2man not available, man page lacks options."
	@cp $(srcdir)/qpidd.x $@
endif


