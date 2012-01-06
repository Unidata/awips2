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
SASL_CONF = sasl2/qpidd.conf

EXTRA_DIST = \
	$(SASL_CONF) \
	qpidd qpidd.conf qpidc.conf CMakeLists.txt

confdir=$(sysconfdir)/qpid
nobase_conf_DATA=\
	qpidc.conf

nobase_sysconf_DATA = \
	qpidd.conf

if HAVE_SASL
SASL_DB = qpidd.sasldb

nobase_sysconf_DATA += \
	$(SASL_CONF)

sasldbdir = $(localstatedir)/lib/qpidd
sasldb_DATA = $(SASL_DB)

# Setup the default sasldb file with a single user, guest, with an
# obvious password. This user and password are the default for many
# clients.
#
# The realm specified by -u is very important, and QPID is the default
# for the broker so we use it here. The realm is important because it
# defaults to the local hostname of the machine running the
# broker. This may not seem to bad at first glance, but it means that
# the sasldb has to be tailored to each machine that would be running
# a broker, and if the machine ever changed its name the
# authentication would stop working until the sasldb was updated. For
# these reasons we always want the broker to specify a realm where its
# users live, and we want the users to exist in that realm as well.
$(SASL_DB):
	echo guest | /usr/sbin/saslpasswd2 -c -p -f $(SASL_DB) -u QPID guest

CLEANFILES=$(SASL_DB)

endif
