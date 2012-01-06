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

if HAVE_LIBCPG

#
# Cluster tests makefile fragment, to be included in Makefile.am
# 

# NOTE: Programs using the openais library must be run with gid=ais
# You should do "newgrp ais" before running the tests to run these.
# 


# ais_check checks pre-requisites for cluster tests and runs them if ok.
TESTS +=					\
	run_cluster_test			\
	cluster_read_credit			\
	test_watchdog				\
	run_cluster_tests			\
	federated_cluster_test			\
	clustered_replication_test

# Clean up after cluster_test and start_cluster
CLEANFILES += cluster_test.acl cluster.ports

EXTRA_DIST +=					\
	ais_check				\
	run_cluster_test			\
	cluster_read_credit			\
	test_watchdog				\
	start_cluster				\
	stop_cluster				\
	restart_cluster				\
	cluster_python_tests			\
	cluster_python_tests_failing.txt	\
	federated_cluster_test			\
	clustered_replication_test		\
	run_cluster_tests			\
	run_long_cluster_tests			\
	testlib.py				\
	cluster_tests.py			\
	long_cluster_tests.py			\
	cluster_tests.fail

LONG_TESTS +=					\
	run_long_cluster_tests			\
	start_cluster				\
	cluster_python_tests			\
	stop_cluster

qpidtest_PROGRAMS += cluster_test

cluster_test_SOURCES =				\
	cluster_test.cpp			\
	unit_test.cpp				\
	ClusterFixture.cpp			\
	ClusterFixture.h			\
	ForkedBroker.h				\
	ForkedBroker.cpp			\
	PartialFailure.cpp			\
	ClusterFailover.cpp			\
	InitialStatusMap.cpp			\
	StoreStatus.cpp

cluster_test_LDADD=$(lib_client) $(lib_broker) ../cluster.la -lboost_unit_test_framework

qpidtest_SCRIPTS += run_cluster_tests cluster_tests.py run_long_cluster_tests long_cluster_tests.py testlib.py cluster_tests.fail

endif
