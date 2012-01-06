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
# Cluster tests cmake fragment, to be included in CMakeLists.txt
# 

add_executable (failover_soak failover_soak.cpp ForkedBroker.cpp ${platform_test_additions})
target_link_libraries (failover_soak qpidclient)
remember_location(failover_soak)

set (cluster_test_SOURCES
  cluster_test
  unit_test
  ClusterFixture
  ForkedBroker
  PartialFailure
  ClusterFailover
  InitialStatusMap
  StoreStatus
  )
add_executable (cluster_test ${cluster_test_SOURCES}  ${platform_test_additions})
target_link_libraries (cluster_test ${qpid_test_boost_libs} qpidclient qpidbroker cluster_shared)
remember_location(cluster_test)

add_test (cluster_test ${CMAKE_CURRENT_SOURCE_DIR}/run_cluster_test${test_script_suffix})
add_test (cluster_tests ${CMAKE_CURRENT_SOURCE_DIR}/run_cluster_tests${test_script_suffix})
add_test (cluster_read_credit ${CMAKE_CURRENT_SOURCE_DIR}/cluster_read_credit${test_script_suffix})
add_test (cluster_test_watchdog ${CMAKE_CURRENT_SOURCE_DIR}/test_watchdog${test_script_suffix})
add_test (federated_cluster_test ${CMAKE_CURRENT_SOURCE_DIR}/federated_cluster_test${test_script_suffix})
add_test (clustered_replication_test ${CMAKE_CURRENT_SOURCE_DIR}/clustered_replication_test${test_script_suffix})

# FIXME aconway 2009-12-01: translate to cmake
# # Clean up after cluster_test and start_cluster
# CLEANFILES += cluster_test.acl cluster.ports

# EXTRA_DIST +=					\
# 	ais_check				\
# 	run_cluster_test			\
# 	cluster_read_credit			\
# 	test_watchdog				\
# 	start_cluster				\
# 	stop_cluster				\
# 	restart_cluster				\
# 	cluster_python_tests			\
# 	cluster_python_tests_failing.txt	\
# 	federated_cluster_test			\
# 	clustered_replication_test		\
# 	run_cluster_tests			\
# 	run_long_cluster_tests			\
# 	testlib.py				\
# 	cluster_tests.py			\
# 	long_cluster_tests.py			\
# 	cluster_tests.fail

# LONG_TESTS +=					\
# 	run_long_cluster_tests			\
# 	start_cluster				\
# 	cluster_python_tests			\
# 	stop_cluster

# qpidtest_PROGRAMS += cluster_test

# cluster_test_SOURCES =				\

# cluster_test_LDADD=$(lib_client) $(lib_broker) ../cluster.la -lboost_unit_test_framework

# qpidtest_SCRIPTS += run_cluster_tests cluster_tests.py run_long_cluster_tests long_cluster_tests.py testlib.py cluster_tests.fail

# endif
