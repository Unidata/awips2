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

# Enable dashboard reporting.
include (CTest)

# Make sure that everything get built before the tests
# Need to create a var with all the necessary top level targets

add_definitions(-DBOOST_TEST_DYN_LINK)
include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )

include (FindPythonInterp)

# Create the environment scripts for tests
set (abs_srcdir ${CMAKE_CURRENT_SOURCE_DIR})
set (abs_builddir ${CMAKE_CURRENT_BINARY_DIR})
set (abs_top_srcdir ${CMAKE_SOURCE_DIR})
set (abs_top_builddir ${CMAKE_BINARY_DIR})
set (builddir_lib_suffix "")
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/test_env.sh.in
  ${CMAKE_CURRENT_BINARY_DIR}/test_env.sh)


# If valgrind is selected in the configuration step, set up the path to it
# for CTest.
if (ENABLE_VALGRIND)
  set (MEMORYCHECK_COMMAND ${VALGRIND})
  set (MEMORYCHECK_COMMAND_OPTIONS "--gen-suppressions=all
--leak-check=full
--demangle=yes
--suppressions=${CMAKE_CURRENT_SOURCE_DIR}/.valgrind.supp
--num-callers=25
--log-file=ctest_valgrind.vglog")
endif (ENABLE_VALGRIND)

# Using the Boost DLLs triggers warning 4275 on Visual Studio
# (non dll-interface class used as base for dll-interface class).
# This is ok, so suppress the warning.
# Also, boost lengthy names trigger warning 4503, decorated name length exceeded
# and using getenv() triggers insecure CRT warnings which we can silence in the
# test environment.
if (MSVC)
  add_definitions( /wd4275 /wd4503 /D_CRT_SECURE_NO_WARNINGS)
endif (MSVC)

# Like this to work with cmake 2.4 on Unix
set (qpid_test_boost_libs 
     ${Boost_REGEX_LIBRARY}
     ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

# Macro to make it easier to remember where the tests are built
macro(remember_location testname)
  set (${testname}_LOCATION ${CMAKE_CURRENT_BINARY_DIR}/${testname}${CMAKE_EXECUTABLE_SUFFIX})
endmacro(remember_location)

# Windows uses some process-startup calls to ensure that errors, etc. don't
# result in error boxes being thrown up. Since it's expected that most test
# runs will be in scripts, the default is to force these outputs to stderr
# instead of windows. If you want to remove this code, build without the
# QPID_WINDOWS_DEFAULT_TEST_OUTPUTS ON.
if (CMAKE_SYSTEM_NAME STREQUAL Windows)
  option(QPID_WINDOWS_DEFAULT_TEST_OUTPUTS "Use default error-handling on Windows tests" OFF)
  if (NOT QPID_WINDOWS_DEFAULT_TEST_OUTPUTS)
    set(platform_test_additions windows/DisableWin32ErrorWindows.cpp)
  endif (NOT QPID_WINDOWS_DEFAULT_TEST_OUTPUTS)
endif (CMAKE_SYSTEM_NAME STREQUAL Windows)

#
# Unit test program
#
# Unit tests are built as a single program to reduce valgrind overhead
# when running the tests. If you want to build a subset of the tests run
# ccmake and set unit_tests_to_build to the set you want to build.

set(unit_tests_to_build
    exception_test
    RefCounted
    SessionState
    logging
    AsyncCompletion
    Url
    Uuid
    Shlib
    FieldValue
    FieldTable
    Array
    QueueOptionsTest
    InlineAllocator
    InlineVector
    ClientSessionTest
    MessagingSessionTests
    SequenceSet
    StringUtils
    IncompleteMessageList
    RangeSet
    AtomicValue
    QueueTest
    AccumulatedAckTest
    DtxWorkRecordTest
    DeliveryRecordTest
    ExchangeTest
    HeadersExchangeTest
    MessageTest
    QueueRegistryTest
    QueuePolicyTest
    FramingTest
    HeaderTest
    SequenceNumberTest
    TimerTest
    TopicExchangeTest
    TxBufferTest
    TxPublishTest
    MessageBuilderTest
    ManagementTest
    MessageReplayTracker
    ConsoleTest
    QueueEvents
    ProxyTest
    RetryList
    RateFlowcontrolTest
    FrameDecoder
    ReplicationTest
    ClientMessageTest
    PollableCondition
    Variant
    ClientMessage
    ${xml_tests}
    CACHE STRING "Which unit tests to build"
   )

mark_as_advanced(unit_tests_to_build)

# Disabled till we move to amqp_0_10 codec.
# 	amqp_0_10/serialize.cpp allSegmentTypes.h \
# 	amqp_0_10/ProxyTemplate.cpp \
# 	amqp_0_10/apply.cpp \
# 	amqp_0_10/Map.cpp \
# 	amqp_0_10/handlers.cpp 

add_executable (unit_test unit_test
                ${unit_tests_to_build} ${platform_test_additions})
# The generally recommended way to add macro settings is to use
# COMPILE_DEFINITIONS, but it's a rough go to add more than one definition
# with a value; in this case, assuming that -D works everywhere is easier.
set_source_files_properties (ReplicationTest.cpp Shlib.cpp ${xml_tests}
                             PROPERTIES
                             COMPILE_FLAGS
                             "-DQPID_MODULE_SUFFIX=\\\"${CMAKE_SHARED_MODULE_SUFFIX}\\\" -DQPID_MODULE_PREFIX=\\\"${CMAKE_SHARED_MODULE_PREFIX}\\\"")
target_link_libraries (unit_test
                       ${qpid_test_boost_libs}
                       qpidclient qpidbroker qmfconsole)
remember_location(unit_test)

add_library (shlibtest MODULE shlibtest.cpp)

if (BUILD_CLUSTER)
  include (cluster.cmake)
endif (BUILD_CLUSTER)

# FIXME aconway 2009-11-30: enable SSL
#if SSL
#include ssl.mk
#endif

#
# Other test programs
#
add_executable (perftest perftest.cpp ${platform_test_additions})
target_link_libraries (perftest qpidclient)
#perftest_SOURCES=perftest.cpp test_tools.h TestOptions.h ConnectionOptions.h
remember_location(perftest)

add_executable (txtest txtest.cpp ${platform_test_additions})
target_link_libraries (txtest qpidclient)
#txtest_SOURCES=txtest.cpp  TestOptions.h ConnectionOptions.h
remember_location(txtest)

add_executable (latencytest latencytest.cpp ${platform_test_additions})
target_link_libraries (latencytest qpidclient)
#latencytest_SOURCES=latencytest.cpp TestOptions.h ConnectionOptions.h
remember_location(latencytest)

add_executable (echotest echotest.cpp ${platform_test_additions})
target_link_libraries (echotest qpidclient)
#echotest_SOURCES=echotest.cpp TestOptions.h ConnectionOptions.h
remember_location(echotest)

add_executable (client_test client_test.cpp ${platform_test_additions})
target_link_libraries (client_test qpidclient)
#client_test_SOURCES=client_test.cpp TestOptions.h ConnectionOptions.h
remember_location(client_test)

add_executable (topic_listener topic_listener.cpp ${platform_test_additions})
target_link_libraries (topic_listener qpidclient)
#topic_listener_SOURCES=topic_listener.cpp TestOptions.h ConnectionOptions.h
remember_location(topic_listener)

add_executable (topic_publisher topic_publisher.cpp ${platform_test_additions})
target_link_libraries (topic_publisher qpidclient)
#topic_publisher_SOURCES=topic_publisher.cpp TestOptions.h ConnectionOptions.h
remember_location(topic_publisher)

add_executable (publish publish.cpp ${platform_test_additions})
target_link_libraries (publish qpidclient)
#publish_SOURCES=publish.cpp TestOptions.h ConnectionOptions.h
remember_location(publish)

add_executable (consume consume.cpp ${platform_test_additions})
target_link_libraries (consume qpidclient)
#consume_SOURCES=consume.cpp  TestOptions.h ConnectionOptions.h
remember_location(consume)

add_executable (header_test header_test.cpp ${platform_test_additions})
target_link_libraries (header_test qpidclient)
#header_test_SOURCES=header_test.cpp TestOptions.h ConnectionOptions.h
remember_location(header_test)

add_executable (declare_queues declare_queues.cpp ${platform_test_additions})
target_link_libraries (declare_queues qpidclient)
remember_location(declare_queues)

add_executable (replaying_sender replaying_sender.cpp ${platform_test_additions})
target_link_libraries (replaying_sender qpidclient)
remember_location(replaying_sender)

add_executable (resuming_receiver resuming_receiver.cpp ${platform_test_additions})
target_link_libraries (resuming_receiver qpidclient)
remember_location(resuming_receiver)

add_executable (txshift txshift.cpp ${platform_test_additions})
target_link_libraries (txshift qpidclient)
#txshift_SOURCES=txshift.cpp  TestOptions.h ConnectionOptions.h
remember_location(txshift)

add_executable (txjob txjob.cpp ${platform_test_additions})
target_link_libraries (txjob qpidclient)
#txjob_SOURCES=txjob.cpp  TestOptions.h ConnectionOptions.h
remember_location(txjob)

add_executable (receiver receiver.cpp ${platform_test_additions})
target_link_libraries (receiver qpidclient)
#receiver_SOURCES=receiver.cpp  TestOptions.h ConnectionOptions.h
remember_location(receiver)

add_executable (sender sender.cpp ${platform_test_additions})
target_link_libraries (sender qpidclient)
#sender_SOURCES=sender.cpp  TestOptions.h ConnectionOptions.h
remember_location(sender)

if (CMAKE_SYSTEM_NAME STREQUAL Windows)
  set (ENV{OUTDIR} ${EXECUTABLE_OUTPUT_PATH})
  set (test_script_suffix ".ps1")
  set (shell "powershell")
endif (CMAKE_SYSTEM_NAME STREQUAL Windows)

set(test_wrap ${shell} ${CMAKE_CURRENT_SOURCE_DIR}/run_test${test_script_suffix})

add_test (unit_test ${test_wrap} ${unit_test_LOCATION})
add_test (start_broker ${shell} ${CMAKE_CURRENT_SOURCE_DIR}/start_broker${test_script_suffix})
add_test (client_test ${test_wrap} ${client_test_LOCATION})
add_test (quick_perftest ${test_wrap} ${perftest_LOCATION} --summary --count 100)
add_test (quick_topictest ${test_wrap} ${CMAKE_CURRENT_SOURCE_DIR}/quick_topictest${test_script_suffix})
add_test (quick_txtest ${test_wrap} ${txtest_LOCATION} --queues 4 --tx-count 10 --quiet)
if (PYTHON_EXECUTABLE)
  add_test (run_header_test ${shell} ${CMAKE_CURRENT_SOURCE_DIR}/run_header_test${test_script_suffix})
  add_test (python_tests ${test_wrap} ${CMAKE_CURRENT_SOURCE_DIR}/python_tests${test_script_suffix})
endif (PYTHON_EXECUTABLE)
add_test (stop_broker ${shell} ${CMAKE_CURRENT_SOURCE_DIR}/stop_broker${test_script_suffix})
if (PYTHON_EXECUTABLE)
  add_test (federation_tests ${shell} ${CMAKE_CURRENT_SOURCE_DIR}/run_federation_tests${test_script_suffix})
if (BUILD_ACL)
  add_test (acl_tests ${shell} ${CMAKE_CURRENT_SOURCE_DIR}/run_acl_tests${test_script_suffix})
endif (BUILD_ACL)
endif (PYTHON_EXECUTABLE)

add_library(test_store MODULE test_store.cpp)
target_link_libraries (test_store qpidbroker qpidcommon)
set_target_properties (test_store PROPERTIES PREFIX "")

#EXTRA_DIST +=								\
#  run_test vg_check							\
#  run-unit-tests start_broker python_tests stop_broker 			\
#  quick_topictest							\
#  quick_perftest							\
#  quick_txtest								\
#  topictest								\
#  run_header_test							\
#  header_test.py							\
#  ssl_test								\
#  config.null								\
#  ais_check								\
#  run_federation_tests							\
#  run_acl_tests								\
#  .valgrind.supp							\
#  MessageUtils.h							\
#  TestMessageStore.h							\
#  TxMocks.h								\
#  start_cluster stop_cluster restart_cluster

add_library (dlclose_noop MODULE dlclose_noop.c)
#libdlclose_noop_la_LDFLAGS = -module -rpath $(abs_builddir)

#CLEANFILES+=valgrind.out *.log *.vglog* dummy_test $(unit_wrappers)
#
## FIXME aconway 2008-05-23: Disabled interop_runner because it uses
## the obsolete Channel class.  Convert to Session and re-enable.
## 
## check_PROGRAMS += interop_runner
#
## interop_runner_SOURCES = 	\
##   interop_runner.cpp	 	\
##   SimpleTestCaseBase.cpp	\
##   BasicP2PTest.cpp		\
##   BasicPubSubTest.cpp		\
##   SimpleTestCaseBase.h		\
##   BasicP2PTest.h		\
##   BasicPubSubTest.h		\
##   TestCase.h			\
##   TestOptions.h ConnectionOptions.h
## interop_runner_LDADD = $(lib_client) $(lib_common) $(extra_libs)
#
#
## Longer running stability tests, not run by default check: target.
## Not run under valgrind, too slow
#LONG_TESTS=fanout_perftest shared_perftest multiq_perftest topic_perftest run_failover_soak
#EXTRA_DIST+=$(LONG_TESTS) run_perftest
#check-long:
#	$(MAKE) check TESTS="start_broker $(LONG_TESTS) stop_broker" VALGRIND=
