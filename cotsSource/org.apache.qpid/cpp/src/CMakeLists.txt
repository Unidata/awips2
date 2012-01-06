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

# Option to require building optional plugins
foreach (r ${REQUIRE})
  set(${r}_force ON)
  message(STATUS "Forcing ${r} to ${${r}_force}")
endforeach(r)

include(CheckFunctionExists)
include(CheckIncludeFileCXX)
include(CheckIncludeFiles)
include(CheckIncludeFileCXX)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(FindBoost)
include(FindDoxygen)

#set (CMAKE_VERBOSE_MAKEFILE ON)  # for debugging

# check if we generate source as part of the build
#   - rubygen generates the amqp spec and clustering
#   - managementgen generates the broker management code
#
# rubygen subdir is excluded from stable distributions
# If the main AMQP spec is present, then check if ruby and python are
# present, and if any sources have changed, forcing a re-gen of source code.

set(AMQP_SPEC_DIR ${qpid-cpp_SOURCE_DIR}/../specs)
set(AMQP_SPEC ${AMQP_SPEC_DIR}/amqp.0-10-qpid-errata.xml)
if (EXISTS ${AMQP_SPEC})
  include(FindRuby)
  include(FindPythonInterp)
  if (NOT RUBY_EXECUTABLE)
    message(FATAL_ERROR "Can't locate ruby, needed to generate source files.")
  endif (NOT RUBY_EXECUTABLE)
  if (NOT PYTHON_EXECUTABLE)
    message(FATAL_ERROR "Can't locate python, needed to generate source files.")
  endif (NOT PYTHON_EXECUTABLE)

  set(specs ${AMQP_SPEC} ${qpid-cpp_SOURCE_DIR}/xml/cluster.xml)
  set(regen_amqp OFF)
  set(rgen_dir ${qpid-cpp_SOURCE_DIR}/rubygen)
  file(GLOB_RECURSE rgen_progs ${rgen_dir}/*.rb)
  # If any of the specs, or any of the sources used to generate code, change
  # then regenerate the sources.
  foreach (spec_file ${specs} ${rgen_progs})
    if (${spec_file} IS_NEWER_THAN ${CMAKE_CURRENT_BINARY_DIR}/rubygen.cmake)
      set(regen_amqp ON)
    endif (${spec_file} IS_NEWER_THAN ${CMAKE_CURRENT_BINARY_DIR}/rubygen.cmake)
  endforeach (spec_file ${specs})
  if (regen_amqp)
    message(STATUS "Regenerating AMQP protocol sources")
execute_process(COMMAND ${RUBY_EXECUTABLE} -I ${rgen_dir} ${rgen_dir}/generate ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/../include  ${specs} all rubygen.cmake
                    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  else (regen_amqp)
    message(STATUS "No need to generate AMQP protocol sources")
  endif (regen_amqp)

  set(mgmt_specs ${AMQP_SPEC_DIR}/management-schema.xml
                 ${CMAKE_CURRENT_SOURCE_DIR}/qpid/acl/management-schema.xml
                 ${CMAKE_CURRENT_SOURCE_DIR}/qpid/cluster/management-schema.xml)
  set(mgen_dir ${qpid-cpp_SOURCE_DIR}/managementgen)
  set(regen_mgmt OFF)
  foreach (spec_file ${mgmt_specs})
    if (${spec_file} IS_NEWER_THAN ${CMAKE_CURRENT_BINARY_DIR}/managementgen.cmake)
      message(STATUS "${spec_file} is newer")
      set(regen_mgmt ON)
    endif (${spec_file} IS_NEWER_THAN ${CMAKE_CURRENT_BINARY_DIR}/managementgen.cmake)
  endforeach (spec_file ${mgmt_specs})
  if (regen_mgmt)
    message(STATUS "Regenerating Qpid Management Framework sources")
execute_process(COMMAND ${PYTHON_EXECUTABLE} ${mgen_dir}/qmf-gen -c managementgen.cmake -b -q -o ${CMAKE_CURRENT_BINARY_DIR}/qmf ${mgmt_specs}
                    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  else (regen_mgmt)
    message(STATUS "No need to generate Qpid Management Framework sources")
  endif (regen_mgmt)

  # Pull in the names of the generated files, i.e. ${rgen_framing_srcs}
  include (${CMAKE_CURRENT_BINARY_DIR}/rubygen.cmake)
  include (${CMAKE_CURRENT_BINARY_DIR}/managementgen.cmake)

else (EXISTS ${AMQP_SPEC})
  message(STATUS "No AMQP spec... presume generated sources are included")
  include (rubygen.cmake)
  include (managementgen.cmake)
endif (EXISTS ${AMQP_SPEC})

find_program(HELP2MAN help2man DOC "Location of the help2man program")
option(GEN_MANPAGES "Use help2man to generate man pages" ON)
if (GEN_MANPAGES AND NOT HELP2MAN)
  message(STATUS "Can't locate the help2man command; man pages will not be generated")
  set (GEN_MANPAGES OFF)
endif (GEN_MANPAGES AND NOT HELP2MAN)

# FindDoxygen module tries to locate doxygen and Graphviz dot
set (docs_default ON)
if (NOT DOXYGEN_EXECUTABLE)
  set (docs_default OFF)
endif (NOT DOXYGEN_EXECUTABLE)
option(GEN_DOXYGEN "Use doxygen to generate user documentation" ${docs_default})
if (GEN_DOXYGEN AND NOT DOXYGEN_EXECUTABLE)
  message(STATUS "Can't locate the doxygen command; user documentation will not be generated")
  set (GEN_DOXYGEN OFF)
endif (GEN_DOXYGEN AND NOT DOXYGEN_EXECUTABLE)

find_program(VALGRIND valgrind DOC "Location of the valgrind program")
option(ENABLE_VALGRIND "Use valgrind to detect run-time problems" ON)
if (ENABLE_VALGRIND AND NOT VALGRIND)
  message(STATUS "Can't locate the valgrind command; no run-time error detection")
endif (ENABLE_VALGRIND AND NOT VALGRIND)

if (CMAKE_COMPILER_IS_GNUCXX)
   set (COMPILER_FLAGS "")
   # Warnings: Enable as many as possible, keep the code clean. Please
   # do not disable warnings or remove -Werror without discussing on
   # qpid-dev list.
   #
   # The following warnings are deliberately omitted, they warn on valid code.
   # -Wunreachable-code -Wpadded -Winline
   # -Wshadow - warns about boost headers.
   set (WARNING_FLAGS
        "-Werror -pedantic -Wall -Wextra -Wno-shadow -Wpointer-arith -Wcast-qual -Wcast-align -Wno-long-long -Wvolatile-register-var -Winvalid-pch -Wno-system-headers -Woverloaded-virtual")
endif (CMAKE_COMPILER_IS_GNUCXX)

if (CMAKE_CXX_COMPILER_ID STREQUAL SunPro)
  set (COMPILER_FLAGS "-library=stlport4 -mt")
  set (WARNING_FLAGS "+w2")
endif (CMAKE_CXX_COMPILER_ID STREQUAL SunPro)

option(ENABLE_WARNINGS "Enable lots of compiler warnings (recommended)" ON)
if (NOT ENABLE_WARNINGS)
  set (WARNING_FLAGS "")
endif (NOT ENABLE_WARNINGS)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${COMPILER_FLAGS} ${WARNING_FLAGS}")

# Expand a bit from the basic Find_Boost; be specific about what's needed.
# TODO: Not all these libs are needed everywhere:
# Linux only uses filesystem program_options unit_test_framework
# (which itself uses regex).
# Boost.system is sometimes needed; it's handled separately, below.
find_package(Boost 1.33 REQUIRED
             COMPONENTS filesystem program_options date_time thread
                        regex unit_test_framework)
if(NOT Boost_FOUND)
  message(FATAL_ERROR "Boost C++ libraries not found.  Please install or try setting BOOST_ROOT")
endif(NOT Boost_FOUND)

# Boost.system was introduced at Boost 1.35; it's needed secondarily by other
# Boost libs Qpid needs, so be sure it's there.
if (NOT Boost_VERSION LESS 103500)
  find_package(Boost COMPONENTS system)
  if (NOT Boost_SYSTEM_LIBRARY)
    set(Boost_SYSTEM_LIBRARY boost_system)
  endif (NOT Boost_SYSTEM_LIBRARY)

endif (NOT Boost_VERSION LESS 103500)

# Versions of cmake pre 2.6 don't set the Boost_*_LIBRARY variables correctly
# these values are correct for Linux
if (NOT Boost_PROGRAM_OPTIONS_LIBRARY)
  set(Boost_PROGRAM_OPTIONS_LIBRARY boost_program_options)
endif (NOT Boost_PROGRAM_OPTIONS_LIBRARY)

if (NOT Boost_FILESYSTEM_LIBRARY)
  set(Boost_FILESYSTEM_LIBRARY boost_filesystem)
endif (NOT Boost_FILESYSTEM_LIBRARY)

if (NOT Boost_UNIT_TEST_FRAMEWORK_LIBRARY)
  set(Boost_UNIT_TEST_FRAMEWORK_LIBRARY boost_unit_test_framework)
endif (NOT Boost_UNIT_TEST_FRAMEWORK_LIBRARY)

if (NOT Boost_REGEX_LIBRARY)
  set(Boost_REGEX_LIBRARY boost_regex)
endif (NOT Boost_REGEX_LIBRARY)

if (NOT Boost_SYSTEM_LIBRARY)
  set(Boost_SYSTEM_LIBRARY boost_system)
endif (NOT Boost_SYSTEM_LIBRARY)

# The Windows install also wants the Boost DLLs and headers that the release
# is built with. The DLLs enable everything to run, and the headers ensure
# that users building Qpid C++ client programs can compile (the C++ API
# still exposes Boost headers, but hopefully this will be fixed in the
# future).
#
# On Windows you can pick whether the static or dynamic versions of the libs
# are used; allow this choice to the user. Since we also install the Boost
# DLLs that are needed for the Windows package, none are needed for the
# static link case; else drop them into the install. Do this all first, since
# Boost on Windows can use automatic linking to pick up the correct
# Boost libs based on compile-time touching of the headers. Since we don't
# really need to add them to the link lines, set the names to blanks.
if (MSVC)
   option(QPID_LINK_BOOST_DYNAMIC "Link with dynamic Boost libs (OFF to link static)" ON)
   if (QPID_LINK_BOOST_DYNAMIC)
      add_definitions( /D BOOST_ALL_DYN_LINK)
      string (REPLACE .lib .dll
              _boost_date_time_debug ${Boost_DATE_TIME_LIBRARY_DEBUG})
      string (REPLACE .lib .dll
              _boost_date_time_release ${Boost_DATE_TIME_LIBRARY_RELEASE})
      string (REPLACE .lib .dll
              _boost_filesystem_debug ${Boost_FILESYSTEM_LIBRARY_DEBUG})
      string (REPLACE .lib .dll
              _boost_filesystem_release ${Boost_FILESYSTEM_LIBRARY_RELEASE})
      string (REPLACE .lib .dll
              _boost_program_options_debug ${Boost_PROGRAM_OPTIONS_LIBRARY_DEBUG})
      string (REPLACE .lib .dll
              _boost_program_options_release ${Boost_PROGRAM_OPTIONS_LIBRARY_RELEASE})
      string (REPLACE .lib .dll
              _boost_regex_debug ${Boost_REGEX_LIBRARY_DEBUG})
      string (REPLACE .lib .dll
              _boost_regex_release ${Boost_REGEX_LIBRARY_RELEASE})
      string (REPLACE .lib .dll
              _boost_thread_debug ${Boost_THREAD_LIBRARY_DEBUG})
      string (REPLACE .lib .dll
              _boost_thread_release ${Boost_THREAD_LIBRARY_RELEASE})
      # Boost 1.35 added the system library, which gets indirectly linked in
      # via other Boost libs. So, if building with Boost 1.35 or later, also
      # include system in the Windows install package.
      if (NOT Boost_VERSION LESS 103500)
         string (REPLACE boost_thread boost_system
                 _boost_system_debug ${_boost_thread_debug})
         string (REPLACE boost_thread boost_system
                 _boost_system_release ${_boost_thread_release})
      endif (NOT Boost_VERSION LESS 103500)
      install (PROGRAMS
               ${_boost_date_time_debug} ${_boost_date_time_release}
               ${_boost_filesystem_debug} ${_boost_filesystem_release}
               ${_boost_program_options_debug} ${_boost_program_options_release}
               ${_boost_regex_debug} ${_boost_regex_release}
               ${_boost_system_debug} ${_boost_system_release}
               ${_boost_thread_debug} ${_boost_thread_release}
               DESTINATION ${QPID_INSTALL_LIBDIR}
               COMPONENT ${QPID_COMPONENT_COMMON})
   endif (QPID_LINK_BOOST_DYNAMIC)

   # Need the boost headers regardless of which way the libs go.
   install (DIRECTORY ${Boost_INCLUDE_DIR}/boost
            DESTINATION ${QPID_INSTALL_INCLUDEDIR}
            COMPONENT ${QPID_COMPONENT_CLIENT_INCLUDE})

   set(Boost_DATE_TIME_LIBRARY "")
   set(Boost_THREAD_LIBRARY "")
   set(Boost_PROGRAM_OPTIONS_LIBRARY "")
   set(Boost_FILESYSTEM_LIBRARY "")
   set(Boost_UNIT_TEST_FRAMEWORK_LIBRARY "")
   set(Boost_REGEX_LIBRARY "")
endif (MSVC)

include_directories( ${Boost_INCLUDE_DIR} )

include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )
include_directories( ${CMAKE_CURRENT_SOURCE_DIR}/../include )
include_directories( ${CMAKE_CURRENT_BINARY_DIR} )
include_directories( ${CMAKE_CURRENT_BINARY_DIR}/../include )

link_directories( ${Boost_LIBRARY_DIRS} )

CHECK_LIBRARY_EXISTS (rt clock_gettime "" CLOCK_GETTIME_IN_RT)
if (NOT CLOCK_GETTIME_IN_RT)
  CHECK_FUNCTION_EXISTS (clock_gettime QPID_HAS_CLOCK_GETTIME)
else (NOT CLOCK_GETTIME_IN_RT)
  set(CMAKE_REQUIRED_LIBS ${CMAKE_REQUIRED_LIBS} rt)
  set(QPID_HAS_CLOCK_GETTIME YES CACHE BOOL "Platform has clock_gettime")
endif (NOT CLOCK_GETTIME_IN_RT)

# See if Cyrus SASL is desired and available
CHECK_LIBRARY_EXISTS (sasl2 sasl_checkpass "" HAVE_SASL2)
CHECK_INCLUDE_FILES (sasl/sasl.h HAVE_SASL_H)

set (sasl_default ${sasl_force})
if (HAVE_SASL2 AND HAVE_SASL_H)
  set (sasl_default ON)
endif (HAVE_SASL2 AND HAVE_SASL_H)

option(BUILD_SASL "Build with Cyrus SASL support" ${sasl_default})
if (BUILD_SASL)
  if (NOT HAVE_SASL2)
    message(FATAL_ERROR "Cyrus SASL support requested but libsasl2 not found")
  endif (NOT HAVE_SASL2)
  if (NOT HAVE_SASL_H)
    message(FATAL_ERROR "Cyrus SASL support requested but sasl.h not found")
  endif (NOT HAVE_SASL_H)

  set(BROKER_SASL_NAME "qpidd" CACHE STRING "SASL app name for the qpid broker")
  set(qpidcommon_sasl_source
      qpid/sys/cyrus/CyrusSecurityLayer.h
      qpid/sys/cyrus/CyrusSecurityLayer.cpp
     )
  set(qpidcommon_sasl_lib sasl2)
endif (BUILD_SASL)

# See if XML Exchange is desired and prerequisites are available
CHECK_LIBRARY_EXISTS (xerces-c _init "" HAVE_XERCES)
CHECK_INCLUDE_FILE_CXX (xercesc/framework/MemBufInputSource.hpp HAVE_XERCES_H)
CHECK_INCLUDE_FILE_CXX (xqilla/xqilla-simple.hpp HAVE_XQILLA_H)

set (xml_default ${xml_force})
if (CMAKE_SYSTEM_NAME STREQUAL Windows)
else (CMAKE_SYSTEM_NAME STREQUAL Windows)
  if (HAVE_XERCES AND HAVE_XERCES_H)
    if (HAVE_XQILLA_H)
      set (xml_default ON)
    endif (HAVE_XQILLA_H)
  endif (HAVE_XERCES AND HAVE_XERCES_H)
endif (CMAKE_SYSTEM_NAME STREQUAL Windows)

option(BUILD_XML "Build with XML Exchange" ${xml_default})
if (BUILD_XML)
  if (NOT HAVE_XERCES)
    message(FATAL_ERROR "XML Exchange support requested but xerces-c library not found")
  endif (NOT HAVE_XERCES)
  if (NOT HAVE_XERCES_H)
    message(FATAL_ERROR "XML Exchange support requested but Xerces-C headers not found")
  endif (NOT HAVE_XERCES_H)
  if (NOT HAVE_XQILLA_H)
    message(FATAL_ERROR "XML Exchange support requested but XQilla headers not found")
  endif (NOT HAVE_XQILLA_H)

  add_library (xml MODULE
               qpid/xml/XmlExchange.cpp
               qpid/xml/XmlExchange.h
               qpid/xml/XmlExchangePlugin.cpp)
  set_target_properties (xml PROPERTIES PREFIX "")
  target_link_libraries (xml xerces-c xqilla qpidbroker pthread)
  if (CMAKE_COMPILER_IS_GNUCXX)
    set_target_properties (xml PROPERTIES
                           PREFIX ""
                           LINK_FLAGS -Wl,--no-undefined)
  endif (CMAKE_COMPILER_IS_GNUCXX)
  install (TARGETS xml
           DESTINATION ${QPIDD_MODULE_DIR}
           COMPONENT ${QPID_COMPONENT_BROKER})

  set(xml_tests XmlClientSessionTest)

endif (BUILD_XML)

# Build the ACL plugin
set (acl_default ON)
# Like this until we fix exporting symbols from the generated management code
if (CMAKE_SYSTEM_NAME STREQUAL Windows)
  set(acl_default OFF)
endif (CMAKE_SYSTEM_NAME STREQUAL Windows)
option(BUILD_ACL "Build ACL enforcement broker plugin" ${acl_default})
if (BUILD_ACL)
  set (acl_SOURCES
       qpid/acl/Acl.cpp
       qpid/acl/Acl.h
       qpid/acl/AclData.cpp
       qpid/acl/AclData.h
       qpid/acl/AclPlugin.cpp
       qpid/acl/AclReader.cpp
       qpid/acl/AclReader.h
      )
  add_library (acl MODULE ${acl_SOURCES})
  set_target_properties (acl PROPERTIES PREFIX "")
  target_link_libraries (acl qpidbroker ${Boost_PROGRAM_OPTIONS_LIBRARY})
  if (CMAKE_COMPILER_IS_GNUCXX)
    set_target_properties (acl PROPERTIES
                           PREFIX ""
                           LINK_FLAGS -Wl,--no-undefined)
  endif (CMAKE_COMPILER_IS_GNUCXX)
  install (TARGETS acl
           DESTINATION ${QPIDD_MODULE_DIR}
           COMPONENT ${QPID_COMPONENT_BROKER})
endif (BUILD_ACL)

# Check for optional cluster support requirements
include (cluster.cmake)

# Check for optional RDMA support requirements
include (rdma.cmake)

# Check for optional SSL support requirements
include (ssl.cmake)

# Check for syslog capabilities not present on all systems
check_symbol_exists (LOG_AUTHPRIV "sys/syslog.h" HAVE_LOG_AUTHPRIV)
check_symbol_exists (LOG_FTP "sys/syslog.h" HAVE_LOG_FTP)

if (CMAKE_SYSTEM_NAME STREQUAL Windows)
  if (MSVC)
    add_definitions( 
      /D "_CRT_NONSTDC_NO_WARNINGS"
      /D "NOMINMAX"
      /D "WIN32_LEAN_AND_MEAN"
      /D "_SCL_SECURE_NO_WARNINGS"
      /wd4244
      /wd4800
      /wd4355
    )
  endif (MSVC)

  set (qpidcommon_platform_SOURCES
    qpid/log/windows/SinkOptions.cpp
    qpid/sys/windows/AsynchIO.cpp
    qpid/sys/windows/FileSysDir.cpp
    qpid/sys/windows/IocpPoller.cpp
    qpid/sys/windows/IOHandle.cpp
    qpid/sys/windows/LockFile.cpp
    qpid/sys/windows/PipeHandle.cpp
    qpid/sys/windows/PollableCondition.cpp
    qpid/sys/windows/Shlib.cpp
    qpid/sys/windows/Socket.cpp
    qpid/sys/windows/SocketAddress.cpp
    qpid/sys/windows/StrError.cpp
    qpid/sys/windows/SystemInfo.cpp
    qpid/sys/windows/Thread.cpp
    qpid/sys/windows/Time.cpp
    qpid/sys/windows/uuid.cpp
    ${sslcommon_windows_SOURCES}
  )
  set (qpidcommon_platform_LIBS
    ${windows_ssl_libs} rpcrt4 ws2_32
  )
  set (qpidbroker_platform_SOURCES
    qpid/broker/windows/BrokerDefaults.cpp
    qpid/broker/windows/SaslAuthenticator.cpp
    ${sslbroker_windows_SOURCES}
  )
  set (qpidbroker_platform_LIBS
    ${windows_ssl_libs}
  )
  set (qpidclient_platform_SOURCES
    qpid/client/windows/SaslFactory.cpp
    ${sslclient_windows_SOURCES}
  )
  set (qpidclient_platform_LIBS
    ${windows_ssl_libs}
  )

  set (qpidd_platform_SOURCES
    windows/QpiddBroker.cpp
  )

else (CMAKE_SYSTEM_NAME STREQUAL Windows)

  # POSIX (Non-Windows) platforms have a lot of overlap in sources; the only
  # major difference is the poller module.
  if (CMAKE_SYSTEM_NAME STREQUAL Linux)
    set (qpid_poller_module 
      qpid/sys/epoll/EpollPoller.cpp
      qpid/sys/posix/SystemInfo.cpp
    )
    add_definitions(-pthread)
    set (CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS} -pthread)
  endif (CMAKE_SYSTEM_NAME STREQUAL Linux)

  if (CMAKE_SYSTEM_NAME STREQUAL SunOS)
    set (qpid_poller_module
      qpid/sys/solaris/ECFPoller.cpp
      qpid/sys/solaris/SystemInfo.cpp
    )
  endif (CMAKE_SYSTEM_NAME STREQUAL SunOS)

  set (qpidcommon_platform_SOURCES
    qpid/sys/posix/AsynchIO.cpp
    qpid/sys/posix/Fork.cpp
    qpid/sys/posix/FileSysDir.cpp
    qpid/sys/posix/IOHandle.cpp
    qpid/sys/posix/LockFile.cpp
    qpid/sys/posix/Mutex.cpp
    qpid/sys/posix/PipeHandle.cpp
    qpid/sys/posix/PollableCondition.cpp
    qpid/sys/posix/Shlib.cpp
    qpid/log/posix/SinkOptions.cpp
    qpid/sys/posix/Socket.cpp
    qpid/sys/posix/SocketAddress.cpp
    qpid/sys/posix/StrError.cpp
    qpid/sys/posix/Thread.cpp
    qpid/sys/posix/Time.cpp

    ${qpid_poller_module}
  )
  set (qpidcommon_platform_LIBS
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${Boost_FILESYSTEM_LIBRARY}
    uuid
    ${CMAKE_DL_LIBS}
  )

  set (qpidbroker_platform_SOURCES
     qpid/broker/Daemon.cpp
     qpid/broker/SaslAuthenticator.cpp
     qpid/broker/SignalHandler.h
     qpid/broker/SignalHandler.cpp
     qpid/broker/posix/BrokerDefaults.cpp
  )

  set (qpidclient_platform_SOURCES
     qpid/client/SaslFactory.cpp
  )

  set (qpidd_platform_SOURCES
    posix/QpiddBroker.cpp
  )
endif (CMAKE_SYSTEM_NAME STREQUAL Windows)

set (qpidcommon_SOURCES
     ${rgen_framing_srcs}
     ${qpidcommon_platform_SOURCES}
     ${qpidcommon_sasl_source}
     qpid/assert.cpp
     qpid/Address.cpp
     qpid/DataDir.cpp
     qpid/Exception.cpp
     qpid/Modules.cpp
     qpid/Options.cpp
     qpid/Plugin.cpp
     qpid/RefCountedBuffer.cpp
     qpid/SessionState.cpp
     qpid/SessionId.cpp
     qpid/StringUtils.cpp
     qpid/Url.cpp
     qpid/amqp_0_10/SessionHandler.cpp
     qpid/framing/AccumulatedAck.cpp
     qpid/framing/AMQBody.cpp
     qpid/framing/AMQMethodBody.cpp
     qpid/framing/AMQContentBody.cpp
     qpid/framing/AMQFrame.cpp
     qpid/framing/AMQHeaderBody.cpp
     qpid/framing/AMQHeartbeatBody.cpp
     qpid/framing/Array.cpp
     qpid/framing/BodyHandler.cpp
     qpid/framing/Buffer.cpp
     qpid/framing/Endian.cpp
     qpid/framing/FieldTable.cpp
     qpid/framing/FieldValue.cpp
     qpid/framing/FrameSet.cpp
     qpid/framing/FrameDecoder.cpp
     qpid/framing/List.cpp
     qpid/framing/ProtocolInitiation.cpp
     qpid/framing/ProtocolVersion.cpp
     qpid/framing/SendContent.cpp
     qpid/framing/SequenceNumber.cpp
     qpid/framing/SequenceNumberSet.cpp
     qpid/framing/SequenceSet.cpp
     qpid/framing/Proxy.cpp
     qpid/framing/Uuid.cpp
     qpid/framing/TransferContent.cpp
     qpid/log/Logger.cpp
     qpid/log/Options.cpp
     qpid/log/OstreamOutput.cpp
     qpid/log/Selector.cpp
     qpid/log/Statement.cpp
     qpid/management/Manageable.cpp
     qpid/management/ManagementObject.cpp
     qpid/sys/AggregateOutput.cpp
     qpid/sys/AsynchIOHandler.cpp
     qpid/sys/Dispatcher.cpp
     qpid/sys/DispatchHandle.cpp
     qpid/sys/Runnable.cpp
     qpid/sys/Shlib.cpp
     qpid/sys/Timer.cpp
)

add_library (qpidcommon SHARED ${qpidcommon_SOURCES})
if (CLOCK_GETTIME_IN_RT)
  set (qpidcommon_platform_LIBS ${qpidcommon_platform_LIBS} rt)
endif (CLOCK_GETTIME_IN_RT)
target_link_libraries (qpidcommon
                       ${qpidcommon_platform_LIBS}
                       ${qpidcommon_sasl_lib})
set_target_properties (qpidcommon PROPERTIES
                       VERSION ${qpidc_version})
install (TARGETS qpidcommon
         DESTINATION ${QPID_INSTALL_LIBDIR}
         COMPONENT ${QPID_COMPONENT_COMMON})
# When the client programming component is installed, Windows also needs
# the debug variant of qpidcommon and qpidclient, and the PDBs for those
# as well. It would be nice to figure out a way to put some sanity checking
# here... as it is, success relies on the packager building the debug then
# the release, then packaging the release build.
if (WIN32)
    install (PROGRAMS
             ${CMAKE_CURRENT_BINARY_DIR}/Debug/qpidcommond.dll
             ${CMAKE_CURRENT_BINARY_DIR}/Debug/qpidcommond.lib
             ${CMAKE_CURRENT_BINARY_DIR}/Debug/qpidcommond.pdb
             DESTINATION ${QPID_INSTALL_LIBDIR}
             COMPONENT ${QPID_COMPONENT_CLIENT})
endif (WIN32)

set (qpidclient_SOURCES
     ${rgen_client_srcs}
     ${qpidclient_platform_SOURCES}
     qpid/client/Bounds.cpp
     qpid/client/Completion.cpp
     qpid/client/Connection.cpp
     qpid/client/ConnectionHandler.cpp
     qpid/client/ConnectionImpl.cpp
     qpid/client/ConnectionSettings.cpp
     qpid/client/Connector.cpp
     qpid/client/Demux.cpp
     qpid/client/Dispatcher.cpp
     qpid/client/FailoverManager.cpp
     qpid/client/FailoverListener.cpp
     qpid/client/Future.cpp
     qpid/client/FutureCompletion.cpp
     qpid/client/FutureResult.cpp
     qpid/client/LoadPlugins.cpp
     qpid/client/LocalQueue.cpp
     qpid/client/LocalQueueImpl.cpp
     qpid/client/Message.cpp
     qpid/client/MessageImpl.cpp
     qpid/client/MessageListener.cpp
     qpid/client/MessageReplayTracker.cpp
     qpid/client/QueueOptions.cpp
     qpid/client/Results.cpp
     qpid/client/SessionBase_0_10.cpp
     qpid/client/SessionBase_0_10Access.h
     qpid/client/ConnectionAccess.h
     qpid/client/SessionImpl.cpp
     qpid/client/StateManager.cpp
     qpid/client/Subscription.cpp
     qpid/client/SubscriptionImpl.cpp
     qpid/client/SubscriptionManager.cpp
     qpid/client/SubscriptionManagerImpl.cpp
     qpid/client/TCPConnector.cpp
     qpid/messaging/Address.cpp
     qpid/messaging/Connection.cpp
     qpid/messaging/ConnectionImpl.h
     qpid/messaging/ListContent.cpp
     qpid/messaging/ListView.cpp
     qpid/messaging/MapContent.cpp
     qpid/messaging/MapView.cpp
     qpid/messaging/Message.cpp
     qpid/messaging/MessageImpl.h
     qpid/messaging/MessageImpl.cpp
     qpid/messaging/Receiver.cpp
     qpid/messaging/ReceiverImpl.h
     qpid/messaging/Session.cpp
     qpid/messaging/SessionImpl.h
     qpid/messaging/Sender.cpp
     qpid/messaging/SenderImpl.h
     qpid/messaging/Variant.cpp
     qpid/client/amqp0_10/AcceptTracker.h
     qpid/client/amqp0_10/AcceptTracker.cpp
     qpid/client/amqp0_10/AddressResolution.h
     qpid/client/amqp0_10/AddressResolution.cpp
     qpid/client/amqp0_10/Codecs.cpp
     qpid/client/amqp0_10/CodecsInternal.h
     qpid/client/amqp0_10/ConnectionImpl.h
     qpid/client/amqp0_10/ConnectionImpl.cpp
     qpid/client/amqp0_10/IncomingMessages.h
     qpid/client/amqp0_10/IncomingMessages.cpp
     qpid/client/amqp0_10/MessageSink.h
     qpid/client/amqp0_10/MessageSource.h
     qpid/client/amqp0_10/OutgoingMessage.h
     qpid/client/amqp0_10/OutgoingMessage.cpp
     qpid/client/amqp0_10/ReceiverImpl.h
     qpid/client/amqp0_10/ReceiverImpl.cpp
     qpid/client/amqp0_10/SessionImpl.h
     qpid/client/amqp0_10/SessionImpl.cpp
     qpid/client/amqp0_10/SenderImpl.h
     qpid/client/amqp0_10/SenderImpl.cpp
)

add_library (qpidclient SHARED ${qpidclient_SOURCES})
target_link_libraries (qpidclient qpidcommon ${qpidclient_platform_LIBS})
set_target_properties (qpidclient PROPERTIES VERSION ${qpidc_version})
install (TARGETS qpidclient
         DESTINATION ${QPID_INSTALL_LIBDIR}
         COMPONENT ${QPID_COMPONENT_CLIENT})
install (DIRECTORY ../include/qpid ${CMAKE_CURRENT_BINARY_DIR}/../include/qpid
         DESTINATION ${QPID_INSTALL_INCLUDEDIR}
         COMPONENT ${QPID_COMPONENT_CLIENT_INCLUDE}
         PATTERN ".svn" EXCLUDE)
if (WIN32)
    install (PROGRAMS
             ${CMAKE_CURRENT_BINARY_DIR}/Debug/qpidclientd.dll
             ${CMAKE_CURRENT_BINARY_DIR}/Debug/qpidclientd.lib
             ${CMAKE_CURRENT_BINARY_DIR}/Debug/qpidclientd.pdb
             DESTINATION ${QPID_INSTALL_LIBDIR}
             COMPONENT ${QPID_COMPONENT_CLIENT})
endif (WIN32)

if (WIN32)
    set(AMQP_WCF_DIR ${qpid-cpp_SOURCE_DIR}/../wcf)
    set(DTC_PLUGIN_SOURCE ${AMQP_WCF_DIR}/src/Apache/Qpid/DtcPlugin/DtcPlugin.cpp)
    if (EXISTS ${DTC_PLUGIN_SOURCE})
        add_library (qpidxarm SHARED ${DTC_PLUGIN_SOURCE})
        target_link_libraries (qpidxarm qpidclient qpidcommon)
    endif (EXISTS ${DTC_PLUGIN_SOURCE})
endif (WIN32)

set (qpidbroker_SOURCES
     ${mgen_broker_cpp}
     ${qpidbroker_platform_SOURCES}
     qpid/amqp_0_10/Connection.h
     qpid/amqp_0_10/Connection.cpp
     qpid/broker/Broker.cpp
     qpid/broker/Exchange.cpp
     qpid/broker/ExpiryPolicy.cpp
     qpid/broker/Queue.cpp
     qpid/broker/QueueCleaner.cpp
     qpid/broker/QueueListeners.cpp
     qpid/broker/PersistableMessage.cpp
     qpid/broker/Bridge.cpp
     qpid/broker/Connection.cpp
     qpid/broker/ConnectionHandler.cpp
     qpid/broker/ConnectionFactory.cpp
     qpid/broker/DeliverableMessage.cpp
     qpid/broker/DeliveryRecord.cpp
     qpid/broker/DirectExchange.cpp
     qpid/broker/DtxAck.cpp
     qpid/broker/DtxBuffer.cpp
     qpid/broker/DtxManager.cpp
     qpid/broker/DtxTimeout.cpp
     qpid/broker/DtxWorkRecord.cpp
     qpid/broker/ExchangeRegistry.cpp
     qpid/broker/FanOutExchange.cpp
     qpid/broker/HeadersExchange.cpp
     qpid/broker/IncompleteMessageList.cpp
     qpid/broker/Link.cpp
     qpid/broker/LinkRegistry.cpp
     qpid/broker/Message.cpp
     qpid/broker/MessageAdapter.cpp
     qpid/broker/MessageBuilder.cpp
     qpid/broker/MessageStoreModule.cpp
     qpid/broker/NameGenerator.cpp
     qpid/broker/NullMessageStore.cpp
     qpid/broker/QueueBindings.cpp
     qpid/broker/QueueEvents.cpp
     qpid/broker/QueuePolicy.cpp
     qpid/broker/QueueRegistry.cpp
     qpid/broker/RateTracker.cpp
     qpid/broker/RecoveryManagerImpl.cpp
     qpid/broker/RecoveredEnqueue.cpp
     qpid/broker/RecoveredDequeue.cpp
     qpid/broker/RetryList.cpp
     qpid/broker/SecureConnection.cpp
     qpid/broker/SecureConnectionFactory.cpp
     qpid/broker/SemanticState.h
     qpid/broker/SemanticState.cpp
     qpid/broker/SessionAdapter.cpp
     qpid/broker/SessionState.h
     qpid/broker/SessionState.cpp
     qpid/broker/SessionManager.h
     qpid/broker/SessionManager.cpp
     qpid/broker/SessionContext.h
     qpid/broker/SessionHandler.h
     qpid/broker/SessionHandler.cpp
     qpid/broker/System.cpp
     qpid/broker/TopicExchange.cpp
     qpid/broker/TxAccept.cpp
     qpid/broker/TxBuffer.cpp
     qpid/broker/TxPublish.cpp
     qpid/broker/Vhost.cpp
     qpid/management/ManagementAgent.cpp
     qpid/management/ManagementExchange.cpp
     qpid/sys/TCPIOPlugin.cpp
)
add_library (qpidbroker SHARED ${qpidbroker_SOURCES})
target_link_libraries (qpidbroker qpidcommon ${qpidbroker_platform_LIBS})
set_target_properties (qpidbroker PROPERTIES VERSION ${qpidc_version})
if (MSVC)
  set_target_properties (qpidbroker PROPERTIES COMPILE_FLAGS /wd4290)
endif (MSVC)
install (TARGETS qpidbroker
         DESTINATION ${QPID_INSTALL_LIBDIR}
         COMPONENT ${QPID_COMPONENT_BROKER})

set (qpidd_SOURCES
     ${qpidd_platform_SOURCES}
     qpidd.cpp
     qpidd.h
)
add_executable (qpidd ${qpidd_SOURCES})
target_link_libraries (qpidd qpidbroker qpidcommon ${Boost_PROGRAM_OPTIONS_LIBRARY}
                          ${Boost_FILESYSTEM_LIBRARY})
install (TARGETS qpidd RUNTIME
         DESTINATION ${QPID_INSTALL_BINDIR}
         COMPONENT ${QPID_COMPONENT_BROKER})
if (CPACK_GENERATOR STREQUAL "NSIS")
    set (CPACK_NSIS_MENU_LINKS
         "qpidd" "Start Qpid Broker")
endif (CPACK_GENERATOR STREQUAL "NSIS")

# QMF library
# Library Version Information (CURRENT.REVISION.AGE):
#
#  CURRENT  => API/ABI version.  Bump this if the interface changes
#  REVISION => Version of underlying implementation.
#              Bump if implementation changes but API/ABI doesn't
#  AGE      => Number of API/ABI versions this is backward compatible with
set (qmf_version 1.0.0)
set (qmfengine_version 1.0.0)

set (qmf_SOURCES
    qpid/agent/ManagementAgentImpl.cpp
    qpid/agent/ManagementAgentImpl.h
    )
add_library (qmf SHARED ${qmf_SOURCES})
target_link_libraries (qmf qmfengine)
set_target_properties (qmf PROPERTIES
                       VERSION ${qmf_version})
install (TARGETS qmf OPTIONAL
         DESTINATION ${QPID_INSTALL_LIBDIR}
         COMPONENT ${QPID_COMPONENT_QMF})

set (qmfengine_SOURCES
     qmf/engine/Agent.cpp
     qmf/engine/BrokerProxyImpl.cpp
     qmf/engine/BrokerProxyImpl.h
     qmf/engine/ConnectionSettingsImpl.cpp
     qmf/engine/ConnectionSettingsImpl.h
     qmf/engine/ConsoleImpl.cpp
     qmf/engine/ConsoleImpl.h
     qmf/engine/MessageImpl.cpp
     qmf/engine/MessageImpl.h
     qmf/engine/ObjectIdImpl.cpp
     qmf/engine/ObjectIdImpl.h
     qmf/engine/ObjectImpl.cpp
     qmf/engine/ObjectImpl.h
     qmf/engine/Protocol.cpp
     qmf/engine/Protocol.h
     qmf/engine/QueryImpl.cpp
     qmf/engine/QueryImpl.h
     qmf/engine/ResilientConnection.cpp
     qmf/engine/SequenceManager.cpp
     qmf/engine/SequenceManager.h
     qmf/engine/SchemaImpl.cpp
     qmf/engine/SchemaImpl.h
     qmf/engine/ValueImpl.cpp
     qmf/engine/ValueImpl.h
    )
add_library (qmfengine SHARED ${qmfengine_SOURCES})
target_link_libraries (qmfengine qpidclient)
set_target_properties (qmfengine PROPERTIES
                       VERSION ${qmfengine_version})
install (TARGETS qmfengine OPTIONAL
         DESTINATION ${QPID_INSTALL_LIBDIR}
         COMPONENT ${QPID_COMPONENT_QMF})

# QMF console library
#module_hdr += \
#  qpid/console/Agent.h \
#  qpid/console/Broker.h \
#  qpid/console/ClassKey.h \
#  qpid/console/ConsoleImportExport.h \
#  qpid/console/ConsoleListener.h \
#  qpid/console/Event.h \
#  qpid/console/Object.h \
#  qpid/console/ObjectId.h \
#  qpid/console/Package.h \
#  qpid/console/Schema.h \
#  qpid/console/SequenceManager.h \
#  qpid/console/SessionManager.h \
#  qpid/console/Value.h
set (qmfconsole_SOURCES
     ../include/qpid/console/Agent.h
     ../include/qpid/console/Broker.h
     ../include/qpid/console/ClassKey.h
     ../include/qpid/console/ConsoleImportExport.h
     ../include/qpid/console/ConsoleListener.h
     ../include/qpid/console/Event.h
     ../include/qpid/console/Object.h
     ../include/qpid/console/ObjectId.h
     ../include/qpid/console/Package.h
     ../include/qpid/console/Schema.h
     ../include/qpid/console/SequenceManager.h
     ../include/qpid/console/SessionManager.h
     ../include/qpid/console/Value.h
     qpid/console/Agent.cpp
     qpid/console/Broker.cpp
     qpid/console/ClassKey.cpp
     qpid/console/Event.cpp
     qpid/console/Object.cpp
     qpid/console/ObjectId.cpp
     qpid/console/Package.cpp
     qpid/console/Schema.cpp
     qpid/console/SequenceManager.cpp
     qpid/console/SessionManager.cpp
     qpid/console/Value.cpp
    )
add_library (qmfconsole SHARED ${qmfconsole_SOURCES})
target_link_libraries (qmfconsole qpidclient)
set_target_properties (qmfconsole PROPERTIES
                       VERSION ${qpidc_version})
install (TARGETS qmfconsole
         DESTINATION ${QPID_INSTALL_LIBDIR}
         COMPONENT ${QPID_COMPONENT_QMF})

# A queue event listener plugin that creates messages on a replication
# queue corresponding to enqueue and dequeue events:
add_library (replicating_listener MODULE
             qpid/replication/constants.h
             qpid/replication/ReplicatingEventListener.cpp
             qpid/replication/ReplicatingEventListener.h
            )
target_link_libraries (replicating_listener qpidbroker ${Boost_PROGRAM_OPTIONS_LIBRARY})
set_target_properties (replicating_listener PROPERTIES PREFIX "")
if (CMAKE_COMPILER_IS_GNUCXX)
  set_target_properties(replicating_listener PROPERTIES
                        LINK_FLAGS -Wl,--no-undefined)
endif (CMAKE_COMPILER_IS_GNUCXX)
install (TARGETS replicating_listener
         DESTINATION ${QPIDD_MODULE_DIR}
         COMPONENT ${QPID_COMPONENT_BROKER})

# A custom exchange plugin that allows an exchange to be created that
# can process the messages from a replication queue (populated on the
# source system by the replicating listener plugin above) and take the
# corresponding action on the local queues
add_library (replication_exchange MODULE
             qpid/replication/constants.h
             qpid/replication/ReplicationExchange.cpp
             qpid/replication/ReplicationExchange.h
            )
target_link_libraries (replication_exchange qpidbroker)
set_target_properties (replication_exchange PROPERTIES PREFIX "")
if (CMAKE_COMPILER_IS_GNUCXX)
  set_target_properties(replication_exchange PROPERTIES
                        LINK_FLAGS -Wl,--no-undefined)
endif (CMAKE_COMPILER_IS_GNUCXX)
install (TARGETS replication_exchange
         DESTINATION ${QPIDD_MODULE_DIR}
         COMPONENT ${QPID_COMPONENT_BROKER})

# This is only really needed until all the trunk builds (Linux, UNIX, Windows)
# are all on cmake only. This is because cmake builds always have a config.h
# file whereas older builds only have config.h on autoconf-generated builds.
add_definitions(-DHAVE_CONFIG_H)

# Now create the config file from all the info learned above.
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.cmake
               ${CMAKE_CURRENT_BINARY_DIR}/config.h)

add_subdirectory(qpid/store)
add_subdirectory(tests)
