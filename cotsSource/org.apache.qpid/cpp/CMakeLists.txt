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
project(qpid-cpp)

cmake_minimum_required(VERSION 2.4.0 FATAL_ERROR)
if(COMMAND cmake_policy)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

include(BuildInstallSettings.cmake)

set (qpidc_version ${QPID_VERSION_MAJOR}.${QPID_VERSION_MINOR})

enable_testing()
include (CTest)

# Overall packaging/install options.
# This section also has all the setup for various packaging-specific options.
set (CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
if (WIN32)
  # Include installing the MSVCRT library
  include(InstallRequiredSystemLibraries)
  set (CPACK_GENERATOR "NSIS")
  set (CPACK_NSIS_MUI_ICON "${CMAKE_CURRENT_SOURCE_DIR}/packaging/NSIS\\\\qpid-icon.ico")
  set (CPACK_NSIS_MUI_UNIICON "${CMAKE_CURRENT_SOURCE_DIR}/packaging/NSIS\\\\qpid-icon.ico")
  set (CPACK_PACKAGE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/packaging/NSIS\\\\qpid-install-banner.bmp")
  set (CPACK_NSIS_URL_INFO_ABOUT "http://qpid.apache.org/")
  # Needs this to correctly set up Start menu links later.
  set (CPACK_PACKAGE_EXECUTABLES "")

  # The WCF/C++ client is built separately (it doesn't build via CMake)
  # but is installed with the C++ components on Windows.
  install (PROGRAMS
           ${CMAKE_SOURCE_DIR}/../wcf/src/Apache/Qpid/Channel/bin/Release/Apache.Qpid.Channel.dll
           ${CMAKE_SOURCE_DIR}/../wcf/src/Apache/Qpid/Channel/bin/Release/Apache.Qpid.Interop.dll
           DESTINATION ${QPID_INSTALL_LIBDIR}
           COMPONENT ${QPID_COMPONENT_CLIENT})
  install (DIRECTORY ${CMAKE_SOURCE_DIR}/../wcf/samples/Channel
           DESTINATION ${QPID_INSTALL_EXAMPLESDIR}
           COMPONENT ${QPID_COMPONENT_EXAMPLES}
           PATTERN ".svn" EXCLUDE)

set (CPACK_NSIS_EXTRA_INSTALL_COMMANDS "
     ExecWait 'gacutil -I \\\"$INSTDIR\\\\${QPID_INSTALL_LIBDIR}\\\\Apache.Qpid.Channel.dll\\\"'
     ExecWait 'gacutil -I \\\"$INSTDIR\\\\${QPID_INSTALL_LIBDIR}\\\\Apache.Qpid.Interop.dll\\\"'
    ")
set (CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS "
     ExecWait 'gacutil /u \\\"Apache.Qpid.Channel\\\"'
     ExecWait 'gacutil /u \\\"Apache.Qpid.Interop\\\"'
    ")

endif (WIN32)

set (QPIDC_CONF_FILE ${QPID_INSTALL_CONFDIR}/qpidc.conf CACHE STRING
     "Name of the Qpid client configuration file")
set (QPIDD_CONF_FILE ${QPID_INSTALL_CONFDIR}/qpidd.conf CACHE STRING
     "Name of the Qpid broker configuration file")

install(FILES  LICENSE NOTICE README SSL RELEASE_NOTES DESIGN
               xml/cluster.xml INSTALL-WINDOWS
        DESTINATION ${QPID_INSTALL_DATADIR})

if (WIN32)
   set (CMAKE_DEBUG_POSTFIX "d")
endif (WIN32)

# set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_subdirectory(managementgen)
add_subdirectory(etc)
add_subdirectory(src)
add_subdirectory(docs/api)
# add_subdirectory(docs/man)
add_subdirectory(examples)

set(CPACK_PACKAGE_NAME "qpid-cpp")
set(CPACK_PACKAGE_VENDOR "Apache Software Foundation")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Apache Qpid C++")
set(CPACK_PACKAGE_VERSION "${qpidc_version}")
set(CPACK_PACKAGE_VERSION_MAJOR "${QPID_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${QPID_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "0")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "qpidc-${qpidc_version}")

include (CPack)
