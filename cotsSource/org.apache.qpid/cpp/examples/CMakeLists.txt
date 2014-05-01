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
project(qpidc_examples)
cmake_minimum_required(VERSION 2.4.0 FATAL_ERROR)
if(COMMAND cmake_policy)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

include_directories(${CMAKE_BINARY_DIR}/include)
include_directories(${CMAKE_SOURCE_DIR}/include)

# Shouldn't need this... but there are still client header inclusions
# of Boost. When building examples at an install site, the Boost files
# should be locatable aside from these settings.
# So set up to find the headers, find the libs at link time, but dynamically
# link them all and clear the CMake Boost library names to avoid adding them to
# the project files.
include_directories( ${Boost_INCLUDE_DIR} )
link_directories( ${Boost_LIBRARY_DIRS} )

# Visual Studio needs some Windows-specific simplifications.
# Linux needs to reference the boost libs, even though they should be
# resolved via the Qpid libs.
if (MSVC)
  add_definitions( /D "NOMINMAX" /D "WIN32_LEAN_AND_MEAN" /D "BOOST_ALL_DYN_LINK" )
  # On Windows, prevent the accidental inclusion of Boost headers from
  # autolinking in the Boost libs. There should be no direct references to
  # Boost in the examples, and references via qpidclient/qpidcommon are
  # resolved in the Qpid libs.
  add_definitions( /D "BOOST_ALL_NO_LIB" )
else (MSVC)
  set(_boost_libs_needed ${Boost_PROGRAM_OPTIONS_LIBRARY}
                         ${Boost_FILESYSTEM_LIBRARY})
endif (MSVC)

# There are numerous duplicate names within the examples. Since all target
# names must be unique, define a macro to prepend a prefix and manage the
# actual names.
# There can be an optional arguments at the end: libs to include
macro(add_example subdir example)
  add_executable(${subdir}_${example} ${example}.cpp)
  set_target_properties(${subdir}_${example} PROPERTIES OUTPUT_NAME ${example})
  if (${ARGC} GREATER 2)
    target_link_libraries(${subdir}_${example} ${ARGN} qpidclient
                          ${_boost_libs_needed})
  else (${ARGC} GREATER 2)
    target_link_libraries(${subdir}_${example} qpidclient
                          ${_boost_libs_needed})
  endif (${ARGC} GREATER 2)
  # For installs, don't install the built example; that would be pointless.
  # Install the things a user needs to build the example on-site.
  install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/${example}.cpp
           DESTINATION ${QPID_INSTALL_EXAMPLESDIR}/${subdir}
           COMPONENT ${QPID_COMPONENT_EXAMPLES})
  if (MSVC)
    install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/${subdir}_${example}.vcproj
             DESTINATION ${QPID_INSTALL_EXAMPLESDIR}/${subdir}
             COMPONENT ${QPID_COMPONENT_EXAMPLES})
  endif (MSVC)

endmacro(add_example)

install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/README.txt
               ${CMAKE_CURRENT_SOURCE_DIR}/README.verify
               ${CMAKE_CURRENT_SOURCE_DIR}/verify
               ${CMAKE_CURRENT_SOURCE_DIR}/verify_all
         DESTINATION ${QPID_INSTALL_EXAMPLESDIR}
         COMPONENT ${QPID_COMPONENT_EXAMPLES})
if (MSVC)
  install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/examples.sln
           DESTINATION ${QPID_INSTALL_EXAMPLESDIR}
           COMPONENT ${QPID_COMPONENT_EXAMPLES})
endif (MSVC)

add_subdirectory(direct)
add_subdirectory(failover)
add_subdirectory(fanout)
add_subdirectory(pub-sub)
#add_subdirectory(qmf-agent)
add_subdirectory(qmf-console)
add_subdirectory(request-response)
add_subdirectory(tradedemo)
add_subdirectory(xml-exchange)
add_subdirectory(messaging)
