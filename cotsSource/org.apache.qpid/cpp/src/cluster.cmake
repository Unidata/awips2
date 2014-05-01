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
# Cluster library CMake fragment, to be included in CMakeLists.txt
# 

# Optional cluster support. Requires CPG; if building it, can optionally
# include CMAN support as well.

include(CheckIncludeFiles)
include(CheckLibraryExists)

set(LIBCPG_PATH /usr/lib/openais /usr/lib64/openais /usr/lib/corosync /usr/lib64/corosync CACHE STRING "Default locations for libcpg (cluster library)" )
find_library(LIBCPG cpg ${LIBCPG_PATH})
if (LIBCPG)
  CHECK_LIBRARY_EXISTS (${LIBCPG} cpg_local_get "" HAVE_LIBCPG)
  CHECK_INCLUDE_FILES (openais/cpg.h HAVE_OPENAIS_CPG_H)
  CHECK_INCLUDE_FILES (corosync/cpg.h HAVE_COROSYNC_CPG_H)
endif (LIBCPG)

set (cluster_default ${cluster_force})
if (CMAKE_SYSTEM_NAME STREQUAL Windows)
else (CMAKE_SYSTEM_NAME STREQUAL Windows)
  if (HAVE_LIBCPG)
    if (HAVE_OPENAIS_CPG_H OR HAVE_COROSYNC_CPG_H)
      set (cluster_default ON)
    endif (HAVE_OPENAIS_CPG_H OR HAVE_COROSYNC_CPG_H)
  endif (HAVE_LIBCPG)
endif (CMAKE_SYSTEM_NAME STREQUAL Windows)

option(BUILD_CLUSTER "Build with CPG support for clustering" ${cluster_default})
if (BUILD_CLUSTER)

  if (NOT HAVE_LIBCPG)
    message(FATAL_ERROR "libcpg not found, install openais-devel or corosync-devel")
  endif (NOT HAVE_LIBCPG)
  if (NOT HAVE_OPENAIS_CPG_H AND NOT HAVE_COROSYNC_CPG_H)
    message(FATAL_ERROR "cpg.h not found, install openais-devel or corosync-devel")
  endif (NOT HAVE_OPENAIS_CPG_H AND NOT HAVE_COROSYNC_CPG_H)

  CHECK_LIBRARY_EXISTS (cman cman_is_quorate "" HAVE_LIBCMAN)
  CHECK_INCLUDE_FILES (libcman.h HAVE_LIBCMAN_H)

  set(cluster_quorum_default ${cluster_quorum_force})
  if (HAVE_LIBCMAN AND HAVE_LIBCMAN_H)
    set(cluster_quorum_default ON)
  endif (HAVE_LIBCMAN AND HAVE_LIBCMAN_H)

  option(BUILD_CLUSTER_QUORUM "Include libcman quorum service integration" ${cluster_quorum_default})
  if (BUILD_CLUSTER_QUORUM)
    if (NOT HAVE_LIBCMAN)
      message(FATAL_ERROR "libcman not found, install cman-devel or cmanlib-devel")
    endif (NOT HAVE_LIBCMAN)
    if (NOT HAVE_LIBCMAN_H)
      message(FATAL_ERROR "libcman.h not found, install cman-devel or cmanlib-devel")
    endif (NOT HAVE_LIBCMAN_H)

    set (CMAN_SOURCES qpid/cluster/Quorum_cman.h qpid/cluster/Quorum_cman.cpp)
    set (CMAN_LIB cman)
  else (BUILD_CLUSTER_QUORUM)
    set (CMAN_SOURCES qpid/cluster/Quorum_null.h)
  endif (BUILD_CLUSTER_QUORUM)

  set (cluster_SOURCES
       ${CMAN_SOURCES}
       qpid/cluster/Cluster.cpp
       qpid/cluster/Cluster.h
       qpid/cluster/Decoder.cpp
       qpid/cluster/Decoder.h
       qpid/cluster/PollableQueue.h
       qpid/cluster/ClusterMap.cpp
       qpid/cluster/ClusterMap.h
       qpid/cluster/ClusterPlugin.cpp
       qpid/cluster/ClusterSettings.h
       qpid/cluster/Connection.cpp
       qpid/cluster/Connection.h
       qpid/cluster/ConnectionCodec.cpp
       qpid/cluster/ConnectionCodec.h
       qpid/cluster/Cpg.cpp
       qpid/cluster/Cpg.h
       qpid/cluster/Dispatchable.h
       qpid/cluster/UpdateClient.cpp
       qpid/cluster/UpdateClient.h
       qpid/cluster/RetractClient.cpp
       qpid/cluster/RetractClient.h
       qpid/cluster/ErrorCheck.cpp
       qpid/cluster/ErrorCheck.h
       qpid/cluster/Event.cpp
       qpid/cluster/Event.h
       qpid/cluster/EventFrame.h
       qpid/cluster/EventFrame.cpp
       qpid/cluster/ExpiryPolicy.h
       qpid/cluster/ExpiryPolicy.cpp
       qpid/cluster/FailoverExchange.cpp
       qpid/cluster/FailoverExchange.h
       qpid/cluster/UpdateExchange.cpp
       qpid/cluster/UpdateExchange.h
       qpid/cluster/UpdateReceiver.h
       qpid/cluster/LockedConnectionMap.h
       qpid/cluster/Multicaster.cpp
       qpid/cluster/Multicaster.h
       qpid/cluster/McastFrameHandler.h
       qpid/cluster/NoOpConnectionOutputHandler.h
       qpid/cluster/Numbering.h
       qpid/cluster/OutputInterceptor.cpp
       qpid/cluster/OutputInterceptor.h
       qpid/cluster/PollerDispatch.cpp
       qpid/cluster/PollerDispatch.h
       qpid/cluster/ProxyInputHandler.h
       qpid/cluster/Quorum.h
       qpid/cluster/InitialStatusMap.h
       qpid/cluster/InitialStatusMap.cpp
       qpid/cluster/MemberSet.h
       qpid/cluster/MemberSet.cpp
       qpid/cluster/types.h
       qpid/cluster/StoreStatus.h
       qpid/cluster/StoreStatus.cpp
      )

  add_library (cluster MODULE ${cluster_SOURCES})
  target_link_libraries (cluster ${LIBCPG} ${CMAN_LIB} qpidbroker qpidclient ${Boost_FILESYSTEM_LIBRARY} ${Boost_SYSTEM_LIBRARY})
  set_target_properties (cluster PROPERTIES PREFIX "")
  
  # Create a second shared library for linking with test executables,
  # cmake will not allow a module to be linked with an executable.
  add_library (cluster_shared SHARED ${cluster_SOURCES})
  target_link_libraries (cluster_shared ${LIBCPG} ${CMAN_LIB} qpidbroker qpidclient ${Boost_FILESYSTEM_LIBRARY})

  if (CMAKE_COMPILER_IS_GNUCXX)
    set_target_properties(cluster PROPERTIES
                          LINK_FLAGS "-Wl,--no-undefined -pthread")
  endif (CMAKE_COMPILER_IS_GNUCXX)

  install (TARGETS cluster
           DESTINATION ${QPIDD_MODULE_DIR}
           COMPONENT ${QPID_COMPONENT_BROKER})

endif (BUILD_CLUSTER)

# Distribute all sources.
#EXTRA_DIST += qpid/cluster/Quorum_cman.h qpid/cluster/Quorum_cman.cpp qpid/cluster/Quorum_null.h
