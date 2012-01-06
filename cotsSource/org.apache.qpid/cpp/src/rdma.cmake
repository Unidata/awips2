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
# RDMA (Remote DMA) wrapper CMake fragment, to be included in CMakeLists.txt
# 

# Optional RDMA support. Requires ibverbs and rdma_cm.

include(CheckIncludeFiles)
include(CheckLibraryExists)

CHECK_LIBRARY_EXISTS (ibverbs ibv_create_qp "" HAVE_IBVERBS)
CHECK_LIBRARY_EXISTS (rdmacm rdma_create_id "" HAVE_RDMACM)
CHECK_INCLUDE_FILES (infiniband/verbs.h HAVE_IBVERBS_H)
CHECK_INCLUDE_FILES (rdma/rdma_cma.h HAVE_RDMACM_H)

set (rdma_default ${rdma_force})
if (HAVE_IBVERBS AND HAVE_IBVERBS_H)
  if (HAVE_RDMACM AND HAVE_RDMACM_H)
    set (rdma_default ON)
  endif (HAVE_RDMACM AND HAVE_RDMACM_H)
endif (HAVE_IBVERBS AND HAVE_IBVERBS_H)

option(BUILD_RDMA "Build with support for Remote DMA protocols" ${rdma_default})
if (BUILD_RDMA)
  if (NOT HAVE_IBVERBS)
    message(FATAL_ERROR "libibverbs not found, required for RDMA support")
  endif (NOT HAVE_IBVERBS)
  if (NOT HAVE_RDMACM)
    message(FATAL_ERROR "librdmacm not found, required for RDMA support")
  endif (NOT HAVE_RDMACM)
  if (NOT HAVE_IBVERBS_H)
    message(FATAL_ERROR "ibverbs headers not found, required for RDMA support")
  endif (NOT HAVE_IBVERBS_H)
  if (NOT HAVE_RDMACM_H)
    message(FATAL_ERROR "rdmacm headers not found, required for RDMA support")
  endif (NOT HAVE_RDMACM_H)

  set (rdma_SOURCES
       qpid/sys/rdma/rdma_exception.h
       qpid/sys/rdma/rdma_factories.cpp
       qpid/sys/rdma/rdma_factories.h
       qpid/sys/rdma/RdmaIO.cpp
       qpid/sys/rdma/RdmaIO.h
       qpid/sys/rdma/rdma_wrap.cpp
       qpid/sys/rdma/rdma_wrap.h
      )

  add_library (rdmawrap SHARED ${rdma_SOURCES})
  target_link_libraries (rdmawrap qpidcommon rdmacm ibverbs)
  set_target_properties (rdmawrap PROPERTIES VERSION ${qpidc_version})
  if (CMAKE_COMPILER_IS_GNUCXX)
    set_target_properties(rdmawrap PROPERTIES
                          COMPILE_FLAGS -Wno-missing-field-initializers
                          LINK_FLAGS -Wl,--no-undefined)
  endif (CMAKE_COMPILER_IS_GNUCXX)

  install (TARGETS rdmawrap
           DESTINATION ${QPID_INSTALL_LIBDIR}
           COMPONENT ${QPID_COMPONENT_COMMON})

  add_library (rdma MODULE qpid/sys/RdmaIOPlugin.cpp)
  target_link_libraries (rdma qpidbroker rdmawrap)
  set_target_properties (rdma PROPERTIES
                         PREFIX "")

  if (CMAKE_COMPILER_IS_GNUCXX)
    set_target_properties(rdma PROPERTIES
                          COMPILE_FLAGS -Wno-missing-field-initializers
                          LINK_FLAGS -Wl,--no-undefined)
  endif (CMAKE_COMPILER_IS_GNUCXX)

  install (TARGETS rdma
           DESTINATION ${QPIDD_MODULE_DIR}
           COMPONENT ${QPID_COMPONENT_BROKER})

  add_library (rdmaconnector MODULE qpid/client/RdmaConnector.cpp)
  target_link_libraries (rdmaconnector qpidclient rdmawrap)
  set_target_properties (rdmaconnector PROPERTIES
                         PREFIX "")

  if (CMAKE_COMPILER_IS_GNUCXX)
    set_target_properties(rdmaconnector PROPERTIES
                          COMPILE_FLAGS -Wno-missing-field-initializers
                          LINK_FLAGS -Wl,--no-undefined)
  endif (CMAKE_COMPILER_IS_GNUCXX)

  install (TARGETS rdmaconnector
           DESTINATION ${QPIDC_MODULE_DIR}
           COMPONENT ${QPID_COMPONENT_CLIENT})

  # RDMA test/sample programs
  add_executable (RdmaServer qpid/sys/rdma/RdmaServer.cpp)
  target_link_libraries (RdmaServer rdmawrap qpidcommon)
  add_executable (RdmaClient qpid/sys/rdma/RdmaClient.cpp)
  target_link_libraries (RdmaClient rdmawrap qpidcommon)
  if (CMAKE_COMPILER_IS_GNUCXX)
    set_target_properties(RdmaClient PROPERTIES
                          COMPILE_FLAGS -Wno-missing-field-initializers)
  endif (CMAKE_COMPILER_IS_GNUCXX)

endif (BUILD_RDMA)
