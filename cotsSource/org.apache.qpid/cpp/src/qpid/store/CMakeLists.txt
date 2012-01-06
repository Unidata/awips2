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

project(qpidc_store)

#set (CMAKE_VERBOSE_MAKEFILE ON)  # for debugging

include_directories( ${Boost_INCLUDE_DIR} )

include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )
include_directories( ${CMAKE_HOME_DIRECTORY}/include )

link_directories( ${Boost_LIBRARY_DIRS} )

set (store_SOURCES
     MessageStorePlugin.cpp
    )
add_library (store MODULE ${store_SOURCES})
target_link_libraries (store qpidbroker ${Boost_PROGRAM_OPTIONS_LIBRARY})
if (CMAKE_COMPILER_IS_GNUCXX)
  set_target_properties (store PROPERTIES
                         PREFIX ""
                         LINK_FLAGS -Wl,--no-undefined)
endif (CMAKE_COMPILER_IS_GNUCXX)

if (CMAKE_SYSTEM_NAME STREQUAL Windows)
  if (MSVC)
    add_definitions( 
      /D "NOMINMAX"
      /D "WIN32_LEAN_AND_MEAN"
    )
  endif (MSVC)
endif (CMAKE_SYSTEM_NAME STREQUAL Windows)

set_target_properties (store PROPERTIES VERSION ${qpidc_version})
install (TARGETS store # RUNTIME
         DESTINATION ${QPIDD_MODULE_DIR}
         COMPONENT ${QPID_COMPONENT_BROKER})

# Build the MS SQL Storage Provider plugin
set (mssql_default ON)
if (NOT CMAKE_SYSTEM_NAME STREQUAL Windows)
  set(mssql_default OFF)
endif (NOT CMAKE_SYSTEM_NAME STREQUAL Windows)
option(BUILD_MSSQL "Build MS SQL Store provider plugin" ${mssql_default})
if (BUILD_MSSQL)
  add_library (mssql_store MODULE
               ms-sql/MsSqlProvider.cpp
               ms-sql/AmqpTransaction.cpp
               ms-sql/BindingRecordset.cpp
               ms-sql/BlobAdapter.cpp
               ms-sql/BlobEncoder.cpp
               ms-sql/BlobRecordset.cpp
               ms-sql/DatabaseConnection.cpp
               ms-sql/MessageMapRecordset.cpp
               ms-sql/MessageRecordset.cpp
               ms-sql/Recordset.cpp
               ms-sql/State.cpp
               ms-sql/VariantHelper.cpp)
  target_link_libraries (mssql_store qpidbroker qpidcommon ${Boost_PROGRAM_OPTIONS_LIBRARY})
  install (TARGETS mssql_store # RUNTIME
           DESTINATION ${QPIDD_MODULE_DIR}
           COMPONENT ${QPID_COMPONENT_BROKER})
endif (BUILD_MSSQL)
