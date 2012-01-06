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

if (GEN_DOXYGEN)
    # The user.doxygen.in file was originally written for a
    # configure-generated situation so makes use of configure names that
    # need to be set from the CMake equivalents.
    set (top_builddir ${CMAKE_BINARY_DIR})
    set (top_srcdir ${CMAKE_SOURCE_DIR})
    set (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/user.doxygen.in
                    ${CMAKE_CURRENT_BINARY_DIR}/user.doxygen)
    add_custom_target (user-api-docs COMMAND ${DOXYGEN_EXECUTABLE} user.doxygen)

    # HTML files are generated to ./html - put those in the install.
    install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html/
             DESTINATION ${QPID_INSTALL_HTMLDIR}
             COMPONENT ${QPID_COMPONENT_CLIENT_INCLUDE})
    if (CPACK_GENERATOR STREQUAL "NSIS")
        set (CPACK_NSIS_MENU_LINKS
             "${QPID_INSTALL_HTMLDIR}/index.html" "Qpid C++ API Documentation"
             "https://issues.apache.org/jira/browse/QPID" "Report Qpid Problem")
    endif (CPACK_GENERATOR STREQUAL "NSIS")
endif (GEN_DOXYGEN)
