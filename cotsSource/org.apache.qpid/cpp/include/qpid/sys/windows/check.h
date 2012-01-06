#ifndef _windows_check_h
#define _windows_check_h

/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

#include "qpid/Exception.h"
#include "qpid/sys/StrError.h"

#define QPID_WINDOWS_ERROR(ERRVAL) qpid::Exception(QPID_MSG(qpid::sys::strError(ERRVAL)))
#define QPID_WINDOWS_CRT_ERROR(ERRNO) qpid::Exception(QPID_MSG(qpid::sys::strError(ERRNO)))

/** THROW QPID_WINDOWS_ERROR(::GetLastError()) if RESULT is NULL */
#define QPID_WINDOWS_CHECK_NULL(RESULT)                        \
    if ((RESULT) == NULL) throw QPID_WINDOWS_ERROR((::GetLastError()))

#define QPID_WINDOWS_CHECK_NOT(RESULT,VAL)                                 \
  if ((RESULT) == (VAL)) throw QPID_WINDOWS_ERROR((::GetLastError()))

#define QPID_WINDOWS_CHECK_ASYNC_START(STATUS)                 \
    if (!(STATUS) && ::WSAGetLastError() != ERROR_IO_PENDING)  \
        throw QPID_WINDOWS_ERROR((::WSAGetLastError()))

#define QPID_WINDOWS_CHECK_CRT_NZ(VAL)   \
    if ((VAL) == 0) throw QPID_WINDOWS_CRT_ERROR(errno)

#define QPID_WINSOCK_CHECK(OP)                        \
    if ((OP) == SOCKET_ERROR) throw QPID_WINDOWS_ERROR((::WSAGetLastError()))

#endif  /*!_windows_check_h*/
