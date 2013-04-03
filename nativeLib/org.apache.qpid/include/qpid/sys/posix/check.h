#ifndef _posix_check_h
#define _posix_check_h

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
#include "qpid/Msg.h"

#include <cerrno>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define QPID_POSIX_ERROR(ERRNO) qpid::Exception(QPID_MSG(qpid::sys::strError(ERRNO)))

/** THROW QPID_POSIX_ERROR(errno) if RESULT is less than zero */
#define QPID_POSIX_CHECK(RESULT)                        \
    if ((RESULT) < 0) throw QPID_POSIX_ERROR((errno))

/** Throw a posix error if ERRNO is non-zero */
#define QPID_POSIX_THROW_IF(ERRNO)              \
    do { int e=(ERRNO); if (e) throw QPID_POSIX_ERROR(e); } while(0)

/** Same as _THROW_IF in a release build, but abort a debug build */
#ifdef NDEBUG
#define QPID_POSIX_ASSERT_THROW_IF(ERRNO) QPID_POSIX_THROW_IF(ERRNO)
#else
#define QPID_POSIX_ASSERT_THROW_IF(ERRNO)                               \
    do { int e=(ERRNO); if (e) { errno=e; ::perror(0); assert(0); } } while(0)
#endif

#define QPID_POSIX_ABORT_IF(ERRNO) if ((int) ERRNO) { errno=ERRNO; ::perror(0); abort(); }

#endif  /*!_posix_check_h*/
