#ifndef _sys_posix_PrivatePosix_h
#define _sys_posix_PrivatePosix_h

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

#include "qpid/sys/Time.h"
#include "qpid/sys/IOHandle.h"

struct timespec;
struct timeval;
struct addrinfo;

namespace qpid {
namespace sys {

// Private Time related implementation details
struct timespec& toTimespec(struct timespec& ts, const Duration& t);
struct timeval& toTimeval(struct timeval& tv, const Duration& t);
Duration toTime(const struct timespec& ts);

// Private SocketAddress details
class SocketAddress;
const struct addrinfo& getAddrInfo(const SocketAddress&);

// Private fd related implementation details
class IOHandlePrivate {
public:
    IOHandlePrivate(int f = -1) :
            fd(f)
    {}

    int fd;
};

int toFd(const IOHandlePrivate* h);

// Posix fd as an IOHandle
class PosixIOHandle : public IOHandle {
public:
    PosixIOHandle(int fd) :
        IOHandle(new IOHandlePrivate(fd))
    {}
};

// Dummy IOHandle for places it's required in the API
// but we promise not to actually try to do any operations on the IOHandle
class NullIOHandle : public IOHandle {
public:
    NullIOHandle() :
        IOHandle(new IOHandlePrivate)
    {}
};

extern NullIOHandle DummyIOHandle;

}}

#endif  /*!_sys_posix_PrivatePosix_h*/
