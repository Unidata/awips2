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

#include "qpid/sys/SocketAddress.h"

#include "qpid/sys/posix/check.h"

#include <sys/socket.h>
#include <string.h>
#include <netdb.h>

namespace qpid {
namespace sys {

SocketAddress::SocketAddress(const std::string& host0, const std::string& port0) :
    host(host0),
    port(port0),
    addrInfo(0)
{
}

SocketAddress::SocketAddress(const SocketAddress& sa) :
    host(sa.host),
    port(sa.port),
    addrInfo(0)
{
}

SocketAddress& SocketAddress::operator=(const SocketAddress& sa)
{
    if (&sa != this) {
        host = sa.host;
        port = sa.port;

        if (addrInfo) {
            ::freeaddrinfo(addrInfo);
            addrInfo = 0;
        }
    }
    return *this;
}

SocketAddress::~SocketAddress()
{
    if (addrInfo) {
        ::freeaddrinfo(addrInfo);
    }
}

std::string SocketAddress::asString() const
{
    return host + ":" + port;
}

const ::addrinfo& getAddrInfo(const SocketAddress& sa)
{
    if (!sa.addrInfo) {
        ::addrinfo hints;
        ::memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_INET; // In order to allow AF_INET6 we'd have to change createTcp() as well
        hints.ai_socktype = SOCK_STREAM;

        const char* node = 0;
        if (sa.host.empty()) {
            hints.ai_flags |= AI_PASSIVE;
        } else {
            node = sa.host.c_str();
        }
        const char* service = sa.port.empty() ? "0" : sa.port.c_str();

        int n = ::getaddrinfo(node, service, &hints, &sa.addrInfo);
        if (n != 0)
            throw Exception(QPID_MSG("Cannot resolve " << sa.host << ": " << ::gai_strerror(n)));
    }

    return *sa.addrInfo;
}

}}
