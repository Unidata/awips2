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

#include "qpid/sys/windows/check.h"

#include <ws2tcpip.h>
#include <string.h>

namespace qpid {
namespace sys {

SocketAddress::SocketAddress(const std::string& host0, const std::string& port0) :
    host(host0),
    port(port0),
    addrInfo(0)
{
    ::addrinfo hints;
    ::memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET; // In order to allow AF_INET6 we'd have to change createTcp() as well
    hints.ai_socktype = SOCK_STREAM;

    const char* node = 0;
    if (host.empty()) {
        hints.ai_flags |= AI_PASSIVE;
    } else {
        node = host.c_str();
    }
    const char* service = port.empty() ? "0" : port.c_str();

    int n = ::getaddrinfo(node, service, &hints, &addrInfo);
    if (n != 0)
        throw Exception(QPID_MSG("Cannot resolve " << host << ": " << ::gai_strerror(n)));
}

SocketAddress::~SocketAddress()
{
    ::freeaddrinfo(addrInfo);
}

std::string SocketAddress::asString() const
{
    return host + ":" + port;
}

const ::addrinfo& getAddrInfo(const SocketAddress& sa)
{
    return *sa.addrInfo;
}

}}
