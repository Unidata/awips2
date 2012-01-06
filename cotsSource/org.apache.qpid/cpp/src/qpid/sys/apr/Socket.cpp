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


#include "qpid/sys/Socket.h"

#include "qpid/sys/apr/APRBase.h"
#include "qpid/sys/apr/APRPool.h"

#include <apr_network_io.h>

namespace qpid {
namespace sys {

class SocketPrivate {
public:
	SocketPrivate(apr_socket_t* s = 0) :
		socket(s)
	{}

	apr_socket_t* socket;
};

Socket::Socket() :
	impl(new SocketPrivate)
{
	createTcp();
}

Socket::Socket(SocketPrivate* sp) :
	impl(sp)
{}

Socket::~Socket() {
	delete impl;
}

void Socket::createTcp() const {
	apr_socket_t*& socket = impl->socket;
    apr_socket_t* s;
    CHECK_APR_SUCCESS(
        apr_socket_create(
            &s, APR_INET, SOCK_STREAM, APR_PROTO_TCP,
            APRPool::get()));
    socket = s;
}

void Socket::setTimeout(const Duration& interval) const {
	apr_socket_t*& socket = impl->socket;
    apr_socket_timeout_set(socket, interval/TIME_USEC);
}

void Socket::connect(const std::string& host, int port) const {
	apr_socket_t*& socket = impl->socket;
    apr_sockaddr_t* address;
    CHECK_APR_SUCCESS(
        apr_sockaddr_info_get(
            &address, host.c_str(), APR_UNSPEC, port, APR_IPV4_ADDR_OK,
            APRPool::get()));
    CHECK_APR_SUCCESS(apr_socket_connect(socket, address));
}

void Socket::close() const {
	apr_socket_t*& socket = impl->socket;
    if (socket == 0) return;
    CHECK_APR_SUCCESS(apr_socket_close(socket));
    socket = 0;
}

ssize_t Socket::send(const void* data, size_t size) const
{
	apr_socket_t*& socket = impl->socket;
    apr_size_t sent = size;
    apr_status_t status =
        apr_socket_send(socket, reinterpret_cast<const char*>(data), &sent);
    if (APR_STATUS_IS_TIMEUP(status)) return SOCKET_TIMEOUT;
    if (APR_STATUS_IS_EOF(status)) return SOCKET_EOF;
    CHECK_APR_SUCCESS(status);
    return sent;
}

ssize_t Socket::recv(void* data, size_t size) const
{
	apr_socket_t*& socket = impl->socket;
    apr_size_t received = size;
    apr_status_t status =
        apr_socket_recv(socket, reinterpret_cast<char*>(data), &received);
    if (APR_STATUS_IS_TIMEUP(status))
        return SOCKET_TIMEOUT;
    if (APR_STATUS_IS_EOF(status))
        return SOCKET_EOF;
    CHECK_APR_SUCCESS(status);
    return received;
}

}} // namespace qpid::sys
