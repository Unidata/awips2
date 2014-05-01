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

#include "qpid/sys/SocketAddress.h"
#include "qpid/sys/posix/check.h"
#include "qpid/sys/posix/PrivatePosix.h"

#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <cstdlib>
#include <string.h>
#include <iostream>

#include <boost/format.hpp>
#include <boost/lexical_cast.hpp>

namespace qpid {
namespace sys {

namespace {
std::string getName(int fd, bool local, bool includeService = false)
{
    ::sockaddr_storage name; // big enough for any socket address    
    ::socklen_t namelen = sizeof(name);
    
    int result = -1;
    if (local) {
        result = ::getsockname(fd, (::sockaddr*)&name, &namelen);
    } else {
        result = ::getpeername(fd, (::sockaddr*)&name, &namelen);
    }

    QPID_POSIX_CHECK(result);

    char servName[NI_MAXSERV];
    char dispName[NI_MAXHOST];
    if (includeService) {
        if (int rc=::getnameinfo((::sockaddr*)&name, namelen, dispName, sizeof(dispName), 
                                 servName, sizeof(servName), 
                                 NI_NUMERICHOST | NI_NUMERICSERV) != 0)
            throw QPID_POSIX_ERROR(rc);
        return std::string(dispName) + ":" + std::string(servName);

    } else {
        if (int rc=::getnameinfo((::sockaddr*)&name, namelen, dispName, sizeof(dispName), 0, 0, NI_NUMERICHOST) != 0)
            throw QPID_POSIX_ERROR(rc);
        return dispName;
    }
}

std::string getService(int fd, bool local)
{
    ::sockaddr_storage name; // big enough for any socket address    
    ::socklen_t namelen = sizeof(name);
    
    int result = -1;
    if (local) {
        result = ::getsockname(fd, (::sockaddr*)&name, &namelen);
    } else {
        result = ::getpeername(fd, (::sockaddr*)&name, &namelen);
    }

    QPID_POSIX_CHECK(result);

    char servName[NI_MAXSERV];
    if (int rc=::getnameinfo((::sockaddr*)&name, namelen, 0, 0, 
                                 servName, sizeof(servName), 
                                 NI_NUMERICHOST | NI_NUMERICSERV) != 0)
        throw QPID_POSIX_ERROR(rc);
    return servName;
}
}

Socket::Socket() :
    IOHandle(new IOHandlePrivate),
    nonblocking(false),
    nodelay(false)
{}

Socket::Socket(IOHandlePrivate* h) :
    IOHandle(h),
    nonblocking(false),
    nodelay(false)
{}

void Socket::createSocket(const SocketAddress& sa) const
{
    int& socket = impl->fd;
    if (socket != -1) Socket::close();
    int s = ::socket(getAddrInfo(sa).ai_family, getAddrInfo(sa).ai_socktype, 0);
    if (s < 0) throw QPID_POSIX_ERROR(errno);
    socket = s;

    try {
        if (nonblocking) setNonblocking();
        if (nodelay) setTcpNoDelay();
    } catch (std::exception&) {
        ::close(s);
        socket = -1;
        throw;
    }
}

void Socket::setTimeout(const Duration& interval) const
{
    const int& socket = impl->fd;
    struct timeval tv;
    toTimeval(tv, interval);
    setsockopt(socket, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
    setsockopt(socket, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
}

void Socket::setNonblocking() const {
    int& socket = impl->fd;
    nonblocking = true;
    if (socket != -1) {
        QPID_POSIX_CHECK(::fcntl(socket, F_SETFL, O_NONBLOCK));
    }
}

void Socket::setTcpNoDelay() const
{
    int& socket = impl->fd;
    nodelay = true;
    if (socket != -1) {
        int flag = 1;
        int result = setsockopt(impl->fd, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(flag));
        QPID_POSIX_CHECK(result);
    }
}

void Socket::connect(const std::string& host, uint16_t port) const
{
    SocketAddress sa(host, boost::lexical_cast<std::string>(port));
    connect(sa);
}

void Socket::connect(const SocketAddress& addr) const
{
    connectname = addr.asString();

    createSocket(addr);

    const int& socket = impl->fd;
    // TODO the correct thing to do here is loop on failure until you've used all the returned addresses
    if ((::connect(socket, getAddrInfo(addr).ai_addr, getAddrInfo(addr).ai_addrlen) < 0) &&
        (errno != EINPROGRESS)) {
        throw Exception(QPID_MSG(strError(errno) << ": " << connectname));
    }
}

void
Socket::close() const
{
    int& socket = impl->fd;
    if (socket == -1) return;
    if (::close(socket) < 0) throw QPID_POSIX_ERROR(errno);
    socket = -1;
}

int Socket::listen(uint16_t port, int backlog) const
{
    SocketAddress sa("", boost::lexical_cast<std::string>(port));
    return listen(sa, backlog);
}

int Socket::listen(const SocketAddress& sa, int backlog) const
{
    createSocket(sa);

    const int& socket = impl->fd;
    int yes=1;
    QPID_POSIX_CHECK(setsockopt(socket,SOL_SOCKET,SO_REUSEADDR,&yes,sizeof(yes)));

    if (::bind(socket, getAddrInfo(sa).ai_addr, getAddrInfo(sa).ai_addrlen) < 0)
        throw Exception(QPID_MSG("Can't bind to port " << sa.asString() << ": " << strError(errno)));
    if (::listen(socket, backlog) < 0)
        throw Exception(QPID_MSG("Can't listen on port " << sa.asString() << ": " << strError(errno)));

    struct sockaddr_in name;
    socklen_t namelen = sizeof(name);
    if (::getsockname(socket, (struct sockaddr*)&name, &namelen) < 0)
        throw QPID_POSIX_ERROR(errno);

    return ntohs(name.sin_port);
}

Socket* Socket::accept() const
{
    int afd = ::accept(impl->fd, 0, 0);
    if ( afd >= 0)
        return new Socket(new IOHandlePrivate(afd));
    else if (errno == EAGAIN)
        return 0;
    else throw QPID_POSIX_ERROR(errno);
}

int Socket::read(void *buf, size_t count) const
{
    return ::read(impl->fd, buf, count);
}

int Socket::write(const void *buf, size_t count) const
{
    return ::write(impl->fd, buf, count);
}

std::string Socket::getSockname() const
{
    return getName(impl->fd, true);
}

std::string Socket::getPeername() const
{
    return getName(impl->fd, false);
}

std::string Socket::getPeerAddress() const
{
    if (connectname.empty()) {
        connectname = getName(impl->fd, false, true);
    }
    return connectname;
}

std::string Socket::getLocalAddress() const
{
    return getName(impl->fd, true, true);
}

uint16_t Socket::getLocalPort() const
{
    return std::atoi(getService(impl->fd, true).c_str());
}

uint16_t Socket::getRemotePort() const
{
    return std::atoi(getService(impl->fd, true).c_str());
}

int Socket::getError() const
{
    int       result;
    socklen_t rSize = sizeof (result);

    if (::getsockopt(impl->fd, SOL_SOCKET, SO_ERROR, &result, &rSize) < 0)
        throw QPID_POSIX_ERROR(errno);

    return result;
}

}} // namespace qpid::sys
