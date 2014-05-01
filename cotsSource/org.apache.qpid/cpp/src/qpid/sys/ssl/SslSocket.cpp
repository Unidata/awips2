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

#include "qpid/sys/ssl/SslSocket.h"
#include "qpid/sys/ssl/check.h"
#include "qpid/sys/ssl/util.h"
#include "qpid/Exception.h"
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

#include <private/pprio.h>
#include <nss.h>
#include <pk11pub.h>
#include <ssl.h>
#include <key.h>

#include <boost/format.hpp>

namespace qpid {
namespace sys {
namespace ssl {

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

SslSocket::SslSocket() : IOHandle(new IOHandlePrivate()), socket(0), prototype(0) 
{ 
    impl->fd = ::socket (PF_INET, SOCK_STREAM, 0);
    if (impl->fd < 0) throw QPID_POSIX_ERROR(errno);
    socket = SSL_ImportFD(0, PR_ImportTCPSocket(impl->fd));
}

/**
 * This form of the constructor is used with the server-side sockets
 * returned from accept. Because we use posix accept rather than
 * PR_Accept, we have to reset the handshake.
 */
SslSocket::SslSocket(IOHandlePrivate* ioph, PRFileDesc* model) : IOHandle(ioph), socket(0), prototype(0)
{ 
    socket = SSL_ImportFD(model, PR_ImportTCPSocket(impl->fd));
    NSS_CHECK(SSL_ResetHandshake(socket, true));
}

void SslSocket::setNonblocking() const 
{
    PRSocketOptionData option;
    option.option = PR_SockOpt_Nonblocking;
    option.value.non_blocking = true;
    PR_SetSocketOption(socket, &option);
}

void SslSocket::connect(const std::string& host, uint16_t port) const
{
    std::stringstream namestream;
    namestream << host << ":" << port;
    connectname = namestream.str();

    void* arg = SslOptions::global.certName.empty() ? 0 : const_cast<char*>(SslOptions::global.certName.c_str());
    NSS_CHECK(SSL_GetClientAuthDataHook(socket, NSS_GetClientAuthData, arg));
    NSS_CHECK(SSL_SetURL(socket, host.data()));

    char hostBuffer[PR_NETDB_BUF_SIZE];
    PRHostEnt hostEntry;
    PR_CHECK(PR_GetHostByName(host.data(), hostBuffer, PR_NETDB_BUF_SIZE, &hostEntry));
    PRNetAddr address;
    int value = PR_EnumerateHostEnt(0, &hostEntry, port, &address);
    if (value < 0) {
        throw Exception(QPID_MSG("Error getting address for host: " << ErrorString()));
    } else if (value == 0) {
        throw Exception(QPID_MSG("Could not resolve address for host."));
    }
    PR_CHECK(PR_Connect(socket, &address, PR_INTERVAL_NO_TIMEOUT));
}

void SslSocket::close() const
{
    if (impl->fd > 0) {
        PR_Close(socket);
        impl->fd = -1;
    }
}

int SslSocket::listen(uint16_t port, int backlog, const std::string& certName, bool clientAuth) const
{
    //configure prototype socket:
    prototype = SSL_ImportFD(0, PR_NewTCPSocket());
    if (clientAuth) {
        NSS_CHECK(SSL_OptionSet(prototype, SSL_REQUEST_CERTIFICATE, PR_TRUE));
        NSS_CHECK(SSL_OptionSet(prototype, SSL_REQUIRE_CERTIFICATE, PR_TRUE));
    }

    //get certificate and key (is this the correct way?)
    CERTCertificate *cert = PK11_FindCertFromNickname(const_cast<char*>(certName.c_str()), 0);
    if (!cert) throw Exception(QPID_MSG("Failed to load certificate '" << certName << "'"));
    SECKEYPrivateKey *key = PK11_FindKeyByAnyCert(cert, 0);
    if (!key) throw Exception(QPID_MSG("Failed to retrieve private key from certificate"));
    NSS_CHECK(SSL_ConfigSecureServer(prototype, cert, key, NSS_FindCertKEAType(cert)));
    SECKEY_DestroyPrivateKey(key);
    CERT_DestroyCertificate(cert);

    //bind and listen
    const int& socket = impl->fd;
    int yes=1;
    QPID_POSIX_CHECK(setsockopt(socket,SOL_SOCKET,SO_REUSEADDR,&yes,sizeof(yes)));
    struct sockaddr_in name;
    name.sin_family = AF_INET;
    name.sin_port = htons(port);
    name.sin_addr.s_addr = 0;
    if (::bind(socket, (struct sockaddr*)&name, sizeof(name)) < 0)
        throw Exception(QPID_MSG("Can't bind to port " << port << ": " << strError(errno)));
    if (::listen(socket, backlog) < 0)
        throw Exception(QPID_MSG("Can't listen on port " << port << ": " << strError(errno)));
    
    socklen_t namelen = sizeof(name);
    if (::getsockname(socket, (struct sockaddr*)&name, &namelen) < 0)
        throw QPID_POSIX_ERROR(errno);

    return ntohs(name.sin_port);
}

SslSocket* SslSocket::accept() const
{
    int afd = ::accept(impl->fd, 0, 0);
    if ( afd >= 0) {
        return new SslSocket(new IOHandlePrivate(afd), prototype);
    } else if (errno == EAGAIN) {
        return 0;
    } else { 
        throw QPID_POSIX_ERROR(errno);
    }
}

int SslSocket::read(void *buf, size_t count) const
{
    return PR_Read(socket, buf, count);
}

int SslSocket::write(const void *buf, size_t count) const
{
    return PR_Write(socket, buf, count);
}

std::string SslSocket::getSockname() const
{
    return getName(impl->fd, true);
}

std::string SslSocket::getPeername() const
{
    return getName(impl->fd, false);
}

std::string SslSocket::getPeerAddress() const
{
    if (!connectname.empty())
        return connectname;
    return getName(impl->fd, false, true);
}

std::string SslSocket::getLocalAddress() const
{
    return getName(impl->fd, true, true);
}

uint16_t SslSocket::getLocalPort() const
{
    return std::atoi(getService(impl->fd, true).c_str());
}

uint16_t SslSocket::getRemotePort() const
{
    return atoi(getService(impl->fd, true).c_str());
}

int SslSocket::getError() const
{
    int       result;
    socklen_t rSize = sizeof (result);

    if (::getsockopt(impl->fd, SOL_SOCKET, SO_ERROR, &result, &rSize) < 0)
        throw QPID_POSIX_ERROR(errno);

    return result;
}

void SslSocket::setTcpNoDelay(bool nodelay) const
{
    if (nodelay) {
        PRSocketOptionData option;
        option.option = PR_SockOpt_NoDelay;
        option.value.no_delay = true;
        PR_SetSocketOption(socket, &option);
    }
}


/** get the bit length of the current cipher's key */
int SslSocket::getKeyLen() const
{
    int enabled = 0;
    int keySize = 0;
    SECStatus   rc;

    rc = SSL_SecurityStatus( socket,
                             &enabled,
                             NULL,
                             NULL,
                             &keySize,
                             NULL, NULL );
    if (rc == SECSuccess && enabled) {
        return keySize;
    }
    return 0;
}

}}} // namespace qpid::sys::ssl
