//
// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
// 
//   http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//

#include "qpid/sys/PipeHandle.h"
#include "qpid/sys/windows/check.h"
#include <winsock2.h>

namespace qpid {
namespace sys {

PipeHandle::PipeHandle(bool nonBlocking) {

    SOCKET listener, pair[2];
    struct sockaddr_in addr;
    int err;
    int addrlen = sizeof(addr);
    pair[0] = pair[1] = INVALID_SOCKET;
    if ((listener = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
        throw QPID_WINDOWS_ERROR(WSAGetLastError());

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = 0;

    err = bind(listener, (const struct sockaddr*) &addr, sizeof(addr));
    if (err == SOCKET_ERROR) {
        err = WSAGetLastError();
        closesocket(listener);
        throw QPID_WINDOWS_ERROR(err);
    }

    err = getsockname(listener, (struct sockaddr*) &addr, &addrlen);
    if (err == SOCKET_ERROR) {
        err = WSAGetLastError();
        closesocket(listener);
        throw QPID_WINDOWS_ERROR(err);
    }

    try {
        if (listen(listener, 1) == SOCKET_ERROR)
            throw QPID_WINDOWS_ERROR(WSAGetLastError());
        if ((pair[0] = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
            throw QPID_WINDOWS_ERROR(WSAGetLastError());
        if (connect(pair[0], (const struct sockaddr*)&addr, sizeof(addr)) == SOCKET_ERROR)
            throw QPID_WINDOWS_ERROR(WSAGetLastError());
        if ((pair[1] = accept(listener, NULL, NULL)) == INVALID_SOCKET)
            throw QPID_WINDOWS_ERROR(WSAGetLastError());

        closesocket(listener);
        writeFd = pair[0];
        readFd = pair[1]; 
    }
    catch (...) {
        closesocket(listener);
        if (pair[0] != INVALID_SOCKET)
            closesocket(pair[0]);
        throw;
    }

    // Set the socket to non-blocking
    if (nonBlocking) {
        unsigned long nonblock = 1;
        ioctlsocket(readFd, FIONBIO, &nonblock);
    }
}

PipeHandle::~PipeHandle() {
    closesocket(readFd);
    closesocket(writeFd);
}

int PipeHandle::read(void* buf, size_t bufSize) {
    return ::recv(readFd, (char *)buf, bufSize, 0);
}

int PipeHandle::write(const void* buf, size_t bufSize) {
    return ::send(writeFd, (const char *)buf, bufSize, 0);
}

int PipeHandle::getReadHandle() {
    return readFd;
}

}} // namespace qpid::sys
