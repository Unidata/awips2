#ifndef SOCKETPROXY_H
#define SOCKETPROXY_H

/*
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

#include "qpid/sys/IOHandle.h"
#ifdef _WIN32
#  include "qpid/sys/windows/IoHandlePrivate.h"
   typedef SOCKET FdType;
#else
#  include "qpid/sys/posix/PrivatePosix.h"
   typedef int FdType;
#endif
#include "qpid/sys/Socket.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/Thread.h"
#include "qpid/sys/Mutex.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace tests {

/**
 * A simple socket proxy that forwards to another socket.
 * Used between client & local broker to simulate network failures.
 */
class SocketProxy : private qpid::sys::Runnable
{
    // Need a Socket we can get the fd from
    class LowSocket : public qpid::sys::Socket {
    public:
#ifdef _WIN32
        FdType getFd() { return toSocketHandle(*this); }
#else
        FdType getFd() { return toFd(impl); }
#endif
    };

  public:
    /** Connect to connectPort on host, start a forwarding thread.
     * Listen for connection on getPort().
     */
    SocketProxy(int connectPort, const std::string host="localhost")
      : closed(false), joined(true),
        port(listener.listen()), dropClient(), dropServer()
    {
        client.connect(host, connectPort);
        joined = false;
        thread = qpid::sys::Thread(static_cast<qpid::sys::Runnable*>(this));
    }

      ~SocketProxy() { close(); if (!joined) thread.join(); }

    /** Simulate a network disconnect. */
    void close() {
        {
            qpid::sys::Mutex::ScopedLock l(lock);
            if (closed) { return; }
            closed=true;
        }
        if (thread.id() != qpid::sys::Thread::current().id()) {
            thread.join();
            joined = true;
        }
        client.close();
    }

    /** Simulate lost packets, drop data from client */
    void dropClientData(bool drop=true) { dropClient=drop; }

    /** Simulate lost packets, drop data from server */
    void dropServerData(bool drop=true) { dropServer=drop; }

    bool isClosed() const {
        qpid::sys::Mutex::ScopedLock l(lock);
        return closed;
    }

    uint16_t getPort() const { return port; }

  private:
    static void throwErrno(const std::string& msg) {
        throw qpid::Exception(msg+":"+qpid::sys::strError(errno));
    }
    static void throwIf(bool condition, const std::string& msg) {
        if (condition) throw qpid::Exception(msg);
    }

    void run() {
        std::auto_ptr<LowSocket> server;
        try {
            fd_set socks;
            FdType maxFd = listener.getFd();
            struct timeval tmo;
            for (;;) {
                FD_ZERO(&socks);
                FD_SET(maxFd, &socks);
                tmo.tv_sec = 0;
                tmo.tv_usec = 500 * 1000;
                if (select(maxFd+1, &socks, 0, 0, &tmo) == 0) {
                    qpid::sys::Mutex::ScopedLock l(lock);
                    throwIf(closed, "SocketProxy: Closed by close()");
                    continue;
                }
                throwIf(!FD_ISSET(maxFd, &socks), "SocketProxy: Accept failed");
                break;   // Accept ready... go to next step
            }
            server.reset(reinterpret_cast<LowSocket *>(listener.accept()));
            maxFd = server->getFd();
            if (client.getFd() > maxFd)
                maxFd = client.getFd();
            char buffer[1024];
            for (;;) {
                FD_ZERO(&socks);
                tmo.tv_sec = 0;
                tmo.tv_usec = 500 * 1000;
                FD_SET(client.getFd(), &socks);
                FD_SET(server->getFd(), &socks);
                if (select(maxFd+1, &socks, 0, 0, &tmo) == 0) {
                    qpid::sys::Mutex::ScopedLock l(lock);
                    throwIf(closed, "SocketProxy: Closed by close()");
                    continue;
                }
                // Something is set; relay data as needed until something closes
                if (FD_ISSET(server->getFd(), &socks)) {
                    int n = server->read(buffer, sizeof(buffer));
                    throwIf(n <= 0, "SocketProxy: server disconnected");
                    if (!dropServer) client.write(buffer, n);
                }
                if (FD_ISSET(client.getFd(), &socks)) {
                    int n = client.read(buffer, sizeof(buffer));
                    throwIf(n <= 0, "SocketProxy: client disconnected");
                    if (!dropServer) server->write(buffer, n);
                }
                if (!FD_ISSET(client.getFd(), &socks) &&
                    !FD_ISSET(server->getFd(), &socks))
                    throwIf(true, "SocketProxy: No handle ready");
            }
        }
        catch (const std::exception& e) {
            QPID_LOG(debug, "SocketProxy::run exception: " << e.what());
        }
        try {
            if (server.get()) server->close();
            close();
        }
        catch (const std::exception& e) {
            QPID_LOG(debug, "SocketProxy::run exception in client/server close()" << e.what());
        }
    }

    mutable qpid::sys::Mutex lock;
    mutable bool closed;
    bool joined;
    LowSocket client, listener;
    uint16_t port;
    qpid::sys::Thread thread;
    bool dropClient, dropServer;
};

}} // namespace qpid::tests

#endif
