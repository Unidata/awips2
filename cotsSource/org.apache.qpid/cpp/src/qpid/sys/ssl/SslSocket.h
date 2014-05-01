#ifndef _sys_ssl_Socket_h
#define _sys_ssl_Socket_h

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

#include "qpid/sys/IOHandle.h"
#include <nspr.h>

#include <string>

struct sockaddr;

namespace qpid {
namespace sys {

class Duration;

namespace ssl {

class SslSocket : public qpid::sys::IOHandle
{
public:
    /** Create a socket wrapper for descriptor. */
    SslSocket();
    
    /** Set socket non blocking */
    void setNonblocking() const;

    /** Set tcp-nodelay */
    void setTcpNoDelay(bool nodelay) const;

    void connect(const std::string& host, uint16_t port) const;

    void close() const;

    /** Bind to a port and start listening.
     *@param port 0 means choose an available port.
     *@param backlog maximum number of pending connections.
     *@param certName name of certificate to use to identify the server
     *@return The bound port.
     */
    int listen(uint16_t port = 0, int backlog = 10, const std::string& certName = "localhost.localdomain", bool clientAuth = false) const;
    
    /** 
     * Accept a connection from a socket that is already listening
     * and has an incoming connection
     */
    SslSocket* accept() const;

    // TODO The following are raw operations, maybe they need better wrapping? 
    int read(void *buf, size_t count) const;
    int write(const void *buf, size_t count) const;

    /** Returns the "socket name" ie the address bound to 
     * the near end of the socket
     */
    std::string getSockname() const;

    /** Returns the "peer name" ie the address bound to 
     * the remote end of the socket
     */
    std::string getPeername() const;

    /** 
     * Returns an address (host and port) for the remote end of the
     * socket
     */
    std::string getPeerAddress() const;
    /** 
     * Returns an address (host and port) for the local end of the
     * socket
     */
    std::string getLocalAddress() const;

    uint16_t getLocalPort() const;
    uint16_t getRemotePort() const;

    /**
     * Returns the error code stored in the socket.  This may be used
     * to determine the result of a non-blocking connect.
     */
    int getError() const;

    int getKeyLen() const;

private:
    mutable std::string connectname;
    mutable PRFileDesc* socket;
    /**
     * 'model' socket, with configuration to use when importing
     * accepted sockets for use as ssl sockets. Set on listen(), used
     * in accept to pass through to newly created socket instances.
     */
    mutable PRFileDesc* prototype;

    SslSocket(IOHandlePrivate* ioph, PRFileDesc* model);
};

}}}
#endif  /*!_sys_ssl_Socket_h*/
