#ifndef QPID_CLIENT_CONNECTIONSETTINGS_H
#define QPID_CLIENT_CONNECTIONSETTINGS_H

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

#include "qpid/Options.h"
#include "qpid/log/Options.h"
#include "qpid/Url.h"
#include "qpid/client/ClientImportExport.h"

#include <iostream>
#include <exception>

namespace qpid {

namespace sys {
class Socket;
}

namespace client {

/**
 * Settings for a Connection.
 */
struct ConnectionSettings {

    QPID_CLIENT_EXTERN ConnectionSettings();
    QPID_CLIENT_EXTERN virtual ~ConnectionSettings();

    /**
     * Allows socket to be configured; default only sets tcp-nodelay
     * based on the flag set. Can be overridden.
     */
    QPID_CLIENT_EXTERN virtual void configureSocket(qpid::sys::Socket&) const;

    /**
     * The protocol used for the connection (defaults to 'tcp')
     */
    std::string protocol;

    /**
     * The host (or ip address) to connect to (defaults to 'localhost').
     */
    std::string host;
    /**
     * The port to connect to (defaults to 5672).
     */
    uint16_t port;
    /**
     * Allows an AMQP 'virtual host' to be specified for the
     * connection.
     */
    std::string virtualhost;

    /**
     * The username to use when authenticating the connection. If not
     * specified the current users login is used if available.
     */
    std::string username;
    /**
     * The password to use when authenticating the connection.
     */
    std::string password;
    /**
     * The SASL mechanism to use when authenticating the connection;
     * the options are currently PLAIN or ANONYMOUS.
     */
    std::string mechanism;
    /**
     * Allows a locale to be specified for the connection.
     */
    std::string locale;
    /**
     * Allows a heartbeat frequency to be specified
     */
    uint16_t heartbeat;
    /**
     * The maximum number of channels that the client will request for
     * use on this connection.
     */
    uint16_t maxChannels;
    /**
     * The maximum frame size that the client will request for this
     * connection.
     */
    uint16_t maxFrameSize;
    /**
     * Limit the size of the connections send buffer . The buffer
     * is limited to bounds * maxFrameSize.
     */
    uint bounds;
    /**
     * If true, TCP_NODELAY will be set for the connection.
     */
    bool tcpNoDelay;
    /**
     * SASL service name
     */
    std::string service;
    /**
     * Minimum acceptable strength of any SASL negotiated security
     * layer. 0 means no security layer required.
     */
    uint minSsf;
    /**
     * Maximum acceptable strength of any SASL negotiated security
     * layer. 0 means no security layer allowed.
     */
    uint maxSsf;
};

}} // namespace qpid::client

#endif  /*!QPID_CLIENT_CONNECTIONSETTINGS_H*/
