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
#include "qpid/client/ConnectionSettings.h"

#include "qpid/log/Logger.h"
#include "qpid/sys/Socket.h"
#include "qpid/Version.h"

namespace qpid {
namespace client {

ConnectionSettings::ConnectionSettings() :
    protocol("tcp"),
    host("localhost"), 
    port(TcpAddress::DEFAULT_PORT),
    locale("en_US"),
    heartbeat(0),
    maxChannels(32767),
    maxFrameSize(65535),
    bounds(2),
    tcpNoDelay(false),
    service(qpid::saslName),
    minSsf(0),
    maxSsf(256)
{}

ConnectionSettings::~ConnectionSettings() {}

void ConnectionSettings::configureSocket(qpid::sys::Socket& socket) const
{
    if (tcpNoDelay) {
        socket.setTcpNoDelay();
        QPID_LOG(info, "Set TCP_NODELAY");
    }
}

}} // namespace qpid::client
