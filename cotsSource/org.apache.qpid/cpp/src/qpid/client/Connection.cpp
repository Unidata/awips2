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
#include "qpid/client/Connection.h"
#include "qpid/client/ConnectionImpl.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/client/Message.h"
#include "qpid/client/SessionImpl.h"
#include "qpid/client/SessionBase_0_10Access.h"
#include "qpid/Url.h"
#include "qpid/log/Logger.h"
#include "qpid/log/Options.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/AMQP_HighestVersion.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <functional>
#include <boost/format.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>

using namespace qpid::framing;
using namespace qpid::sys;


namespace qpid {
namespace client {

Connection::Connection() : version(framing::highestProtocolVersion) {}

Connection::~Connection() {}

void Connection::open(
    const Url& url,
    const std::string& uid, const std::string& pwd, 
    const std::string& vhost,
    uint16_t maxFrameSize)
{
    ConnectionSettings settings;
    settings.username = uid;
    settings.password = pwd;
    settings.virtualhost = vhost;
    settings.maxFrameSize = maxFrameSize;
    open(url, settings);
}

void Connection::open(const Url& url, const ConnectionSettings& settings) {
    if (url.empty())
        throw Exception(QPID_MSG("Attempt to open URL with no addresses."));
    Url::const_iterator i = url.begin();
    do {
        const TcpAddress* tcp = i->get<TcpAddress>();
        i++;
        if (tcp) {
            try {
                ConnectionSettings cs(settings);
                cs.host = tcp->host;
                cs.port = tcp->port;
                open(cs);
                break;
            }
            catch (const Exception& /*e*/) {
                if (i == url.end()) throw;
            }
        }
    } while (i != url.end());
}

void Connection::open(
    const std::string& host, int port,
    const std::string& uid, const std::string& pwd, 
    const std::string& vhost,
    uint16_t maxFrameSize)
{
    ConnectionSettings settings; 
    settings.host = host;
    settings.port = port;
    settings.username = uid;
    settings.password = pwd;
    settings.virtualhost = vhost;
    settings.maxFrameSize = maxFrameSize;
    open(settings);
}

bool Connection::isOpen() const {
    return impl && impl->isOpen();
}

void 
Connection::registerFailureCallback ( boost::function<void ()> fn ) {
    failureCallback = fn;
    if ( impl )
        impl->registerFailureCallback ( fn );
}



void Connection::open(const ConnectionSettings& settings)
{
    if (isOpen())
        throw Exception(QPID_MSG("Connection::open() was already called"));

    impl = boost::shared_ptr<ConnectionImpl>(new ConnectionImpl(version, settings));
    impl->open();
    if ( failureCallback )
        impl->registerFailureCallback ( failureCallback );
}

const ConnectionSettings& Connection::getNegotiatedSettings()
{
    if (!isOpen())
        throw Exception(QPID_MSG("Connection is not open."));
     return impl->getNegotiatedSettings();
}

Session Connection::newSession(const std::string& name, uint32_t timeout) {
    if (!isOpen())
        throw Exception(QPID_MSG("Connection has not yet been opened"));
    Session s;
    SessionBase_0_10Access(s).set(impl->newSession(name, timeout));
    return s;
}

void Connection::resume(Session& session) {
    if (!isOpen())
        throw Exception(QPID_MSG("Connection is not open."));
    impl->addSession(session.impl);
    session.impl->resume(impl);
}

void Connection::close() {
    if ( impl )
        impl->close();
}

std::vector<Url> Connection::getInitialBrokers() {
    return impl ? impl->getInitialBrokers() : std::vector<Url>();
}

}} // namespace qpid::client
