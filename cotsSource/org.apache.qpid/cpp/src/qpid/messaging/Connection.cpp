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
#include "qpid/messaging/Connection.h"
#include "qpid/messaging/ConnectionImpl.h"
#include "qpid/messaging/Session.h"
#include "qpid/messaging/SessionImpl.h"
#include "qpid/client/PrivateImplRef.h"
#include "qpid/client/amqp0_10/ConnectionImpl.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace client {

typedef PrivateImplRef<qpid::messaging::Connection> PI;

}

namespace messaging {

using qpid::client::PI;

Connection Connection::open(const std::string& url, const Variant::Map& options)
{
    //only support amqp 0-10 at present
    Connection connection(new qpid::client::amqp0_10::ConnectionImpl(url, options));
    return connection;
}

Connection::Connection(ConnectionImpl* impl) { PI::ctor(*this, impl); }
Connection::Connection(const Connection& c) : qpid::client::Handle<ConnectionImpl>() { PI::copy(*this, c); }
Connection& Connection::operator=(const Connection& c) { return PI::assign(*this, c); }
Connection::~Connection() { PI::dtor(*this); }

void Connection::close() { impl->close(); }
Session Connection::newSession(const char* name) { return impl->newSession(false, name); }
Session Connection::newSession(const std::string& name) { return impl->newSession(false, name); }
Session Connection::newSession(bool transactional, const std::string& name)
{ 
    return impl->newSession(transactional, name);
}
Session Connection::getSession(const std::string& name) const { return impl->getSession(name); }

InvalidOptionString::InvalidOptionString(const std::string& msg) : Exception(msg) {}

void parseKeyValuePair(const std::string& in, Variant::Map& out)
{
    std::string::size_type i = in.find('=');
    if (i == std::string::npos || i == in.size() || in.find('=', i+1) != std::string::npos) {
        throw InvalidOptionString(QPID_MSG("Cannot parse name-value pair from " << in));
    } else {
        out[in.substr(0, i)] = in.substr(i+1);
    }
}

void parseOptionString(const std::string& in, Variant::Map& out)
{
    std::string::size_type start = 0;
    std::string::size_type i = in.find('&');
    while (i != std::string::npos) {
        parseKeyValuePair(in.substr(start, i-start), out);
        if (i < in.size()) {
            start = i+1;
            i = in.find('&', start);
        } else {
            i = std::string::npos;
        }
    }
    parseKeyValuePair(in.substr(start), out);
}

Variant::Map parseOptionString(const std::string& in)
{
    Variant::Map map;    
    parseOptionString(in, map);
    return map;
}

}} // namespace qpid::messaging
