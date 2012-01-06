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
#include "qpid/broker/SecureConnectionFactory.h"
#include "qpid/framing/ProtocolVersion.h"
#include "qpid/amqp_0_10/Connection.h"
#include "qpid/broker/Connection.h"
#include "qpid/broker/SecureConnection.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace broker {

using framing::ProtocolVersion;
typedef std::auto_ptr<amqp_0_10::Connection> CodecPtr;
typedef std::auto_ptr<SecureConnection> SecureConnectionPtr;
typedef std::auto_ptr<Connection> ConnectionPtr;
typedef std::auto_ptr<sys::ConnectionInputHandler> InputPtr;

SecureConnectionFactory::SecureConnectionFactory(Broker& b) : broker(b) {}

sys::ConnectionCodec*
SecureConnectionFactory::create(ProtocolVersion v, sys::OutputControl& out, const std::string& id,
                                unsigned int conn_ssf ) {
    if (broker.getConnectionCounter().allowConnection())
    {
        QPID_LOG(error, "Client max connection count limit exceeded: " << broker.getOptions().maxConnections << " connection refused");
        return 0;
    }
    if (v == ProtocolVersion(0, 10)) {
        SecureConnectionPtr sc(new SecureConnection());
        CodecPtr c(new amqp_0_10::Connection(out, id, false));
        ConnectionPtr i(new broker::Connection(c.get(), broker, id, conn_ssf, false));
        i->setSecureConnection(sc.get());
        c->setInputHandler(InputPtr(i.release()));
        sc->setCodec(std::auto_ptr<sys::ConnectionCodec>(c));
        return sc.release();
    }
    return 0;
}

sys::ConnectionCodec*
SecureConnectionFactory::create(sys::OutputControl& out, const std::string& id,
                                unsigned int conn_ssf) {
    // used to create connections from one broker to another
    SecureConnectionPtr sc(new SecureConnection());
    CodecPtr c(new amqp_0_10::Connection(out, id, true));
    ConnectionPtr i(new broker::Connection(c.get(), broker, id, conn_ssf, true ));
    i->setSecureConnection(sc.get());
    c->setInputHandler(InputPtr(i.release()));
    sc->setCodec(std::auto_ptr<sys::ConnectionCodec>(c));
    return sc.release();
}

    
}} // namespace qpid::broker
