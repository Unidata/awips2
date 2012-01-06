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
#include "qpid/broker/ConnectionFactory.h"
#include "qpid/framing/ProtocolVersion.h"
#include "qpid/amqp_0_10/Connection.h"
#include "qpid/broker/Connection.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace broker {

using framing::ProtocolVersion;
typedef std::auto_ptr<amqp_0_10::Connection> ConnectionPtr;
typedef std::auto_ptr<sys::ConnectionInputHandler> InputPtr;

ConnectionFactory::ConnectionFactory(Broker& b) : broker(b) {}

ConnectionFactory::~ConnectionFactory() {}

sys::ConnectionCodec*
ConnectionFactory::create(ProtocolVersion v, sys::OutputControl& out, const std::string& id,
                          unsigned int ) {
    if (broker.getConnectionCounter().allowConnection())
    {
        QPID_LOG(error, "Client max connection count limit exceeded: " << broker.getOptions().maxConnections << " connection refused");
        return 0;
    }
    if (v == ProtocolVersion(0, 10)) {
        ConnectionPtr c(new amqp_0_10::Connection(out, id, false));
        c->setInputHandler(InputPtr(new broker::Connection(c.get(), broker, id, false)));
        return c.release();
    }
    return 0;
}

sys::ConnectionCodec*
ConnectionFactory::create(sys::OutputControl& out, const std::string& id,
                          unsigned int) {
    // used to create connections from one broker to another
    ConnectionPtr c(new amqp_0_10::Connection(out, id, true));
    c->setInputHandler(InputPtr(new broker::Connection(c.get(), broker, id, true)));
    return c.release();
}

    
}} // namespace qpid::broker
