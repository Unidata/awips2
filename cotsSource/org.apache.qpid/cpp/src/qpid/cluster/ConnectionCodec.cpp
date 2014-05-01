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
#include "qpid/cluster/ConnectionCodec.h"
#include "qpid/cluster/Connection.h"
#include "qpid/cluster/Cluster.h"
#include "qpid/cluster/ProxyInputHandler.h"
#include "qpid/broker/Connection.h"
#include "qpid/framing/ConnectionCloseBody.h"
#include "qpid/framing/ConnectionCloseOkBody.h"
#include "qpid/log/Statement.h"
#include "qpid/memory.h"
#include <stdexcept>
#include <boost/utility/in_place_factory.hpp>

namespace qpid {
namespace cluster {

using namespace framing;

sys::ConnectionCodec*
ConnectionCodec::Factory::create(ProtocolVersion v, sys::OutputControl& out, const std::string& id,
                                 unsigned int ssf) {
    if (v == ProtocolVersion(0, 10))
        return new ConnectionCodec(v, out, id, cluster, false, false, ssf);
    else if (v == ProtocolVersion(0x80 + 0, 0x80 + 10)) // Catch-up connection
        return new ConnectionCodec(v, out, id, cluster, true, false, ssf); 
    return 0;
}

// Used for outgoing Link connections
sys::ConnectionCodec*
ConnectionCodec::Factory::create(sys::OutputControl& out, const std::string& logId,
                                 unsigned int ssf) {
    return new ConnectionCodec(ProtocolVersion(0,10), out, logId, cluster, false, true, ssf);
}

ConnectionCodec::ConnectionCodec(
    const ProtocolVersion& v, sys::OutputControl& out,
    const std::string& logId, Cluster& cluster, bool catchUp, bool isLink, unsigned int ssf
) : codec(out, logId, isLink),
    interceptor(new Connection(cluster, codec, logId, cluster.getId(), catchUp, isLink, ssf))
{
    std::auto_ptr<sys::ConnectionInputHandler> ih(new ProxyInputHandler(interceptor));
    codec.setInputHandler(ih);
    codec.setVersion(v);
}

ConnectionCodec::~ConnectionCodec() {}

size_t ConnectionCodec::decode(const char* buffer, size_t size) {
    return interceptor->decode(buffer, size);
}

bool ConnectionCodec::isClosed() const { return codec.isClosed(); }

size_t ConnectionCodec::encode(const char* buffer, size_t size) { return codec.encode(buffer, size); }

bool ConnectionCodec::canEncode() { return codec.canEncode(); }

void ConnectionCodec::closed() { codec.closed(); }

ProtocolVersion ConnectionCodec::getVersion() const { return codec.getVersion(); }

}} // namespace qpid::cluster
