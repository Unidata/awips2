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
#include "qpid/cluster/RetractClient.h"
#include "qpid/cluster/UpdateClient.h"
#include "qpid/framing/ClusterConnectionRetractOfferBody.h"
#include "qpid/client/ConnectionAccess.h"
#include "qpid/client/ConnectionImpl.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace cluster {

using namespace framing;

namespace {

struct AutoClose {
    client::Connection& connection;
    AutoClose(client::Connection& c) : connection(c) {}
    ~AutoClose() { connection.close(); }
};
}

RetractClient::RetractClient(const Url& u, const client::ConnectionSettings& cs)
    : url(u), connectionSettings(cs)
{}

RetractClient::~RetractClient() { delete this; }


void RetractClient::run() {
    try {
        client::Connection c = UpdateClient::catchUpConnection();
        c.open(url, connectionSettings);
        AutoClose ac(c);
        AMQFrame retract((ClusterConnectionRetractOfferBody()));
        client::ConnectionAccess::getImpl(c)->handle(retract);
    } catch (const std::exception& e) {
        QPID_LOG(error, " while retracting retract to " << url << ": " << e.what()); 
    }
}

}} // namespace qpid::cluster
