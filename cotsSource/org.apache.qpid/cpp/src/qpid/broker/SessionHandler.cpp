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

#include "qpid/broker/SessionHandler.h"
#include "qpid/broker/SessionState.h"
#include "qpid/broker/Connection.h"
#include "qpid/log/Statement.h"

#include <boost/bind.hpp>

namespace qpid {
namespace broker {
using namespace framing;
using namespace std;
using namespace qpid::sys;

SessionHandler::SessionHandler(Connection& c, ChannelId ch)
    : amqp_0_10::SessionHandler(&c.getOutput(), ch),
      connection(c), 
      proxy(out),
      clusterOrderProxy(c.getClusterOrderOutput() ? new SetChannelProxy(ch, c.getClusterOrderOutput()) : 0)
{}

SessionHandler::~SessionHandler() {}

namespace {
ClassId classId(AMQMethodBody* m) { return m ? m->amqpMethodId() : 0; }
MethodId methodId(AMQMethodBody* m) { return m ? m->amqpClassId() : 0; }
} // namespace

void SessionHandler::connectionException(framing::connection::CloseCode code, const std::string& msg) {
    // NOTE: must tell the error listener _before_ calling connection.close()
    if (connection.getErrorListener()) connection.getErrorListener()->connectionError(msg);
    connection.close(code, msg);
}

void SessionHandler::channelException(framing::session::DetachCode, const std::string& msg) {
    if (connection.getErrorListener()) connection.getErrorListener()->sessionError(getChannel(), msg);
}

void SessionHandler::executionException(framing::execution::ErrorCode, const std::string& msg) {
    if (connection.getErrorListener()) connection.getErrorListener()->sessionError(getChannel(), msg);
}

ConnectionState& SessionHandler::getConnection() { return connection; }

const ConnectionState& SessionHandler::getConnection() const { return connection; }

void SessionHandler::handleDetach() {
    amqp_0_10::SessionHandler::handleDetach();
    assert(&connection.getChannel(channel.get()) == this);
    if (session.get())
        connection.getBroker().getSessionManager().detach(session);
    assert(!session.get());
    connection.closeChannel(channel.get()); 
}

void SessionHandler::setState(const std::string& name, bool force) {
    assert(!session.get());
    SessionId id(connection.getUserId(), name);
    session = connection.broker.getSessionManager().attach(*this, id, force);
}

void SessionHandler::detaching() 
{
    assert(session.get());
    session->disableOutput();
}

FrameHandler* SessionHandler::getInHandler() { return session.get() ? &session->in : 0; }
qpid::SessionState* SessionHandler::getState() { return session.get(); }

void SessionHandler::readyToSend() {
    if (session.get()) session->readyToSend();
}

/**
 * Used by inter-broker bridges to set up session id and attach
 */
void SessionHandler::attachAs(const std::string& name)
{
    SessionId id(connection.getUserId(), name);
    SessionState::Configuration config = connection.broker.getSessionManager().getSessionConfig();
    session.reset(new SessionState(connection.getBroker(), *this, id, config));
    sendAttach(false);
}

/**
 * TODO: this is a little ugly, fix it; its currently still relied on
 * for 'push' bridges
 */
void SessionHandler::attached(const std::string& name)
{
    if (session.get()) {
        amqp_0_10::SessionHandler::attached(name);
    } else {
        SessionId id(connection.getUserId(), name);
        SessionState::Configuration config = connection.broker.getSessionManager().getSessionConfig();
        session.reset(new SessionState(connection.getBroker(), *this, id, config));
        markReadyToSend();
    }
}
    
}} // namespace qpid::broker
