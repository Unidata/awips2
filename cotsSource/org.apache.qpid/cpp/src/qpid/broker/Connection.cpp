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
#include "qpid/broker/Connection.h"
#include "qpid/broker/SessionState.h"
#include "qpid/broker/Bridge.h"
#include "qpid/broker/Broker.h"

#include "qpid/log/Statement.h"
#include "qpid/ptr_map.h"
#include "qpid/framing/AMQP_ClientProxy.h"
#include "qpid/framing/enum.h"
#include "qmf/org/apache/qpid/broker/EventClientConnect.h"
#include "qmf/org/apache/qpid/broker/EventClientDisconnect.h"

#include <boost/bind.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include <algorithm>
#include <iostream>
#include <assert.h>

using namespace qpid::sys;
using namespace qpid::framing;
using qpid::ptr_map_ptr;
using qpid::management::ManagementAgent;
using qpid::management::ManagementObject;
using qpid::management::Manageable;
using qpid::management::Args;
namespace _qmf = qmf::org::apache::qpid::broker;

namespace qpid {
namespace broker {

struct ConnectionTimeoutTask : public sys::TimerTask {
    sys::Timer& timer;
    Connection& connection;

    ConnectionTimeoutTask(uint16_t hb, sys::Timer& t, Connection& c) :
        TimerTask(Duration(hb*2*TIME_SEC)),
        timer(t),
        connection(c)
    {}

    void touch() {
        restart();
    }

    void fire() {
        // If we get here then we've not received any traffic in the timeout period
        // Schedule closing the connection for the io thread
        QPID_LOG(error, "Connection timed out: closing");
        connection.abort();
    }
};

Connection::Connection(ConnectionOutputHandler* out_, Broker& broker_, const std::string& mgmtId_, unsigned int ssf, bool isLink_, uint64_t objectId) :
    ConnectionState(out_, broker_),
    ssf(ssf),
    adapter(*this, isLink_),
    isLink(isLink_),
    mgmtClosing(false),
    mgmtId(mgmtId_),
    mgmtObject(0),
    links(broker_.getLinks()),
    agent(0),
    timer(broker_.getTimer()),
    errorListener(0),
    shadow(false)
{
    Manageable* parent = broker.GetVhostObject();

    if (isLink)
        links.notifyConnection(mgmtId, this);

    if (parent != 0)
    {
        agent = broker_.getManagementAgent();


        // TODO set last bool true if system connection
        if (agent != 0) {
            mgmtObject = new _qmf::Connection(agent, this, parent, mgmtId, !isLink, false);
            agent->addObject(mgmtObject, objectId, true);
        }
        ConnectionState::setUrl(mgmtId);
    }
    if (!isShadow()) broker.getConnectionCounter().inc_connectionCount();
}

void Connection::requestIOProcessing(boost::function0<void> callback)
{
    ScopedLock<Mutex> l(ioCallbackLock);
    ioCallbacks.push(callback);
    out.activateOutput();
}

Connection::~Connection()
{
    if (mgmtObject != 0) {
        mgmtObject->resourceDestroy();
        if (!isLink)
            agent->raiseEvent(_qmf::EventClientDisconnect(mgmtId, ConnectionState::getUserId()));
    }
    if (isLink)
        links.notifyClosed(mgmtId);

    if (heartbeatTimer)
        heartbeatTimer->cancel();
    if (timeoutTimer)
        timeoutTimer->cancel();

    if (!isShadow()) broker.getConnectionCounter().dec_connectionCount();
}

void Connection::received(framing::AMQFrame& frame) {
    // Received frame on connection so delay timeout
    restartTimeout();

    if (frame.getChannel() == 0 && frame.getMethod()) {
        adapter.handle(frame);
    } else {
        getChannel(frame.getChannel()).in(frame);
    }

    if (isLink)
        recordFromServer(frame);
    else
        recordFromClient(frame);
}

void Connection::recordFromServer(framing::AMQFrame& frame)
{
    if (mgmtObject != 0)
    {
        mgmtObject->inc_framesToClient();
        mgmtObject->inc_bytesToClient(frame.encodedSize());
    }
}

void Connection::recordFromClient(framing::AMQFrame& frame)
{
    if (mgmtObject != 0)
    {
        mgmtObject->inc_framesFromClient();
        mgmtObject->inc_bytesFromClient(frame.encodedSize());
    }
}

string Connection::getAuthMechanism()
{
    if (!isLink)
        return string("ANONYMOUS");

    return links.getAuthMechanism(mgmtId);
}

string Connection::getAuthCredentials()
{
    if (!isLink)
        return string();

    if (mgmtObject != 0)
    {
        if (links.getAuthMechanism(mgmtId) == "ANONYMOUS")
            mgmtObject->set_authIdentity("anonymous");
        else
            mgmtObject->set_authIdentity(links.getAuthIdentity(mgmtId));
    }

    return links.getAuthCredentials(mgmtId);
}

void Connection::notifyConnectionForced(const string& text)
{
    if (isLink)
        links.notifyConnectionForced(mgmtId, text);
}

void Connection::setUserId(const string& userId)
{
    ConnectionState::setUserId(userId);
    if (mgmtObject != 0) {
        mgmtObject->set_authIdentity(userId);
        agent->raiseEvent(_qmf::EventClientConnect(mgmtId, userId));
    }
}

void Connection::setFederationLink(bool b)
{
    ConnectionState::setFederationLink(b);
    if (mgmtObject != 0)
            mgmtObject->set_federationLink(b);
}

void Connection::close(connection::CloseCode code, const string& text)
{
    QPID_LOG_IF(error, code != connection::CLOSE_CODE_NORMAL, "Connection " << mgmtId << " closed by error: " << text << "(" << code << ")");
    if (heartbeatTimer)
        heartbeatTimer->cancel();
    if (timeoutTimer)
        timeoutTimer->cancel();
    adapter.close(code, text);
    //make sure we delete dangling pointers from outputTasks before deleting sessions
    outputTasks.removeAll();
    channels.clear();
    getOutput().close();
}

// Send a close to the client but keep the channels. Used by cluster.
void Connection::sendClose() {
    if (heartbeatTimer)
        heartbeatTimer->cancel();
    if (timeoutTimer)
        timeoutTimer->cancel();
    adapter.close(connection::CLOSE_CODE_NORMAL, "OK");
    getOutput().close();
}

void Connection::idleOut(){}

void Connection::idleIn(){}

void Connection::closed(){ // Physically closed, suspend open sessions.
    if (heartbeatTimer)
        heartbeatTimer->cancel();
    if (timeoutTimer)
        timeoutTimer->cancel();
    try {
        while (!channels.empty())
            ptr_map_ptr(channels.begin())->handleDetach();
        while (!exclusiveQueues.empty()) {
            Queue::shared_ptr q(exclusiveQueues.front());
            q->releaseExclusiveOwnership();
            if (q->canAutoDelete()) {
                Queue::tryAutoDelete(broker, q);
            }
            exclusiveQueues.erase(exclusiveQueues.begin());
        }
    } catch(std::exception& e) {
        QPID_LOG(error, QPID_MSG("While closing connection: " << e.what()));
        assert(0);
    }
}

bool Connection::hasOutput() { return outputTasks.hasOutput(); }

bool Connection::doOutput() {
    try {
        {
            ScopedLock<Mutex> l(ioCallbackLock);
            while (!ioCallbacks.empty()) {
                boost::function0<void> cb = ioCallbacks.front();
                ioCallbacks.pop();
                ScopedUnlock<Mutex> ul(ioCallbackLock);
                cb(); // Lend the IO thread for management processing
            }
        }
        if (mgmtClosing) {
            closed();
            close(connection::CLOSE_CODE_CONNECTION_FORCED, "Closed by Management Request");
        } else {
            //then do other output as needed:
            return outputTasks.doOutput();
	}
    }catch(ConnectionException& e){
        close(e.code, e.getMessage());
    }catch(std::exception& e){
        close(connection::CLOSE_CODE_CONNECTION_FORCED, e.what());
    }
    return false;
}

void Connection::sendHeartbeat() {
	adapter.heartbeat();
}

void Connection::closeChannel(uint16_t id) {
    ChannelMap::iterator i = channels.find(id);
    if (i != channels.end()) channels.erase(i);
}

SessionHandler& Connection::getChannel(ChannelId id) {
    ChannelMap::iterator i=channels.find(id);
    if (i == channels.end()) {
        i = channels.insert(id, new SessionHandler(*this, id)).first;
    }
    return *ptr_map_ptr(i);
}

ManagementObject* Connection::GetManagementObject(void) const
{
    return (ManagementObject*) mgmtObject;
}

Manageable::status_t Connection::ManagementMethod(uint32_t methodId, Args&, string&)
{
    Manageable::status_t status = Manageable::STATUS_UNKNOWN_METHOD;

    QPID_LOG(debug, "Connection::ManagementMethod [id=" << methodId << "]");

    switch (methodId)
    {
    case _qmf::Connection::METHOD_CLOSE :
        mgmtClosing = true;
        if (mgmtObject != 0) mgmtObject->set_closing(1);
        out.activateOutput();
        status = Manageable::STATUS_OK;
        break;
    }

    return status;
}

void Connection::setSecureConnection(SecureConnection* s)
{
    adapter.setSecureConnection(s);
}

struct ConnectionHeartbeatTask : public sys::TimerTask {
    sys::Timer& timer;
    Connection& connection;
    ConnectionHeartbeatTask(uint16_t hb, sys::Timer& t, Connection& c) :
        TimerTask(Duration(hb*TIME_SEC)),
        timer(t),
        connection(c)
    {}

    void fire() {
        // Setup next firing
        setupNextFire();
        timer.add(this);

        // Send Heartbeat
        connection.sendHeartbeat();
    }
};

void Connection::abort()
{
    // Make sure that we don't try to send a heartbeat as we're
    // aborting the connection
    if (heartbeatTimer)
        heartbeatTimer->cancel();

    out.abort();
}

void Connection::setHeartbeatInterval(uint16_t heartbeat)
{
    setHeartbeat(heartbeat);
    if (heartbeat > 0 && !isShadow()) {
        heartbeatTimer = new ConnectionHeartbeatTask(heartbeat, timer, *this);
        timer.add(heartbeatTimer);
        timeoutTimer = new ConnectionTimeoutTask(heartbeat, timer, *this);
        timer.add(timeoutTimer);
    }
}

void Connection::restartTimeout()
{
    if (timeoutTimer)
        timeoutTimer->touch();
}

}}
