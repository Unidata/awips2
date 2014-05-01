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
#include "qpid/client/ConnectionImpl.h"
#include "qpid/client/Connector.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/client/SessionImpl.h"

#include "qpid/log/Statement.h"
#include "qpid/Url.h"
#include "qpid/framing/enum.h"
#include "qpid/framing/reply_exceptions.h"

#include <boost/bind.hpp>
#include <boost/format.hpp>

#include <limits>

namespace qpid {
namespace client {

using namespace qpid::framing;
using namespace qpid::framing::connection;
using namespace qpid::sys;
using namespace qpid::framing::connection;//for connection error codes

// Get timer singleton  
Timer& theTimer() {
    static Mutex timerInitLock;
    ScopedLock<Mutex> l(timerInitLock);

    static qpid::sys::Timer t;
    return t;
}

class HeartbeatTask : public TimerTask {
    TimeoutHandler& timeout;

    void fire() {
        // If we ever get here then we have timed out
        QPID_LOG(debug, "Traffic timeout");
        timeout.idleIn();
    }

public:
    HeartbeatTask(Duration p, TimeoutHandler& t) :
        TimerTask(p),
        timeout(t)
    {}
};

ConnectionImpl::ConnectionImpl(framing::ProtocolVersion v, const ConnectionSettings& settings)
    : Bounds(settings.maxFrameSize * settings.bounds),
      handler(settings, v),
      version(v),
      nextChannel(1)
{
    QPID_LOG(debug, "ConnectionImpl created for " << version.toString());
    handler.in = boost::bind(&ConnectionImpl::incoming, this, _1);
    handler.out = boost::bind(&Connector::send, boost::ref(connector), _1);
    handler.onClose = boost::bind(&ConnectionImpl::closed, this,
                                  CLOSE_CODE_NORMAL, std::string());
    //only set error handler once  open
    handler.onError = boost::bind(&ConnectionImpl::closed, this, _1, _2);
    handler.getSSF = boost::bind(&Connector::getSSF, boost::ref(connector));
}

const uint16_t ConnectionImpl::NEXT_CHANNEL = std::numeric_limits<uint16_t>::max();

ConnectionImpl::~ConnectionImpl() {
    // Important to close the connector first, to ensure the
    // connector thread does not call on us while the destructor
    // is running.
    if (connector) connector->close();
}

void ConnectionImpl::addSession(const boost::shared_ptr<SessionImpl>& session, uint16_t channel)
{
    Mutex::ScopedLock l(lock);
    for (uint16_t i = 0; i < NEXT_CHANNEL; i++) { //will at most search through channels once
        uint16_t c = channel == NEXT_CHANNEL ? nextChannel++ : channel;
        boost::weak_ptr<SessionImpl>& s = sessions[c];
        boost::shared_ptr<SessionImpl> ss = s.lock();
        if (!ss) {
            //channel is free, we can assign it to this session
            session->setChannel(c);
            s = session;
            return;
        } else if (channel != NEXT_CHANNEL) {
            //channel is taken and was requested explicitly so don't look for another
            throw SessionBusyException(QPID_MSG("Channel " << ss->getChannel() << " attached to " << ss->getId()));
        } //else channel is busy, but we can keep looking for a free one
    }

}

void ConnectionImpl::handle(framing::AMQFrame& frame)
{
    handler.outgoing(frame);
}

void ConnectionImpl::incoming(framing::AMQFrame& frame)
{
    boost::shared_ptr<SessionImpl> s;
    {
        Mutex::ScopedLock l(lock);
        s = sessions[frame.getChannel()].lock();
    }
    if (!s) {
        QPID_LOG(info, "Dropping frame received on invalid channel: " << frame);
    } else {
        s->in(frame);
    }
}

bool ConnectionImpl::isOpen() const 
{ 
    return handler.isOpen();
}


void ConnectionImpl::open()
{
    const std::string& protocol = handler.protocol;
    const std::string& host = handler.host;
    int port = handler.port;
    QPID_LOG(info, "Connecting to " << protocol << ":" << host << ":" << port);

    connector.reset(Connector::create(protocol, version, handler, this)); 
    connector->setInputHandler(&handler);
    connector->setShutdownHandler(this);
    connector->connect(host, port);
    connector->init();
 
    // Enable heartbeat if requested
    uint16_t heartbeat = static_cast<ConnectionSettings&>(handler).heartbeat;
    if (heartbeat) {
        // Set connection timeout to be 2x heart beat interval and setup timer
        heartbeatTask = new HeartbeatTask(heartbeat * 2 * TIME_SEC, *this);
        handler.setRcvTimeoutTask(heartbeatTask);
        theTimer().add(heartbeatTask);
    }

    try {
        handler.waitForOpen();
    } catch (...) {
        // Make sure the connector thread is joined.
        connector->close();
        throw;
    }

    // If the SASL layer has provided an "operational" userId for the connection,
    // put it in the negotiated settings.
    const std::string& userId(handler.getUserId());
    if (!userId.empty())
        handler.username = userId;

    //enable security layer if one has been negotiated:
    std::auto_ptr<SecurityLayer> securityLayer = handler.getSecurityLayer();
    if (securityLayer.get()) {
        QPID_LOG(debug, "Activating security layer");
        connector->activateSecurityLayer(securityLayer);
    } else {
        QPID_LOG(debug, "No security layer in place");
    }
}

void ConnectionImpl::idleIn()
{
    connector->abort();
}

void ConnectionImpl::idleOut()
{
    AMQFrame frame((AMQHeartbeatBody()));
    connector->send(frame);
}

void ConnectionImpl::close()
{
    if (heartbeatTask) 
        heartbeatTask->cancel();
    // close() must be idempotent and no-throw as it will often be called in destructors.
    if (handler.isOpen()) {
        try {
            handler.close();
            closed(CLOSE_CODE_NORMAL, "Closed by client");
        } catch (...) {}
    }
    assert(!handler.isOpen());
}


template <class F> void ConnectionImpl::closeInternal(const F& f) {
    if (heartbeatTask) {
        heartbeatTask->cancel();
    }
    {
        Mutex::ScopedUnlock u(lock);
        connector->close();
    }
    //notifying sessions of failure can result in those session being
    //deleted which in turn results in a call to erase(); this can
    //even happen on this thread, when 's' goes out of scope
    //below. Using a copy prevents the map being modified as we
    //iterate through.
    SessionMap copy;
    sessions.swap(copy);
    for (SessionMap::iterator i = copy.begin(); i != copy.end(); ++i) {
        boost::shared_ptr<SessionImpl> s = i->second.lock();
        if (s) f(s);
    }
}

void ConnectionImpl::closed(uint16_t code, const std::string& text) { 
    Mutex::ScopedLock l(lock);
    setException(new ConnectionException(ConnectionHandler::convert(code), text));
    closeInternal(boost::bind(&SessionImpl::connectionClosed, _1, code, text));
}

static const std::string CONN_CLOSED("Connection closed");

void ConnectionImpl::shutdown() {
    if ( failureCallback )
      failureCallback();

    if (handler.isClosed()) return;

    // FIXME aconway 2008-06-06: exception use, amqp0-10 does not seem to have
    // an appropriate close-code. connection-forced is not right.
    bool isClosing = handler.isClosing();
    handler.fail(CONN_CLOSED);//ensure connection is marked as failed before notifying sessions
    Mutex::ScopedLock l(lock);
    if (!isClosing) 
        closeInternal(boost::bind(&SessionImpl::connectionBroke, _1, CONN_CLOSED));
    setException(new TransportFailure(CONN_CLOSED));
}

void ConnectionImpl::erase(uint16_t ch) {
    Mutex::ScopedLock l(lock);
    sessions.erase(ch);
}

const ConnectionSettings& ConnectionImpl::getNegotiatedSettings()
{
    return handler;
}

std::vector<qpid::Url> ConnectionImpl::getInitialBrokers() {
    return handler.knownBrokersUrls;
}

boost::shared_ptr<SessionImpl>  ConnectionImpl::newSession(const std::string& name, uint32_t timeout, uint16_t channel) {
    boost::shared_ptr<SessionImpl> simpl(new SessionImpl(name, shared_from_this()));
    addSession(simpl, channel);
    simpl->open(timeout);
    return simpl;
}

}} // namespace qpid::client
