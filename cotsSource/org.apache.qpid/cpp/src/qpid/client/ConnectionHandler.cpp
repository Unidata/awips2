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

#include "qpid/client/ConnectionHandler.h"

#include "qpid/client/SaslFactory.h"
#include "qpid/framing/amqp_framing.h"
#include "qpid/framing/all_method_bodies.h"
#include "qpid/framing/ClientInvoker.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/log/Helpers.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/SystemInfo.h"

using namespace qpid::client;
using namespace qpid::framing;
using namespace qpid::framing::connection;
using qpid::sys::SecurityLayer;
using qpid::sys::Duration;
using qpid::sys::TimerTask;
using qpid::sys::Timer;
using qpid::sys::AbsTime;
using qpid::sys::TIME_SEC;
using qpid::sys::ScopedLock;
using qpid::sys::Mutex;

namespace {
const std::string OK("OK");
const std::string PLAIN("PLAIN");
const std::string en_US("en_US");

const std::string INVALID_STATE_START("start received in invalid state");
const std::string INVALID_STATE_TUNE("tune received in invalid state");
const std::string INVALID_STATE_OPEN_OK("open-ok received in invalid state");
const std::string INVALID_STATE_CLOSE_OK("close-ok received in invalid state");

const std::string SESSION_FLOW_CONTROL("qpid.session_flow");
const std::string CLIENT_PROCESS_NAME("qpid.client_process");
const std::string CLIENT_PID("qpid.client_pid");
const std::string CLIENT_PPID("qpid.client_ppid");
const int SESSION_FLOW_CONTROL_VER = 1;
}

CloseCode ConnectionHandler::convert(uint16_t replyCode)
{
    switch (replyCode) {
      case 200: return CLOSE_CODE_NORMAL;
      case 320: return CLOSE_CODE_CONNECTION_FORCED;
      case 402: return CLOSE_CODE_INVALID_PATH;
      case 501: default:
        return CLOSE_CODE_FRAMING_ERROR;
    }
}

ConnectionHandler::ConnectionHandler(const ConnectionSettings& s, ProtocolVersion& v)
    : StateManager(NOT_STARTED), ConnectionSettings(s), outHandler(*this), proxy(outHandler),
      errorCode(CLOSE_CODE_NORMAL), version(v)
{
    insist = true;

    ESTABLISHED.insert(FAILED);
    ESTABLISHED.insert(CLOSED);
    ESTABLISHED.insert(OPEN);

    FINISHED.insert(FAILED);
    FINISHED.insert(CLOSED);

    properties.setInt(SESSION_FLOW_CONTROL, SESSION_FLOW_CONTROL_VER);
    properties.setString(CLIENT_PROCESS_NAME, sys::SystemInfo::getProcessName());
    properties.setInt(CLIENT_PID, sys::SystemInfo::getProcessId());
    properties.setInt(CLIENT_PPID, sys::SystemInfo::getParentProcessId());
}

void ConnectionHandler::incoming(AMQFrame& frame)
{
    if (getState() == CLOSED) {
        throw Exception("Received frame on closed connection");
    }

    if (rcvTimeoutTask) {
        // Received frame on connection so delay timeout
        rcvTimeoutTask->restart();
    }

    AMQBody* body = frame.getBody();
    try {
        if (frame.getChannel() != 0 || !invoke(static_cast<ConnectionOperations&>(*this), *body)) {
            switch(getState()) {
            case OPEN:
                in(frame);
                break;
            case CLOSING:
                QPID_LOG(warning, "Ignoring frame while closing connection: " << frame);
                break;
            default:
                throw Exception("Cannot receive frames on non-zero channel until connection is established.");
            }
        }
    }catch(std::exception& e){
        QPID_LOG(warning, "Closing connection due to " << e.what());
        setState(CLOSING);
        errorCode = CLOSE_CODE_FRAMING_ERROR;
        errorText = e.what();
        proxy.close(501, e.what());
    }
}

void ConnectionHandler::outgoing(AMQFrame& frame)
{
    if (getState() == OPEN)
        out(frame);
    else
        throw TransportFailure(errorText.empty() ? "Connection is not open." : errorText);
}

void ConnectionHandler::waitForOpen()
{
    waitFor(ESTABLISHED);
    if (getState() == FAILED || getState() == CLOSED) {
        throw ConnectionException(errorCode, errorText);
    }
}

void ConnectionHandler::close()
{
    switch (getState()) {
      case NEGOTIATING:
      case OPENING:
        fail("Connection closed before it was established");
        break;
      case OPEN:
        if (setState(CLOSING, OPEN)) {
            proxy.close(200, OK);
            waitFor(FINISHED);//FINISHED = CLOSED or FAILED
        }
        //else, state was changed from open after we checked, can only
        //change to failed or closed, so nothing to do
        break;

        // Nothing to do if already CLOSING, CLOSED, FAILED or if NOT_STARTED
    }
}

void ConnectionHandler::heartbeat()
{
    // Do nothing - the purpose of heartbeats is just to make sure that there is some
    // traffic on the connection within the heart beat interval, we check for the
    // traffic and don't need to do anything in response to heartbeats

    // Although the above is still true we're now using a received heartbeat as a trigger
    // to send out our own heartbeat
    proxy.heartbeat();
}

void ConnectionHandler::checkState(STATES s, const std::string& msg)
{
    if (getState() != s) {
        throw CommandInvalidException(msg);
    }
}

void ConnectionHandler::fail(const std::string& message)
{
    errorCode = CLOSE_CODE_FRAMING_ERROR;
    errorText = message;
    QPID_LOG(warning, message);
    setState(FAILED);
}

namespace {
std::string SPACE(" ");
}

void ConnectionHandler::start(const FieldTable& /*serverProps*/, const Array& mechanisms, const Array& /*locales*/)
{
    checkState(NOT_STARTED, INVALID_STATE_START);
    setState(NEGOTIATING);
    sasl = SaslFactory::getInstance().create(*this);

    std::string mechlist;
    bool chosenMechanismSupported = mechanism.empty();
    for (Array::const_iterator i = mechanisms.begin(); i != mechanisms.end(); ++i) {
        if (!mechanism.empty() && mechanism == (*i)->get<std::string>()) {
            chosenMechanismSupported = true;
            mechlist = (*i)->get<std::string>() + SPACE + mechlist;
        } else {
            if (i != mechanisms.begin()) mechlist += SPACE;
            mechlist += (*i)->get<std::string>();
        }
    }

    if (!chosenMechanismSupported) {
        fail("Selected mechanism not supported: " + mechanism);
    }

    if (sasl.get()) {
        string response = sasl->start(mechanism.empty() ? mechlist : mechanism,
                                      getSSF ? getSSF() : 0);
        proxy.startOk(properties, sasl->getMechanism(), response, locale);
    } else {
        //TODO: verify that desired mechanism and locale are supported
        string response = ((char)0) + username + ((char)0) + password;
        proxy.startOk(properties, mechanism, response, locale);
    }
}

void ConnectionHandler::secure(const std::string& challenge)
{
    if (sasl.get()) {
        string response = sasl->step(challenge);
        proxy.secureOk(response);
    } else {
        throw NotImplementedException("Challenge-response cycle not yet implemented in client");
    }
}

void ConnectionHandler::tune(uint16_t maxChannelsProposed, uint16_t maxFrameSizeProposed,
                             uint16_t heartbeatMin, uint16_t heartbeatMax)
{
    checkState(NEGOTIATING, INVALID_STATE_TUNE);
    maxChannels = std::min(maxChannels, maxChannelsProposed);
    maxFrameSize = std::min(maxFrameSize, maxFrameSizeProposed);
    // Clip the requested heartbeat to the maximum/minimum offered
    uint16_t heartbeat = ConnectionSettings::heartbeat;
    heartbeat = heartbeat < heartbeatMin ? heartbeatMin :
                heartbeat > heartbeatMax ? heartbeatMax :
                heartbeat;
    ConnectionSettings::heartbeat = heartbeat;
    proxy.tuneOk(maxChannels, maxFrameSize, heartbeat);
    setState(OPENING);
    proxy.open(virtualhost, capabilities, insist);
}

void ConnectionHandler::openOk ( const Array& knownBrokers )
{
    checkState(OPENING, INVALID_STATE_OPEN_OK);
    knownBrokersUrls.clear();
    framing::Array::ValueVector::const_iterator i;
    for ( i = knownBrokers.begin(); i != knownBrokers.end(); ++i )
        knownBrokersUrls.push_back(Url((*i)->get<std::string>()));
    if (sasl.get()) {
        securityLayer = sasl->getSecurityLayer(maxFrameSize);
        operUserId = sasl->getUserId();
    }
    setState(OPEN);
    QPID_LOG(debug, "Known-brokers for connection: " << log::formatList(knownBrokersUrls));
}


void ConnectionHandler::redirect(const std::string& /*host*/, const Array& /*knownHosts*/)
{
    throw NotImplementedException("Redirection received from broker; not yet implemented in client");
}

void ConnectionHandler::close(uint16_t replyCode, const std::string& replyText)
{
    proxy.closeOk();
    errorCode = convert(replyCode);
    errorText = replyText;
    setState(CLOSED);
    QPID_LOG(warning, "Broker closed connection: " << replyCode << ", " << replyText);
    if (onError) {
        onError(replyCode, replyText);
    }
}

void ConnectionHandler::closeOk()
{
    checkState(CLOSING, INVALID_STATE_CLOSE_OK);
    if (onError && errorCode != CLOSE_CODE_NORMAL) {
        onError(errorCode, errorText);
    } else if (onClose) {
        onClose();
    }
    setState(CLOSED);
}

bool ConnectionHandler::isOpen() const
{
    return getState() == OPEN;
}

bool ConnectionHandler::isClosed() const
{
    int s = getState();
    return s == CLOSED || s == FAILED;
}

bool ConnectionHandler::isClosing() const { return getState() == CLOSING; }

std::auto_ptr<qpid::sys::SecurityLayer> ConnectionHandler::getSecurityLayer()
{
    return securityLayer;
}

void ConnectionHandler::setRcvTimeoutTask(boost::intrusive_ptr<qpid::sys::TimerTask> t)
{
    rcvTimeoutTask = t;
}
