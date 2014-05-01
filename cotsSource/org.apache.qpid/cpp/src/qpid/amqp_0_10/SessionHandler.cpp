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


#include "qpid/amqp_0_10/SessionHandler.h"
#include "qpid/SessionState.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/AllInvoker.h"
#include "qpid/framing/enum.h"
#include "qpid/log/Statement.h"


#include <boost/bind.hpp>

namespace qpid {
namespace amqp_0_10 {
using namespace framing;
using namespace std;

void SessionHandler::checkAttached() {
    if (!getState()) 
        throw NotAttachedException(QPID_MSG("Channel " << channel.get() << " is not attached"));
}

SessionHandler::SessionHandler(FrameHandler* out, ChannelId ch)
    : channel(ch, out), peer(channel),
      awaitingDetached(false),
      sendReady(), receiveReady() {}

SessionHandler::~SessionHandler() {}

namespace {
bool isSessionControl(AMQMethodBody* m) {
    return m && m->amqpClassId() == SESSION_CLASS_ID;
}

session::DetachCode convert(uint8_t code) {
    switch(code) {
      case 0: return session::DETACH_CODE_NORMAL;
      case 1: return session::DETACH_CODE_SESSION_BUSY;
      case 2: return session::DETACH_CODE_TRANSPORT_BUSY;
      case 3: return session::DETACH_CODE_NOT_ATTACHED;
      case 4: default: return session::DETACH_CODE_UNKNOWN_IDS;
    }
}

} // namespace

void SessionHandler::invoke(const AMQMethodBody& m) {
    framing::invoke(*this, m);    
}

void SessionHandler::handleIn(AMQFrame& f) {
    // Note on channel states: a channel is attached if session != 0
    AMQMethodBody* m = f.getBody()->getMethod();
    try {
        // Ignore all but detach controls while awaiting detach
        if (awaitingDetached) {
            if (!isSessionControl(m)) return;
            if (m->amqpMethodId() != SESSION_DETACH_METHOD_ID &&
                m->amqpMethodId() != SESSION_DETACHED_METHOD_ID)
                return;
        }
        if (isSessionControl(m)) {
            invoke(*m);
        }
        else {
            // Drop frames if we are detached.
            if (!getState()) return;  
            if (!receiveReady)
                throw IllegalStateException(QPID_MSG(getState()->getId() << ": Not ready to receive data"));
            if (!getState()->receiverRecord(f))
                return; // Ignore duplicates.
            if (getState()->receiverNeedKnownCompleted())
                sendCompletion();
            getInHandler()->handle(f);
        }
    }
    catch(const SessionException& e) {
        QPID_LOG(error, "Execution exception: " << e.what());
        executionException(e.code, e.what()); // Let subclass handle this first.
        framing::AMQP_AllProxy::Execution  execution(channel);
        AMQMethodBody* m = f.getMethod();
        SequenceNumber commandId;
        if (getState()) commandId =  getState()->receiverGetCurrent();
        execution.exception(e.code, commandId, m ? m->amqpClassId() : 0, m ? m->amqpMethodId() : 0, 0, e.what(), FieldTable());
        detaching();
        sendDetach();
    }
    catch(const ChannelException& e){
        QPID_LOG(error, "Channel exception: " << e.what());
        channelException(e.code, e.what()); // Let subclass handle this first.
        peer.detached(name, e.code);
    }
    catch(const ConnectionException& e) {
        QPID_LOG(error, "Connection exception: " << e.what());
        connectionException(e.code, e.getMessage());
    }
    catch(const std::exception& e) {
        QPID_LOG(error, "Unexpected exception: " << e.what());
        connectionException(connection::CLOSE_CODE_FRAMING_ERROR, e.what());
    }
}

namespace {
bool isControl(const AMQFrame& f) {
    return f.getMethod() && f.getMethod()->type() == framing::SEGMENT_TYPE_CONTROL;
}
bool isCommand(const AMQFrame& f) {
    return f.getMethod() && f.getMethod()->type() == framing::SEGMENT_TYPE_COMMAND;
}
} // namespace

void SessionHandler::handleOut(AMQFrame& f) {
    checkAttached();
    if (!sendReady)
        throw IllegalStateException(QPID_MSG(getState()->getId() << ": Not ready to send data"));
    getState()->senderRecord(f); 
    if (isCommand(f) && getState()->senderNeedFlush()) {
        peer.flush(false, false, true); 
        getState()->senderRecordFlush();
    }
    channel.handle(f);
}

void SessionHandler::attach(const std::string& name_, bool force) {
    // Save the name for possible session-busy exception. Session-busy
    // can be thrown before we have attached the handler to a valid
    // SessionState, and in that case we need the name to send peer.detached
    name = name_;
    if (getState() && name == getState()->getId().getName())
        return;                 // Idempotent
    if (getState())
            throw TransportBusyException(
            QPID_MSG("Channel " << channel.get() << " already attached to " << getState()->getId()));
    setState(name, force);
    QPID_LOG(debug, "Attached channel " << channel.get() << " to " << getState()->getId());
    peer.attached(name);
    if (getState()->hasState())
        peer.flush(true, true, true);
    else
        sendCommandPoint(getState()->senderGetCommandPoint());
}

#define CHECK_NAME(NAME, MSG) do {                                      \
    checkAttached();                                                    \
    if (NAME != getState()->getId().getName())                          \
        throw InvalidArgumentException(                                 \
            QPID_MSG(MSG << ": incorrect session name: " << NAME        \
                     << ", expecting: " << getState()->getId().getName())); \
    } while(0)


void SessionHandler::attached(const std::string& name) {
    CHECK_NAME(name, "session.attached");
}

void SessionHandler::detach(const std::string& name) {
    CHECK_NAME(name, "session.detach");
    peer.detached(name, session::DETACH_CODE_NORMAL);
    handleDetach();
}

void SessionHandler::detached(const std::string& name, uint8_t code) {
    CHECK_NAME(name, "session.detached");
    awaitingDetached = false;
    if (code != session::DETACH_CODE_NORMAL)
        channelException(convert(code), "session.detached from peer.");
    else {
        handleDetach();
    }
}

void SessionHandler::handleDetach() {
    sendReady = receiveReady = false;
}

void SessionHandler::requestTimeout(uint32_t t) {
    checkAttached();
    getState()->setTimeout(t);
    peer.timeout(t);
}

void SessionHandler::timeout(uint32_t t) {
    checkAttached();
    getState()->setTimeout(t);
}

void SessionHandler::commandPoint(const SequenceNumber& id, uint64_t offset) {
    checkAttached();
    getState()->receiverSetCommandPoint(SessionPoint(id, offset));
    if (!receiveReady) {
        receiveReady = true;
        readyToReceive();
    }
}

void SessionHandler::expected(const SequenceSet& commands, const Array& /*fragments*/) {
    checkAttached();
    if (getState()->hasState()) { // Replay
        if (commands.empty()) throw IllegalStateException(
            QPID_MSG(getState()->getId() << ": has state but client is attaching as new session."));        
        // TODO aconway 2008-05-12: support replay of partial commands.
        // Here we always round down to the last command boundary.
        SessionPoint expectedPoint = commands.empty() ? SequenceNumber(0) : SessionPoint(commands.front(),0);
        SessionState::ReplayRange replay = getState()->senderExpected(expectedPoint);
        sendCommandPoint(expectedPoint);
        std::for_each(replay.begin(), replay.end(), out); // replay
    }
    else
        sendCommandPoint(getState()->senderGetCommandPoint());
}

void SessionHandler::confirmed(const SequenceSet& commands, const Array& /*fragments*/) {
    checkAttached();
    // Ignore non-contiguous confirmations.
    if (!commands.empty() && commands.front() >= getState()->senderGetReplayPoint()) 
        getState()->senderConfirmed(commands.rangesBegin()->last());
}

void SessionHandler::completed(const SequenceSet& commands, bool timelyReply) {
    checkAttached();
    getState()->senderCompleted(commands);
    if (getState()->senderNeedKnownCompleted() || timelyReply) {
        peer.knownCompleted(commands);
        getState()->senderRecordKnownCompleted();
    }
}

void SessionHandler::knownCompleted(const SequenceSet& commands) {
    checkAttached();
    getState()->receiverKnownCompleted(commands);
}

void SessionHandler::flush(bool expected, bool confirmed, bool completed) {
    checkAttached();
    if (expected)  {
        SequenceSet expectSet;
        if (getState()->hasState())
            expectSet.add(getState()->receiverGetExpected().command);
        peer.expected(expectSet, Array());
    }
    if (confirmed) {
        SequenceSet confirmSet;
        if (!getState()->receiverGetUnknownComplete().empty()) 
            confirmSet.add(getState()->receiverGetUnknownComplete().front(),
                           getState()->receiverGetReceived().command);
        peer.confirmed(confirmSet, Array());
    }
    if (completed)
        peer.completed(getState()->receiverGetUnknownComplete(), true);
}

void SessionHandler::gap(const SequenceSet& /*commands*/) {
    throw NotImplementedException("session.gap not supported");
}

void SessionHandler::sendDetach()
{
    checkAttached();
    awaitingDetached = true;
    peer.detach(getState()->getId().getName());
}

void SessionHandler::sendCompletion() {
    checkAttached();
    const SequenceSet& c = getState()->receiverGetUnknownComplete();
    peer.completed(c, getState()->receiverNeedKnownCompleted());
}

void SessionHandler::sendAttach(bool force) {
    QPID_LOG(debug, "SessionHandler::sendAttach attach id=" << getState()->getId());
    peer.attach(getState()->getId().getName(), force);
    if (getState()->hasState())
        peer.flush(true, true, true);
    else
        sendCommandPoint(getState()->senderGetCommandPoint());
}

void SessionHandler::sendCommandPoint(const SessionPoint& point)  {
    peer.commandPoint(point.command, point.offset);
    if (!sendReady) {
        sendReady = true;
        readyToSend();
    }
}

void SessionHandler::markReadyToSend() {
    if (!sendReady) {
        sendReady = true;
    }
}

void SessionHandler::sendTimeout(uint32_t t) {
    checkAttached();
    peer.requestTimeout(t);
}

void SessionHandler::sendFlush() {
    peer.flush(false, true, true);
}

bool SessionHandler::ready() const {
    return  sendReady && receiveReady;
}


}} // namespace qpid::broker
