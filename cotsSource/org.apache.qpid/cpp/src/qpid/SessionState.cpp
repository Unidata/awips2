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

#include "qpid/SessionState.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/enum.h"
#include "qpid/log/Statement.h"
#include <boost/bind.hpp>
#include <numeric>

namespace qpid {
using framing::AMQFrame;
using framing::NotImplementedException;
using framing::InvalidArgumentException;
using framing::IllegalStateException;
using framing::ResourceLimitExceededException;
using framing::InternalErrorException;
using framing::FramingErrorException;

namespace {
bool isControl(const AMQFrame& f) {
    return f.getMethod() && f.getMethod()->type() == framing::SEGMENT_TYPE_CONTROL;
}
bool isCommand(const AMQFrame& f) {
    return f.getMethod() && f.getMethod()->type() == framing::SEGMENT_TYPE_COMMAND;
}
} // namespace

SessionPoint::SessionPoint(SequenceNumber c, uint64_t o) : command(c), offset(o) {}

// TODO aconway 2008-05-22: Do complete frame sequence validity check here,
// currently duplicated betwen broker and client session impl.
//
void SessionPoint::advance(const AMQFrame& f) {
    if (isControl(f)) return;   // Ignore controls.
    if (f.isFirstSegment() && f.isFirstFrame()) {
        if (offset != 0)
            throw FramingErrorException(QPID_MSG("Unexpected command start frame."));
        if (!isCommand(f))
            throw FramingErrorException(
                QPID_MSG("Command start frame has invalid type" << f.getBody()->type()));
        if (f.isLastSegment() && f.isLastFrame()) 
            ++command;          // Single-frame command.
        else
            offset += f.encodedSize();
    }
    else {                      // continuation frame for partial command
        if (offset == 0)
            throw FramingErrorException(QPID_MSG("Unexpected command continuation frame."));
        if (f.isLastSegment() && f.isLastFrame()) {
            ++command;
            offset = 0;
        }
        else {
            // TODO aconway 2008-04-24: if we go to support for partial
            // command replay, then it may be better to record the unframed
            // data size in a command point rather than the framed size so
            // that the relationship of fragment offsets to the replay
            // list can be computed more easily.
            // 
            offset += f.encodedSize();
        }
    }
}

bool SessionPoint::operator<(const SessionPoint& x) const {
    return command < x.command || (command == x.command && offset < x.offset);
}

bool SessionPoint::operator==(const SessionPoint& x) const {
    return command == x.command && offset == x.offset;
}


SessionState::SendState::SendState() : unflushedSize(), replaySize(), bytesSinceKnownCompleted() {}

SessionState::ReceiveState::ReceiveState() : bytesSinceKnownCompleted() {}

SessionPoint SessionState::senderGetCommandPoint() { return sender.sendPoint; }
SequenceSet  SessionState::senderGetIncomplete() const { return sender.incomplete; }
SessionPoint SessionState::senderGetReplayPoint() const { return sender.replayPoint; }

SessionState::ReplayRange SessionState::senderExpected(const SessionPoint& expect) {
    if (expect < sender.replayPoint || sender.sendPoint < expect)
        throw InvalidArgumentException(QPID_MSG(getId() << ": expected command-point out of range."));
    QPID_LOG(debug, getId() << ": sender expected point moved to " << expect);
    ReplayList::iterator i = sender.replayList.begin();
    SessionPoint p = sender.replayPoint;
    while (i != sender.replayList.end() && p.command < expect.command)
        p.advance(*i++);
    assert(p.command == expect.command);
    return boost::make_iterator_range(i, sender.replayList.end());
}

void SessionState::senderRecord(const AMQFrame& f) {
    if (isControl(f)) return;   // Ignore control frames.
    QPID_LOG(trace, getId() << ": sent cmd " << sender.sendPoint.command << ": " << *f.getBody());

    stateful = true;
    if (timeout) sender.replayList.push_back(f);
    sender.unflushedSize += f.encodedSize();
    sender.bytesSinceKnownCompleted += f.encodedSize();
    sender.replaySize += f.encodedSize();
    sender.incomplete += sender.sendPoint.command;
    sender.sendPoint.advance(f);
    if (config.replayHardLimit && config.replayHardLimit < sender.replaySize) 
        throw ResourceLimitExceededException("Replay buffer exceeeded hard limit");
}

static const uint32_t SPONTANEOUS_REQUEST_INTERVAL = 65536; 

bool SessionState::senderNeedFlush() const {
    return (sender.sendPoint.command % SPONTANEOUS_REQUEST_INTERVAL == 0) ||
        (config.replayFlushLimit && sender.unflushedSize >= config.replayFlushLimit);
}

void SessionState::senderRecordFlush() {
    sender.flushPoint = sender.sendPoint;
    sender.unflushedSize = 0;
}

bool SessionState::senderNeedKnownCompleted() const {
    return config.replayFlushLimit && sender.bytesSinceKnownCompleted >= config.replayFlushLimit;
}

void SessionState::senderRecordKnownCompleted() {
    sender.bytesSinceKnownCompleted = 0;
}

void SessionState::senderConfirmed(const SessionPoint& confirmed) {
    if (confirmed > sender.sendPoint)
        throw InvalidArgumentException(QPID_MSG(getId() << ": confirmed < " << confirmed << " but only sent < " << sender.sendPoint));
    QPID_LOG(debug, getId() << ": sender confirmed point moved to " << confirmed);
    ReplayList::iterator i = sender.replayList.begin();
    while (i != sender.replayList.end() && sender.replayPoint.command < confirmed.command) {
        sender.replayPoint.advance(*i);
        assert(sender.replayPoint <= sender.sendPoint);
        sender.replaySize -= i->encodedSize();
        if (sender.replayPoint > sender.flushPoint) 
            sender.unflushedSize -= i->encodedSize();
        ++i;
    }
    if (sender.replayPoint > sender.flushPoint)
        sender.flushPoint = sender.replayPoint;
    sender.replayList.erase(sender.replayList.begin(), i);
    assert(sender.replayPoint.offset == 0);
}

void SessionState::senderCompleted(const SequenceSet& commands) {
    if (commands.empty()) return;
    QPID_LOG(debug, getId() << ": sender marked completed: " << commands);
    sender.incomplete -= commands;
    // Completion implies confirmation but we don't handle out-of-order
    // confirmation, so confirm up to the end of the first contiguous range of commands.
    senderConfirmed(SessionPoint(commands.rangesBegin()->end()));
}

void SessionState::receiverSetCommandPoint(const SessionPoint& point) {
    if (hasState() && point > receiver.received)
        throw InvalidArgumentException(QPID_MSG(getId() << ": Command-point out of range."));
    QPID_LOG(debug, getId() << ": receiver command-point set to: " << point);
    receiver.expected = point;
    if (receiver.expected > receiver.received)
        receiver.received = receiver.expected;
}

bool SessionState::receiverRecord(const AMQFrame& f) {
    if (receiverTrackingDisabled) return true; //Very nasty hack for push bridges
    if (isControl(f)) return true; // Ignore control frames.
    stateful = true;
    receiver.expected.advance(f);
    receiver.bytesSinceKnownCompleted += f.encodedSize();
    bool firstTime = receiver.expected > receiver.received;
    if (firstTime) {
        receiver.received = receiver.expected;
        receiver.incomplete += receiverGetCurrent();
    }
    QPID_LOG(trace, getId() << ": recv cmd " << receiverGetCurrent() << ": " << *f.getBody());
    if (!firstTime) QPID_LOG(trace, "Ignoring duplicate frame.");
    return firstTime;
}
    
void SessionState::receiverCompleted(SequenceNumber command, bool cumulative) {
    if (receiverTrackingDisabled) return; //Very nasty hack for push bridges
    assert(receiver.incomplete.contains(command)); // Internal error to complete command twice.
    SequenceNumber first =cumulative ? receiver.incomplete.front() : command;
    SequenceNumber last = command;
    receiver.unknownCompleted.add(first, last);
    receiver.incomplete.remove(first, last);
    QPID_LOG(debug, getId() << ": receiver marked completed: " << command
             << " incomplete: " << receiver.incomplete
             << " unknown-completed: " << receiver.unknownCompleted);
}

void SessionState::receiverKnownCompleted(const SequenceSet& commands) {
    if (!commands.empty() && commands.back() > receiver.received.command)
        throw InvalidArgumentException(QPID_MSG(getId() << ": Known-completed has invalid commands."));
    receiver.bytesSinceKnownCompleted=0;
    receiver.unknownCompleted -= commands;
    QPID_LOG(debug, getId() << ": receiver known completed: " << commands << " unknown: " << receiver.unknownCompleted);
}

bool SessionState::receiverNeedKnownCompleted() const {
    return (receiver.expected.command % SPONTANEOUS_REQUEST_INTERVAL == 0) ||
        (config.replayFlushLimit && receiver.bytesSinceKnownCompleted >= config.replayFlushLimit);
}
        
const SessionPoint& SessionState::receiverGetExpected() const { return receiver.expected; }
const SessionPoint& SessionState::receiverGetReceived() const { return receiver.received; }
const SequenceSet& SessionState::receiverGetUnknownComplete() const { return receiver.unknownCompleted; }
const SequenceSet& SessionState::receiverGetIncomplete() const { return receiver.incomplete; }

SequenceNumber SessionState::receiverGetCurrent() const {
    SequenceNumber current = receiver.expected.command;
    if (receiver.expected.offset == 0)
        --current;
    return current;
}

SessionState::Configuration::Configuration(size_t flush, size_t hard) :
    replayFlushLimit(flush), replayHardLimit(hard) {}

SessionState::SessionState(const SessionId& i, const Configuration& c)
    : id(i), timeout(), config(c), stateful(), receiverTrackingDisabled(false)
{
    QPID_LOG(debug, "SessionState::SessionState " << id << ": " << this);
}

bool SessionState::hasState() const { return stateful; }

SessionState::~SessionState() {}

std::ostream& operator<<(std::ostream& o, const SessionPoint& p) {
    return o << "(" << p.command.getValue() << "+" << p.offset << ")";
}

void SessionState::setState(
    const SequenceNumber& replayStart,
    const SequenceNumber& sendCommandPoint,
    const SequenceSet& sentIncomplete,
    const SequenceNumber& expected,
    const SequenceNumber& received,
    const SequenceSet& unknownCompleted,
    const SequenceSet& receivedIncomplete
)
{
    sender.replayPoint = replayStart;
    sender.flushPoint = sendCommandPoint;
    sender.sendPoint = sendCommandPoint;
    sender.unflushedSize = 0;
    sender.replaySize = 0;      // Replay list will be updated separately.
    sender.incomplete = sentIncomplete;
    sender.bytesSinceKnownCompleted = 0;

    receiver.expected = expected;
    receiver.received = received;
    receiver.unknownCompleted = unknownCompleted;
    receiver.incomplete = receivedIncomplete;
    receiver.bytesSinceKnownCompleted = 0;
}

void SessionState::disableReceiverTracking() { receiverTrackingDisabled = true; }
void SessionState::enableReceiverTracking() { receiverTrackingDisabled = false; }

} // namespace qpid
