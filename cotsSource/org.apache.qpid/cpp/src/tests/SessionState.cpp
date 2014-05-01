/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "unit_test.h"

#include "qpid/SessionState.h"
#include "qpid/Exception.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/framing/SessionFlushBody.h"

#include <boost/bind.hpp>
#include <algorithm>
#include <functional>
#include <numeric>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(SessionStateTestSuite)

using namespace std;
using namespace boost;
using namespace qpid::framing;

// ================================================================
// Utility functions.

// Apply f to [begin, end) and accumulate the result
template <class Iter, class T, class F>
T applyAccumulate(Iter begin, Iter end, T seed, const F& f) {
    return std::accumulate(begin, end, seed, bind(std::plus<T>(), _1, bind(f, _2)));
}

// Create a frame with a one-char string.
AMQFrame& frame(char s) {
    static AMQFrame frame((AMQContentBody(string(&s, 1))));
    return frame;
}

// Simple string representation of a frame.
string str(const AMQFrame& f) {
    if (f.getMethod()) return "C"; // Command or Control
    const AMQContentBody* c = dynamic_cast<const AMQContentBody*>(f.getBody());
    if (c) return c->getData(); // Return data for content frames.
    return "H";                 // Must be a header.
}
// Make a string from a range of frames.
string str(const boost::iterator_range<vector<AMQFrame>::const_iterator>& frames) {
    string (*strFrame)(const AMQFrame&) = str;
    return applyAccumulate(frames.begin(), frames.end(), string(), ptr_fun(strFrame));
}
// Make a transfer command frame.
AMQFrame transferFrame(bool hasContent) {
    AMQFrame t((MessageTransferBody()));
    t.setFirstFrame(true);
    t.setLastFrame(true);
    t.setFirstSegment(true);
    t.setLastSegment(!hasContent);
    return t;
}
// Make a content frame
AMQFrame contentFrame(string content, bool isLast=true) {
    AMQFrame f((AMQContentBody(content)));
    f.setFirstFrame(true);
    f.setLastFrame(true);
    f.setFirstSegment(false);
    f.setLastSegment(isLast);
    return f;
}
AMQFrame contentFrameChar(char content, bool isLast=true) {
    return contentFrame(string(1, content), isLast);
}

// Send frame & return size of frame.
size_t send(qpid::SessionState& s, const AMQFrame& f) { s.senderRecord(f); return f.encodedSize(); }
// Send transfer command with no content.
size_t transfer0(qpid::SessionState& s) { return send(s, transferFrame(false)); }
// Send transfer frame with single content frame.
size_t transfer1(qpid::SessionState& s, string content) {
    return send(s,transferFrame(true)) + send(s,contentFrame(content));
}
size_t transfer1Char(qpid::SessionState& s, char content) {
    return transfer1(s, string(1,content));
}

// Send transfer frame with multiple single-byte content frames.
size_t transferN(qpid::SessionState& s, string content) {
    size_t size=send(s, transferFrame(!content.empty()));
    if (!content.empty()) {
        char last = content[content.size()-1];
        content.resize(content.size()-1);
        size += applyAccumulate(content.begin(), content.end(), 0,
                                bind(&send, ref(s),
                                     bind(contentFrameChar, _1, false)));
        size += send(s, contentFrameChar(last, true));
    }
    return size;
}

// Send multiple transfers with single-byte content.
size_t transfers(qpid::SessionState& s, string content) {
    return applyAccumulate(content.begin(), content.end(), 0,
                           bind(transfer1Char, ref(s), _1));
}

size_t contentFrameSize(size_t n=1) { return AMQFrame(( AMQContentBody())).encodedSize() + n; }
size_t transferFrameSize() { return AMQFrame((MessageTransferBody())).encodedSize(); }

// ==== qpid::SessionState test classes

using qpid::SessionId;
using qpid::SessionPoint;


QPID_AUTO_TEST_CASE(testSendGetReplyList) {
    qpid::SessionState s;
    s.setTimeout(1);
    s.senderGetCommandPoint();
    transfer1(s, "abc");
    transfers(s, "def");
    transferN(s, "xyz");
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(0,0))),"CabcCdCeCfCxyz");
    // Ignore controls.
    s.senderRecord(AMQFrame(new SessionFlushBody()));
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(2,0))),"CeCfCxyz");
}

QPID_AUTO_TEST_CASE(testNeedFlush) {
    qpid::SessionState::Configuration c;
    // sync after 2 1-byte transfers or equivalent bytes.
    c.replayFlushLimit = 2*(transferFrameSize()+contentFrameSize());
    qpid::SessionState s(SessionId(), c);
    s.setTimeout(1);
    s.senderGetCommandPoint();
    transfers(s, "a");
    BOOST_CHECK(!s.senderNeedFlush());
    transfers(s, "b");
    BOOST_CHECK(s.senderNeedFlush());
    s.senderRecordFlush();
    BOOST_CHECK(!s.senderNeedFlush());
    transfers(s, "c");
    BOOST_CHECK(!s.senderNeedFlush());
    transfers(s, "d");
    BOOST_CHECK(s.senderNeedFlush());
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint())), "CaCbCcCd");
}

QPID_AUTO_TEST_CASE(testPeerConfirmed) {
    qpid::SessionState::Configuration c;
    // sync after 2 1-byte transfers or equivalent bytes.
    c.replayFlushLimit = 2*(transferFrameSize()+contentFrameSize());
    qpid::SessionState s(SessionId(), c);
    s.setTimeout(1);
    s.senderGetCommandPoint();
    transfers(s, "ab");
    BOOST_CHECK(s.senderNeedFlush());
    transfers(s, "cd");
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(0,0))), "CaCbCcCd");
    s.senderConfirmed(SessionPoint(3));
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(3,0))), "Cd");
    BOOST_CHECK(!s.senderNeedFlush());

    // Multi-frame transfer.
    transfer1(s, "efg");
    transfers(s, "xy");
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(3,0))), "CdCefgCxCy");
    BOOST_CHECK(s.senderNeedFlush());

    s.senderConfirmed(SessionPoint(4));
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(4,0))), "CefgCxCy");
    BOOST_CHECK(s.senderNeedFlush());

    s.senderConfirmed(SessionPoint(5));
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(5,0))), "CxCy");
    BOOST_CHECK(s.senderNeedFlush());

    s.senderConfirmed(SessionPoint(6));
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(6,0))), "Cy");
    BOOST_CHECK(!s.senderNeedFlush());
}

QPID_AUTO_TEST_CASE(testPeerCompleted) {
    qpid::SessionState s;
    s.setTimeout(1);
    s.senderGetCommandPoint();
    // Completion implies confirmation
    transfers(s, "abc");
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(0,0))), "CaCbCc");
    SequenceSet set(SequenceSet() + 0 + 1);
    s.senderCompleted(set);
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(2,0))), "Cc");

    transfers(s, "def");
    // We dont do out-of-order confirmation, so this will only confirm up to 3:
    set = SequenceSet(SequenceSet() + 2 + 3 + 5);
    s.senderCompleted(set);
    BOOST_CHECK_EQUAL(str(s.senderExpected(SessionPoint(4,0))), "CeCf");
}

QPID_AUTO_TEST_CASE(testReceive) {
    // Advance expected/received correctly
    qpid::SessionState s;
    s.receiverSetCommandPoint(SessionPoint());
    BOOST_CHECK_EQUAL(s.receiverGetExpected(), SessionPoint(0));
    BOOST_CHECK_EQUAL(s.receiverGetReceived(), SessionPoint(0));

    BOOST_CHECK(s.receiverRecord(transferFrame(false)));
    BOOST_CHECK_EQUAL(s.receiverGetExpected(), SessionPoint(1));
    BOOST_CHECK_EQUAL(s.receiverGetReceived(), SessionPoint(1));

    BOOST_CHECK(s.receiverRecord(transferFrame(true)));
    SessionPoint point = SessionPoint(1, transferFrameSize());
    BOOST_CHECK_EQUAL(s.receiverGetExpected(), point);
    BOOST_CHECK_EQUAL(s.receiverGetReceived(), point);
    BOOST_CHECK(s.receiverRecord(contentFrame("", false)));
    point.offset += contentFrameSize(0);
    BOOST_CHECK_EQUAL(s.receiverGetExpected(), point);
    BOOST_CHECK_EQUAL(s.receiverGetReceived(), point);
    BOOST_CHECK(s.receiverRecord(contentFrame("", true)));
    BOOST_CHECK_EQUAL(s.receiverGetExpected(), SessionPoint(2));
    BOOST_CHECK_EQUAL(s.receiverGetReceived(), SessionPoint(2));

    // Idempotence barrier, rewind expected & receive some duplicates.
    s.receiverSetCommandPoint(SessionPoint(1));
    BOOST_CHECK(!s.receiverRecord(transferFrame(false)));
    BOOST_CHECK_EQUAL(s.receiverGetExpected(), SessionPoint(2));
    BOOST_CHECK_EQUAL(s.receiverGetReceived(), SessionPoint(2));
    BOOST_CHECK(s.receiverRecord(transferFrame(false)));
    BOOST_CHECK_EQUAL(s.receiverGetExpected(), SessionPoint(3));
    BOOST_CHECK_EQUAL(s.receiverGetReceived(), SessionPoint(3));
}

QPID_AUTO_TEST_CASE(testCompleted) {
    // completed & unknownCompleted
    qpid::SessionState s;
    s.receiverSetCommandPoint(SessionPoint());
    s.receiverRecord(transferFrame(false));
    s.receiverRecord(transferFrame(false));
    s.receiverRecord(transferFrame(false));
    s.receiverCompleted(1);
    BOOST_CHECK_EQUAL(s.receiverGetUnknownComplete(), SequenceSet(SequenceSet()+1));
    s.receiverCompleted(0);
    BOOST_CHECK_EQUAL(s.receiverGetUnknownComplete(),
                      SequenceSet(SequenceSet() + qpid::Range<SequenceNumber>(0,2)));
    s.receiverKnownCompleted(SequenceSet(SequenceSet()+1));
    BOOST_CHECK_EQUAL(s.receiverGetUnknownComplete(), SequenceSet(SequenceSet()+2));
    // TODO aconway 2008-04-30: missing tests for known-completed.
}

QPID_AUTO_TEST_CASE(testNeedKnownCompleted) {
    size_t flushInterval= 2*(transferFrameSize()+contentFrameSize())+1;
    qpid::SessionState::Configuration c(flushInterval);
    qpid::SessionState s(qpid::SessionId(), c);
    s.senderGetCommandPoint();
    transfers(s, "a");
    SequenceSet set(SequenceSet() + 0);
    s.senderCompleted(set);
    BOOST_CHECK(!s.senderNeedKnownCompleted());

    transfers(s, "b");
    set += 1;
    s.senderCompleted(set);
    BOOST_CHECK(!s.senderNeedKnownCompleted());

    transfers(s, "c");
    set += 2;
    s.senderCompleted(set);
    BOOST_CHECK(s.senderNeedKnownCompleted());
    s.senderRecordKnownCompleted();
    BOOST_CHECK(!s.senderNeedKnownCompleted());

    transfers(s, "de");
    set += 3;
    set += 4;
    s.senderCompleted(set);
    BOOST_CHECK(!s.senderNeedKnownCompleted());

    transfers(s, "f");
    set += 2;
    s.senderCompleted(set);
    BOOST_CHECK(s.senderNeedKnownCompleted());
    s.senderRecordKnownCompleted();
    BOOST_CHECK(!s.senderNeedKnownCompleted());
}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
