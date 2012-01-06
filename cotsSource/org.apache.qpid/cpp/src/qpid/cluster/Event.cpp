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
#include "qpid/cluster/types.h"
#include "qpid/cluster/Event.h"
#include "qpid/cluster/Cpg.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/assert.h"
#include <ostream>
#include <iterator>
#include <algorithm>

namespace qpid {
namespace cluster {

using framing::Buffer;
using framing::AMQFrame;

const size_t EventHeader::HEADER_SIZE =
    sizeof(uint8_t) +  // type
    sizeof(uint64_t) + // connection pointer only, CPG provides member ID.
    sizeof(uint32_t)  // payload size
    ;

EventHeader::EventHeader(EventType t, const ConnectionId& c,  size_t s)
    : type(t), connectionId(c), size(s) {}


Event::Event() {}

Event::Event(EventType t, const ConnectionId& c,  size_t s)
    : EventHeader(t,c,s), store(RefCountedBuffer::create(s+HEADER_SIZE))
{}

void EventHeader::decode(const MemberId& m, framing::Buffer& buf) {
    if (buf.available() <= HEADER_SIZE)
        throw Exception("Not enough for multicast header");
    type = (EventType)buf.getOctet();
    if(type != DATA && type != CONTROL)
        throw Exception("Invalid multicast event type");
    connectionId = ConnectionId(m, buf.getLongLong());
    size = buf.getLong();
}

Event Event::decodeCopy(const MemberId& m, framing::Buffer& buf) {
    Event e;
    e.decode(m, buf);           // Header
    if (buf.available() < e.size)
        throw Exception("Not enough data for multicast event");
    e.store = RefCountedBuffer::create(e.size + HEADER_SIZE);
    memcpy(e.getData(), buf.getPointer() + buf.getPosition(), e.size);
    return e;
}

Event Event::control(const framing::AMQFrame& f, const ConnectionId& cid) {
    Event e(CONTROL, cid, f.encodedSize());
    Buffer buf(e);
    f.encode(buf);
    return e;
}

Event Event::control(const framing::AMQBody& body, const ConnectionId& cid) {
    return control(framing::AMQFrame(body), cid);
}

iovec Event::toIovec() const {
    encodeHeader();
    iovec iov = { const_cast<char*>(getStore()), getStoreSize() };
    return iov;
}

void EventHeader::encode(Buffer& b) const {
    b.putOctet(type);
    b.putLongLong(connectionId.getNumber());
    b.putLong(size);
}

// Encode my header in my buffer.
void Event::encodeHeader () const {
    Buffer b(const_cast<char*>(getStore()), HEADER_SIZE);
    encode(b);
    assert(b.getPosition() == HEADER_SIZE);
}

Event::operator Buffer() const  {
    return Buffer(const_cast<char*>(getData()), getSize());
}

const AMQFrame& Event::getFrame() const {
    assert(type == CONTROL);
    if (!frame.getBody()) {
    Buffer buf(*this);
    QPID_ASSERT(frame.decode(buf));
    }
    return frame;
}

static const char* EVENT_TYPE_NAMES[] = { "data", "control" };

std::ostream& operator<< (std::ostream& o, EventType t) {
    return o << EVENT_TYPE_NAMES[t];
}

std::ostream& operator<< (std::ostream& o, const EventHeader& e) {
    return o << "Event[" << e.getConnectionId() << " " << e.getType()
             << " " << e.getSize() << " bytes]";
}

std::ostream& operator<< (std::ostream& o, const Event& e) {
    o << "Event[" << e.getConnectionId() << " ";
    if (e.getType() == CONTROL)
        o << e.getFrame();
    else
        o << " data " << e.getSize() << " bytes";
    return o << "]";
}

}} // namespace qpid::cluster
