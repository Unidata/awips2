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
#include "qpid/framing/AMQFrame.h"

#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/BodyFactory.h"
#include "qpid/framing/MethodBodyFactory.h"
#include <boost/format.hpp>
#include <iostream>

namespace qpid {
namespace framing {

void AMQFrame::init() {
    bof = eof = bos = eos = true;
    subchannel=0;
    channel=0;
    encodedSizeCache = 0;
}

AMQFrame::AMQFrame(const boost::intrusive_ptr<AMQBody>& b) : body(b) { init(); }

AMQFrame::AMQFrame(const AMQBody& b) : body(b.clone()) { init(); }

AMQFrame::~AMQFrame() {}

AMQBody* AMQFrame::getBody() {
    // Non-const AMQBody* may be used to modify the body.
    encodedSizeCache = 0;
    return body.get();
}

const AMQBody* AMQFrame::getBody() const {
    return body.get();
}

void AMQFrame::setMethod(ClassId c, MethodId m) {
    encodedSizeCache = 0;
    body = MethodBodyFactory::create(c,m);
}

uint32_t AMQFrame::encodedSize() const {
    if (!encodedSizeCache) {
        encodedSizeCache = frameOverhead() + body->encodedSize();
        if (body->getMethod())
            encodedSizeCache +=  sizeof(ClassId)+sizeof(MethodId);
    }
    return encodedSizeCache;
}

uint32_t AMQFrame::frameOverhead() {
    return 12 /*frame header*/;
}

uint16_t AMQFrame::DECODE_SIZE_MIN=4;

uint16_t AMQFrame::decodeSize(char* data) {
    Buffer buf(data+2, DECODE_SIZE_MIN);
    return buf.getShort();
}

void AMQFrame::encode(Buffer& buffer) const
{
    //set track first (controls on track 0, everything else on 1):
    uint8_t track = getBody()->type() ? 1 : 0;

    uint8_t flags = (bof ? 0x08 : 0) | (eof ? 0x04 : 0) | (bos ? 0x02 : 0) | (eos ? 0x01 : 0);
    buffer.putOctet(flags);
    buffer.putOctet(getBody()->type());
    buffer.putShort(encodedSize());
    buffer.putOctet(0);
    buffer.putOctet(0x0f & track);
    buffer.putShort(channel);    
    buffer.putLong(0);
    const AMQMethodBody* method=getMethod();
    if (method) {
        buffer.putOctet(method->amqpClassId());
        buffer.putOctet(method->amqpMethodId());
    }
    body->encode(buffer);
}

bool AMQFrame::decode(Buffer& buffer)
{
    if(buffer.available() < frameOverhead())
        return false;
    buffer.record();

    encodedSizeCache = 0;
    uint32_t start = buffer.getPosition();
    uint8_t  flags = buffer.getOctet();
    uint8_t framing_version = (flags & 0xc0) >> 6;
    if (framing_version != 0)
        throw FramingErrorException(QPID_MSG("Framing version unsupported"));
    bof = flags & 0x08;
    eof = flags & 0x04;
    bos = flags & 0x02;
    eos = flags & 0x01;
    uint8_t  type = buffer.getOctet();
    uint16_t frame_size =  buffer.getShort();
    if (frame_size < frameOverhead())
        throw FramingErrorException(QPID_MSG("Frame size too small " << frame_size));    
    uint8_t  reserved1 = buffer.getOctet();
    uint8_t  field1 = buffer.getOctet();
    subchannel = field1 & 0x0f;
    channel = buffer.getShort();
    (void) buffer.getLong(); // reserved2
    
    // Verify that the protocol header meets current spec
    // TODO: should we check reserved2 against zero as well? - the
    // spec isn't clear
    if ((flags & 0x30) != 0 || reserved1 != 0 || (field1 & 0xf0) != 0)
        throw FramingErrorException(QPID_MSG("Reserved bits not zero"));

    // TODO: should no longer care about body size and only pass up
    // B,E,b,e flags
    uint16_t body_size = frame_size - frameOverhead(); 
    if (buffer.available() < body_size){
        buffer.restore();
        return false;
    }

    switch(type)
    {
      case 0://CONTROL 
      case METHOD_BODY: {
          ClassId c = buffer.getOctet();
          MethodId m = buffer.getOctet();
          body = MethodBodyFactory::create(c, m);
          break;
      }
      case HEADER_BODY: body =  BodyFactory::create<AMQHeaderBody>(); break;
      case CONTENT_BODY: body = BodyFactory::create<AMQContentBody>(); break;
      case HEARTBEAT_BODY: body = BodyFactory::create<AMQHeartbeatBody>(); break;
      default:
	throw IllegalArgumentException(QPID_MSG("Invalid frame type " << type));
    }
    body->decode(buffer, body_size);
    encodedSizeCache = buffer.getPosition() - start;
    return true;
}

std::ostream& operator<<(std::ostream& out, const AMQFrame& f)
{
    return
        out << "Frame[" 
            << (f.getBof() ? "B" : "") << (f.getEof() ? "E" : "")
            << (f.getBos() ? "b" : "") << (f.getEos() ? "e" : "") << "; "
            << "channel=" << f.getChannel() << "; " << *f.getBody()
            << "]";
}


}} // namespace qpid::framing
