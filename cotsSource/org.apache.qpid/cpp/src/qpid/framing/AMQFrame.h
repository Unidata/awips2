#ifndef _AMQFrame_
#define _AMQFrame_

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
#include "qpid/framing/AMQDataBlock.h"
#include "qpid/framing/AMQHeaderBody.h"
#include "qpid/framing/AMQContentBody.h"
#include "qpid/framing/AMQHeartbeatBody.h"
#include "qpid/framing/ProtocolVersion.h"
#include <boost/intrusive_ptr.hpp>
#include <boost/cast.hpp>
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class AMQFrame : public AMQDataBlock
{
  public:
    QPID_COMMON_EXTERN AMQFrame(const boost::intrusive_ptr<AMQBody>& b=0);
    QPID_COMMON_EXTERN AMQFrame(const AMQBody& b);
    QPID_COMMON_EXTERN ~AMQFrame();

    ChannelId getChannel() const { return channel; }
    void setChannel(ChannelId c) { channel = c; }

    QPID_COMMON_EXTERN AMQBody* getBody();
    QPID_COMMON_EXTERN const AMQBody* getBody() const;

    AMQMethodBody* getMethod() { return getBody() ? getBody()->getMethod() : 0; }
    const AMQMethodBody* getMethod() const { return getBody() ? getBody()->getMethod() : 0; }

    void setMethod(ClassId c, MethodId m);

    template <class T> T* castBody() {
        return boost::polymorphic_downcast<T*>(getBody());
    }

    template <class T> const T* castBody() const {
        return boost::polymorphic_downcast<const T*>(getBody());
    }

    QPID_COMMON_EXTERN void encode(Buffer& buffer) const; 
    QPID_COMMON_EXTERN bool decode(Buffer& buffer); 
    QPID_COMMON_EXTERN uint32_t encodedSize() const;

    // 0-10 terminology: first/last frame (in segment) first/last segment (in assembly)

    bool isFirstSegment() const { return bof; }
    bool isLastSegment() const { return eof; }
    bool isFirstFrame() const { return bos; }
    bool isLastFrame() const { return eos; }

    void setFirstSegment(bool set=true) { bof = set; }
    void setLastSegment(bool set=true) { eof = set; }
    void setFirstFrame(bool set=true) { bos = set; }
    void setLastFrame(bool set=true) { eos = set; }

    // 0-9 terminology: beginning/end of frameset, beginning/end of segment.

    bool getBof() const { return bof; }
    void setBof(bool isBof) { bof = isBof; }
    bool getEof() const { return eof; }
    void setEof(bool isEof) { eof = isEof; }

    bool getBos() const { return bos; }
    void setBos(bool isBos) { bos = isBos; }
    bool getEos() const { return eos; }
    void setEos(bool isEos) { eos = isEos; }

    static uint16_t DECODE_SIZE_MIN;
    QPID_COMMON_EXTERN static uint32_t frameOverhead();
    /** Must point to at least DECODE_SIZE_MIN bytes of data */
    static uint16_t decodeSize(char* data);

  private:
    void init();

    boost::intrusive_ptr<AMQBody> body;
    uint16_t channel : 16;
    uint8_t subchannel : 8;
    bool bof : 1;
    bool eof : 1;
    bool bos : 1;
    bool eos : 1;
    mutable uint32_t encodedSizeCache;
};

QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream&, const AMQFrame&);

}} // namespace qpid::framing


#endif
