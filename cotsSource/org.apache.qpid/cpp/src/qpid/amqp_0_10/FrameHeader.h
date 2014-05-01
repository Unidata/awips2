#ifndef QPID_AMQP_0_10_FRAMEHEADER_H
#define QPID_AMQP_0_10_FRAMEHEADER_H

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

#include "qpid/amqp_0_10/built_in_types.h"
#include <boost/shared_array.hpp>
#include <string.h>
#include <assert.h>
#include <iosfwd>

namespace qpid {
namespace amqp_0_10 {

enum FrameFlags { FIRST_SEGMENT=8, LAST_SEGMENT=4, FIRST_FRAME=2, LAST_FRAME=1 };

class FrameHeader {
  public:
    static const size_t SIZE=12;
    static uint8_t trackFor(SegmentType type) { return type == 0 ? 0 : 1; }
    
    FrameHeader(uint8_t flags_=0, SegmentType type_=SegmentType(), uint16_t size_=0, uint8_t track_=0, uint16_t channel_=0)
        : flags(flags_), type(type_), size(size_), track(track_), channel(channel_)
    {}

    uint8_t getFlags() const { return flags; }
    SegmentType getType() const { return type; }
    /** @return size total size of of frame, including frame header. */
    uint16_t getSize() const { return size; }
    /** @return size of frame data, excluding frame header. */
    uint16_t getDataSize() const { return size - SIZE; }
    uint8_t getTrack() const { return track; }
    uint16_t getChannel() const { return channel; }

    void setFlags(uint8_t flags_) { flags=flags_; }
    /** Also sets the track. There is no setTrack() */
    void setType(SegmentType type_)  { type=type_; track=trackFor(type); }
    /** @param size total size of of frame, including frame header. */
    void setSize(uint16_t size_) { size = size_; }
    /** @param size size of frame data, excluding frame header. */
    void setDataSize(uint16_t size_) { size = size_+SIZE; }
    void setChannel(uint8_t channel_) { channel=channel_; }

    bool allFlags(uint8_t f) const { return (flags & f) == f; }
    bool anyFlags(uint8_t f) const { return (flags & f); }

    void raiseFlags(uint8_t f) { flags |= f; }
    void clearFlags(uint8_t f) { flags &= ~f; }

    bool isComplete() const { return allFlags(FIRST_FRAME | LAST_FRAME); }

    bool operator==(const FrameHeader&) const;

    template <class S> void serialize(S& s) {
        uint8_t pad8=0; uint32_t pad32=0;
        s(flags)(type)(size)(pad8)(track)(channel)(pad32);
    }

  private:
    uint8_t flags;
    SegmentType type;
    uint16_t size;
    uint8_t track;
    uint16_t channel;
};

std::ostream& operator<<(std::ostream&, const FrameHeader&);

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_FRAMEHEADER_H*/
