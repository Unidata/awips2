#ifndef QPID_FRAMING_CHANNELHANDLER_H
#define QPID_FRAMING_CHANNELHANDLER_H

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
#include "qpid/framing/FrameHandler.h"
#include "qpid/framing/AMQFrame.h"

namespace qpid {
namespace framing {

/**
 * Sets the channel number on outgoing frames.
 */
class ChannelHandler : public FrameHandler
{
  public:
    ChannelHandler(uint16_t channelId=0, FrameHandler* next=0)
        : FrameHandler(next), channel(channelId) {}
    void handle(AMQFrame& frame) {
        frame.setChannel(channel);
        next->handle(frame);
    }
    uint16_t get() const { return channel; }
    ChannelHandler& set(uint16_t ch) { channel=ch; return *this; }
    operator uint16_t() const { return get(); }
    ChannelHandler& operator=(uint16_t ch) { return set(ch); }

  private:
    uint16_t channel;
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_CHANNELHANDLER_H*/
