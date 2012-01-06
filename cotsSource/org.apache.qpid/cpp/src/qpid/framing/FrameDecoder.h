#ifndef QPID_FRAMING_FRAMEDECODER_H
#define QPID_FRAMING_FRAMEDECODER_H

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
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

/**
 * Decode a frame from buffer.  If buffer does not contain a complete
 * frame, caches the fragment for the next call to decode.
 */
class FrameDecoder
{
  public:
    QPID_COMMON_EXTERN bool decode(Buffer& buffer);
    const AMQFrame& getFrame() const { return frame; }
    AMQFrame& getFrame() { return frame; }

    void setFragment(const char*, size_t);
    std::pair<const char*, size_t> getFragment() const;

  private:
    std::vector<char> fragment;
    AMQFrame frame;

};
}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_FRAMEDECODER_H*/
