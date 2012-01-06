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
#include "qpid/framing/FrameDecoder.h"
#include "qpid/framing/Buffer.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/reply_exceptions.h"
#include <algorithm>
#include <string.h>

namespace qpid {
namespace framing {

namespace {
/** Append up to n bytes from start of buf to end of bytes. */
void append(std::vector<char>& bytes, Buffer& buffer, size_t n) {
    size_t oldSize = bytes.size();
    if ((n = std::min(n, size_t(buffer.available()))) == 0)
        return;
    bytes.resize(oldSize+n);
    char* p = &bytes[oldSize];
    buffer.getRawData(reinterpret_cast<uint8_t*>(p), n);
}
}

bool FrameDecoder::decode(Buffer& buffer) {
    if (buffer.available() == 0) return false;
    if (fragment.empty()) {
        if (frame.decode(buffer)) // Decode from buffer
            return true;
        else                    // Store fragment
            append(fragment, buffer, buffer.available());
        }
    else {                      // Already have a fragment
        // Get enough data to decode the frame size.
        if (fragment.size() < AMQFrame::DECODE_SIZE_MIN) {
            append(fragment, buffer, AMQFrame::DECODE_SIZE_MIN - fragment.size());
        }
        if (fragment.size() >= AMQFrame::DECODE_SIZE_MIN) {
            uint16_t size = AMQFrame::decodeSize(&fragment[0]);
            if (size <= fragment.size())
                throw FramingErrorException(QPID_MSG("Frame size " << size << " is too small."));
            append(fragment, buffer, size-fragment.size());
            Buffer b(&fragment[0], fragment.size());
            if (frame.decode(b)) {
                assert(b.available() == 0);
                fragment.clear();
                return true;
            }
        }
    }
    return false;
}

void FrameDecoder::setFragment(const char* data, size_t size) {
    fragment.resize(size);
    ::memcpy(&fragment[0], data, size);
}

std::pair<const char*, size_t> FrameDecoder::getFragment() const {
    return std::pair<const char*, size_t>(&fragment[0], fragment.size());
}

}} // namespace qpid::framing
