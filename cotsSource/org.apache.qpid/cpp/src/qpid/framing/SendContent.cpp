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

#include "qpid/framing/SendContent.h"

qpid::framing::SendContent::SendContent(FrameHandler& h, uint16_t mfs, uint efc) : handler(h), 
                                                                                              maxFrameSize(mfs),
                                                                                               expectedFrameCount(efc), frameCount(0) {}

void qpid::framing::SendContent::operator()(const AMQFrame& f)
{
    bool first = frameCount == 0;
    bool last = ++frameCount == expectedFrameCount;

     /*end of frame marker is included in frameOverhead() but not in
       real frame size, hence substract -1 from frameOverhead()*/
    uint16_t maxContentSize = maxFrameSize - (AMQFrame::frameOverhead() - 1);
    const AMQContentBody* body(f.castBody<AMQContentBody>()); 
    if (body->encodedSize() > maxContentSize) {
        uint32_t offset = 0;
        for (int chunk = body->encodedSize() / maxContentSize; chunk > 0; chunk--) {
            sendFragment(*body, offset, maxContentSize, first && offset == 0, last && offset + maxContentSize == body->encodedSize());
            offset += maxContentSize;
        }
        uint32_t remainder = body->encodedSize() % maxContentSize;
        if (remainder) {
            sendFragment(*body, offset, remainder, first && offset == 0, last);
        }
    } else {
        AMQFrame copy(f);
        setFlags(copy, first, last);
        handler.handle(copy);
    }        
}

void qpid::framing::SendContent::sendFragment(const AMQContentBody& body, uint32_t offset, uint16_t size, bool first, bool last) const
{
    AMQFrame fragment((AMQContentBody(body.getData().substr(offset, size))));
    setFlags(fragment, first, last);
    handler.handle(fragment);
}

void qpid::framing::SendContent::setFlags(AMQFrame& f, bool first, bool last) const
{
    f.setBof(false);
    f.setBos(first);
    f.setEof(true);//content is always the last segment
    f.setEos(last);
}

