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

#include "qpid/framing/FrameSet.h"
#include "qpid/framing/all_method_bodies.h"
#include "qpid/framing/frame_functors.h"
#include "qpid/framing/MessageProperties.h"
#include "qpid/framing/TypeFilter.h"

using namespace qpid::framing;
using namespace boost;

FrameSet::FrameSet(const SequenceNumber& _id) : id(_id),contentSize(0),recalculateSize(true) { }

void FrameSet::append(const AMQFrame& part)
{
    parts.push_back(part);
    recalculateSize = true;
}

bool FrameSet::isComplete() const
{
    return !parts.empty() && parts.back().getEof() && parts.back().getEos();
}

bool FrameSet::isContentBearing() const
{
    const AMQMethodBody* method = getMethod();
    return method && method->isContentBearing();
}

const AMQMethodBody* FrameSet::getMethod() const
{
    return parts.empty() ? 0 : parts[0].getMethod();
}

AMQMethodBody* FrameSet::getMethod() 
{
    return parts.empty() ? 0 : parts[0].getMethod();
}

const AMQHeaderBody* FrameSet::getHeaders() const
{
    return parts.size() < 2 ? 0 : parts[1].castBody<AMQHeaderBody>();
}

AMQHeaderBody* FrameSet::getHeaders()
{
    return parts.size() < 2 ? 0 : parts[1].castBody<AMQHeaderBody>();
}

uint64_t FrameSet::getContentSize() const
{
    if (recalculateSize)
	{
	    SumBodySize sum;
        map_if(sum, TypeFilter<CONTENT_BODY>());
        contentSize = sum.getSize();
		recalculateSize = false;
	}
	return contentSize;
}

void FrameSet::getContent(std::string& out) const {
    out.clear();
    out.reserve(getContentSize());
    for(Frames::const_iterator i = parts.begin(); i != parts.end(); i++) {
        if (i->getBody()->type() == CONTENT_BODY)
            out += i->castBody<AMQContentBody>()->getData();
    }
}

std::string FrameSet::getContent() const {
    std::string out;
    getContent(out);
    return out;
}
