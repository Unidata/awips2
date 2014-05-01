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
#include <string>
#include <ostream>
#include <iostream>
#include "qpid/framing/amqp_framing.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/Buffer.h"

#ifndef _frame_functors_
#define _frame_functors_

namespace qpid {
namespace framing {

class SumFrameSize
{
    uint64_t size;
public:
    SumFrameSize() : size(0) {}
    void operator()(const AMQFrame& f) { size += f.encodedSize(); }
    uint64_t getSize() { return size; }
};

class SumBodySize
{
    uint64_t size;
public:
    SumBodySize() : size(0) {}
    void operator()(const AMQFrame& f) { size += f.getBody()->encodedSize(); }
    uint64_t getSize() { return size; }
};

class Count
{
    uint count;
public:
    Count() : count(0) {}
    void operator()(const AMQFrame&) { count++; }
    uint getCount() { return count; }
};

class EncodeFrame
{
    Buffer& buffer;
public:
    EncodeFrame(Buffer& b) : buffer(b) {}
    void operator()(const AMQFrame& f) { f.encode(buffer); }
};

class EncodeBody
{
    Buffer& buffer;
public:
    EncodeBody(Buffer& b) : buffer(b) {}
    void operator()(const AMQFrame& f) { f.getBody()->encode(buffer); }
};

/**
 * Sends to the specified handler a copy of the frame it is applied to.
 */
class Relay
{
    FrameHandler& handler;

public:
    Relay(FrameHandler& h) : handler(h) {}

    void operator()(const AMQFrame& f)
    {
        AMQFrame copy(f);
        handler.handle(copy);
    }
};

class Print
{
    std::ostream& out;
public:
    Print(std::ostream& o) : out(o) {}

    void operator()(const AMQFrame& f)
    {
        out << f << std::endl;
    }
};

class MarkLastSegment
{
public:
    void operator()(AMQFrame& f) const { f.setEof(true); }
};

}
}


#endif
