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
#include "qpid/framing/amqp_framing.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/FrameHandler.h"
#include "qpid/CommonImportExport.h"

#ifndef _SendContent_
#define _SendContent_

namespace qpid {
namespace framing {

/**
 * Functor that sends frame to handler, refragmenting if
 * necessary. Currently only works on content frames but this could be
 * changed once we support multi-frame segments in general.
 */
class SendContent
{
    mutable FrameHandler& handler;
    const uint16_t maxFrameSize;
    uint expectedFrameCount;
    uint frameCount;

    void sendFragment(const AMQContentBody& body, uint32_t offset, uint16_t size, bool first, bool last) const;
    void setFlags(AMQFrame& f, bool first, bool last) const;
public:
    QPID_COMMON_EXTERN SendContent(FrameHandler& _handler, uint16_t _maxFrameSize, uint frameCount);
    QPID_COMMON_EXTERN void operator()(const AMQFrame& f);
};

}
}


#endif
