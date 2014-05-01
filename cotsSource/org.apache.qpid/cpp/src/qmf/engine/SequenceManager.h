#ifndef _QmfEngineSequenceManager_
#define _QmfEngineSequenceManager_

/*
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
 */

#include "qpid/sys/Mutex.h"
#include <boost/shared_ptr.hpp>
#include <map>

namespace qpid {
    namespace framing {
        class Buffer;
    }
}

namespace qmf {
namespace engine {

    class SequenceContext {
    public:
        typedef boost::shared_ptr<SequenceContext> Ptr;
        SequenceContext() {}
        virtual ~SequenceContext() {}

        virtual void reserve() = 0;
        virtual bool handleMessage(uint8_t opcode, uint32_t sequence, const std::string& routingKey, qpid::framing::Buffer& buffer) = 0;
        virtual void release() = 0;
    };

    class SequenceManager {
    public:
        SequenceManager();

        void setUnsolicitedContext(SequenceContext::Ptr ctx);
        uint32_t reserve(SequenceContext::Ptr ctx = SequenceContext::Ptr());
        void release(uint32_t sequence);
        void releaseAll();
        void dispatch(uint8_t opcode, uint32_t sequence, const std::string& routingKey, qpid::framing::Buffer& buffer);

    private:
        mutable qpid::sys::Mutex lock;
        uint32_t nextSequence;
        SequenceContext::Ptr unsolicitedContext;
        std::map<uint32_t, SequenceContext::Ptr> contextMap;
    };

}
}

#endif

