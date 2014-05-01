#ifndef _QmfEngineObjectIdImpl_
#define _QmfEngineObjectIdImpl_

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

#include <qmf/engine/ObjectId.h>
#include <qpid/framing/Buffer.h>

namespace qmf {
namespace engine {

    struct AgentAttachment {
        uint64_t first;

        AgentAttachment() : first(0) {}
        void setBanks(uint32_t broker, uint32_t bank);
        uint64_t getFirst() const { return first; }
    };

    struct ObjectIdImpl {
        AgentAttachment* agent;
        uint64_t first;
        uint64_t second;
        mutable std::string repr;

        ObjectIdImpl() : agent(0), first(0), second(0) {}
        ObjectIdImpl(qpid::framing::Buffer& buffer);
        ObjectIdImpl(AgentAttachment* agent, uint8_t flags, uint16_t seq, uint64_t object);

        static ObjectId* factory(qpid::framing::Buffer& buffer);
        static ObjectId* factory(AgentAttachment* agent, uint8_t flags, uint16_t seq, uint64_t object);

        void decode(qpid::framing::Buffer& buffer);
        void encode(qpid::framing::Buffer& buffer) const;
        void fromString(const std::string& repr);
        const std::string& asString() const;
        uint8_t getFlags() const;
        uint16_t getSequence() const;
        uint32_t getBrokerBank() const;
        uint32_t getAgentBank() const;
        uint64_t getObjectNum() const;
        uint32_t getObjectNumHi() const;
        uint32_t getObjectNumLo() const;
        bool isDurable() const { return getSequence() == 0; }
        void setValue(uint64_t f, uint64_t s) { first = f; second = s; agent = 0; }

        bool operator==(const ObjectIdImpl& other) const;
        bool operator<(const ObjectIdImpl& other) const;
        bool operator>(const ObjectIdImpl& other) const;
    };
}
}

#endif

