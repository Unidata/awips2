#ifndef _QmfEngineObjectId_
#define _QmfEngineObjectId_

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

#include <qpid/sys/IntegerTypes.h>

namespace qmf {
namespace engine {

    // TODO: Add to/from string and << operator

    struct ObjectIdImpl;
    class ObjectId {
    public:
        ObjectId();
        ObjectId(const ObjectId& from);
        ~ObjectId();

        uint64_t getObjectNum() const;
        uint32_t getObjectNumHi() const;
        uint32_t getObjectNumLo() const;
        bool isDurable() const;
        const char* str() const;
        uint8_t getFlags() const;
        uint16_t getSequence() const;
        uint32_t getBrokerBank() const;
        uint32_t getAgentBank() const;

        bool operator==(const ObjectId& other) const;
        bool operator<(const ObjectId& other) const;
        bool operator>(const ObjectId& other) const;
        bool operator<=(const ObjectId& other) const;
        bool operator>=(const ObjectId& other) const;

    private:
        friend struct ObjectIdImpl;
        friend struct ObjectImpl;
        friend class  BrokerProxyImpl;
        friend struct QueryImpl;
        friend struct ValueImpl;
        friend class  AgentImpl;
        ObjectId(ObjectIdImpl* impl);
        ObjectIdImpl* impl;
    };
}
}

#endif

