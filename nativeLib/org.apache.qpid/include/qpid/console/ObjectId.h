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
#ifndef _QPID_CONSOLE_OBJECTID_H
#define _QPID_CONSOLE_OBJECTID_H

#include <iostream>
#include "qpid/console/ConsoleImportExport.h"
#include "qpid/sys/IntegerTypes.h"

namespace qpid {
namespace framing {
    class Buffer;
}
namespace console {

    /**
     *
     * \ingroup qmfconsoleapi
     */
    class QPID_CONSOLE_EXTERN ObjectId {
    public:
        ObjectId() : first(0), second(0) {}
        ObjectId(framing::Buffer& buffer);

        uint8_t getFlags() const { return (first & 0xF000000000000000LL) >> 60; }
        uint16_t getSequence() const { return (first & 0x0FFF000000000000LL) >> 48; }
        uint32_t getBrokerBank() const { return (first & 0x0000FFFFF0000000LL) >> 28; }
        uint32_t getAgentBank() const { return first & 0x000000000FFFFFFFLL; }
        uint64_t getObject() const { return second; }
        bool isDurable() const { return getSequence() == 0; }
        void decode(framing::Buffer& buffer);
        void encode(framing::Buffer& buffer);
        void setValue(uint64_t f, uint64_t s) { first = f; second = s; }

        bool operator==(const ObjectId& other) const;
        bool operator!=(const ObjectId& other) const;
        bool operator<(const ObjectId& other) const;
        bool operator>(const ObjectId& other) const;
        bool operator<=(const ObjectId& other) const;
        bool operator>=(const ObjectId& other) const;

    private:
        uint64_t first;
        uint64_t second;
    };

    QPID_CONSOLE_EXTERN std::ostream& operator<<(std::ostream& o, const ObjectId& id);
}
}

#endif
