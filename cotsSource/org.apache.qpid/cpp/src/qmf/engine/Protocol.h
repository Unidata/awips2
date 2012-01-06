#ifndef _QmfEngineProtocol_
#define _QmfEngineProtocol_

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

namespace qpid {
    namespace framing {
        class Buffer;
    }
}

namespace qmf {
namespace engine {

    class Protocol {
    public:
        static bool checkHeader(qpid::framing::Buffer& buf, uint8_t *opcode, uint32_t *seq);
        static void encodeHeader(qpid::framing::Buffer& buf, uint8_t opcode, uint32_t seq = 0);

        const static uint8_t OP_ATTACH_REQUEST  = 'A';
        const static uint8_t OP_ATTACH_RESPONSE = 'a';

        const static uint8_t OP_BROKER_REQUEST  = 'B';
        const static uint8_t OP_BROKER_RESPONSE = 'b';

        const static uint8_t OP_CONSOLE_ADDED_INDICATION = 'x';
        const static uint8_t OP_COMMAND_COMPLETE         = 'z';
        const static uint8_t OP_HEARTBEAT_INDICATION     = 'h';

        const static uint8_t OP_PACKAGE_REQUEST    = 'P';
        const static uint8_t OP_PACKAGE_INDICATION = 'p';
        const static uint8_t OP_CLASS_QUERY        = 'Q';
        const static uint8_t OP_CLASS_INDICATION   = 'q';
        const static uint8_t OP_SCHEMA_REQUEST     = 'S';
        const static uint8_t OP_SCHEMA_RESPONSE    = 's';

        const static uint8_t OP_METHOD_REQUEST       = 'M';
        const static uint8_t OP_METHOD_RESPONSE      = 'm';
        const static uint8_t OP_GET_QUERY            = 'G';
        const static uint8_t OP_OBJECT_INDICATION    = 'g';
        const static uint8_t OP_PROPERTY_INDICATION  = 'c';
        const static uint8_t OP_STATISTIC_INDICATION = 'i';
        const static uint8_t OP_EVENT_INDICATION     = 'e';
    };

}
}

#endif

