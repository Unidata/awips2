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
#ifndef _QPID_CONSOLE_SCHEMA_H_
#define _QPID_CONSOLE_SCHEMA_H_

#include "qpid/console/ClassKey.h"
#include <boost/shared_ptr.hpp>
#include <vector>

namespace qpid {
namespace framing {
    class Buffer;
}
namespace console {
    class Value;

    struct SchemaArgument {
        SchemaArgument(framing::Buffer& buffer, bool forMethod = false);
        boost::shared_ptr<Value> decodeValue(framing::Buffer& buffer);

        std::string name;
        uint8_t typeCode;
        bool dirInput;
        bool dirOutput;
        std::string unit;
        int min;
        int max;
        int maxLen;
        std::string desc;
        std::string defaultVal;
    };

    struct SchemaProperty {
        SchemaProperty(framing::Buffer& buffer);
        boost::shared_ptr<Value> decodeValue(framing::Buffer& buffer);

        std::string name;
        uint8_t typeCode;
        uint8_t accessCode;
        bool isIndex;
        bool isOptional;
        std::string unit;
        int min;
        int max;
        int maxLen;
        std::string desc;
    };

    struct SchemaStatistic {
        SchemaStatistic(framing::Buffer& buffer);
        boost::shared_ptr<Value> decodeValue(framing::Buffer& buffer);

        std::string name;
        uint8_t typeCode;
        std::string unit;
        std::string desc;
    };

    struct SchemaMethod {
        SchemaMethod(framing::Buffer& buffer);
        ~SchemaMethod();

        std::string name;
        std::string desc;
        std::vector<SchemaArgument*> arguments;
    };

    struct SchemaClass {
        static const uint8_t KIND_TABLE = 1;
        static const uint8_t KIND_EVENT = 2;

        SchemaClass(const uint8_t kind, const ClassKey& key, framing::Buffer& buffer);
        ~SchemaClass();
        const ClassKey& getClassKey() const { return key; }

        const uint8_t kind;
        const ClassKey key;
        std::vector<SchemaProperty*> properties;
        std::vector<SchemaStatistic*> statistics;
        std::vector<SchemaMethod*> methods;
        std::vector<SchemaArgument*> arguments;
    };
}
}


#endif
