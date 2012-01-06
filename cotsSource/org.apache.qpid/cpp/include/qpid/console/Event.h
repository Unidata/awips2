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
#ifndef _QPID_CONSOLE_EVENT_H_
#define _QPID_CONSOLE_EVENT_H_

#include "qpid/console/ConsoleImportExport.h"
#include "qpid/console/Object.h"
#include "qpid/framing/Uuid.h"
#include "qpid/framing/FieldTable.h"

namespace qpid {
namespace framing {
    class Buffer;
}
namespace console {

    class  Broker;
    struct SchemaClass;
    class  ClassKey;

    /**
     *
     * \ingroup qmfconsoleapi
     */
    class Event {
    public:
        typedef enum {
        SEV_EMERGENCY = 0, SEV_ALERT = 1, SEV_CRITICAL = 2, SEV_ERROR = 3,
        SEV_WARNING = 4, SEV_NOTICE = 5, SEV_INFO = 6, SEV_DEBUG = 7
        } Severity;

        QPID_CONSOLE_EXTERN Event(Broker* broker,
                                  SchemaClass* schemaClass,
                                  framing::Buffer& buffer);
        Broker* getBroker() const { return broker; }
        QPID_CONSOLE_EXTERN const ClassKey& getClassKey() const;
        SchemaClass* getSchema() const { return schema; }
        const Object::AttributeMap& getAttributes() const { return attributes; }
        uint64_t getTimestamp() const { return timestamp; }
        uint8_t getSeverity() const { return severity; }
        QPID_CONSOLE_EXTERN std::string getSeverityString() const;

        QPID_CONSOLE_EXTERN ObjectId attrRef(const std::string& key) const;
        QPID_CONSOLE_EXTERN uint32_t attrUint(const std::string& key) const;
        QPID_CONSOLE_EXTERN int32_t attrInt(const std::string& key) const;
        QPID_CONSOLE_EXTERN uint64_t attrUint64(const std::string& key) const;
        QPID_CONSOLE_EXTERN int64_t attrInt64(const std::string& key) const;
        QPID_CONSOLE_EXTERN std::string attrString(const std::string& key) const;
        QPID_CONSOLE_EXTERN bool attrBool(const std::string& key) const;
        QPID_CONSOLE_EXTERN float attrFloat(const std::string& key) const;
        QPID_CONSOLE_EXTERN double attrDouble(const std::string& key) const;
        QPID_CONSOLE_EXTERN framing::Uuid attrUuid(const std::string& key) const;
        QPID_CONSOLE_EXTERN framing::FieldTable attrMap(const std::string& key) const;

    private:
        Broker* broker;
        SchemaClass* schema;
        uint64_t timestamp;
        Severity severity;
        Object::AttributeMap attributes;
    };

    QPID_CONSOLE_EXTERN std::ostream& operator<<(std::ostream& o, const Event& event);
}
}


#endif
