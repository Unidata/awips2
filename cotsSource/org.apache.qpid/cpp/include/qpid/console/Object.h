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
#ifndef _QPID_CONSOLE_OBJECT_H_
#define _QPID_CONSOLE_OBJECT_H_

#include "qpid/console/ConsoleImportExport.h"
#include "qpid/console/ObjectId.h"
#include "qpid/framing/Uuid.h"
#include "qpid/framing/FieldTable.h"
#include <boost/shared_ptr.hpp>
#include <map>
#include <set>
#include <vector>

namespace qpid {
namespace framing {
    class Buffer;
}
namespace console {

    class  Broker;
    struct SchemaClass;
    struct SchemaMethod;
    class  ObjectId;
    class  ClassKey;
    class  Value;

    /**
     * \ingroup qmfconsoleapi
     */
    struct MethodResponse {
        uint32_t code;
        std::string text;
        std::map<std::string, boost::shared_ptr<Value> > arguments;
    };

    class Object {
    public:
        typedef std::vector<Object> Vector;
        struct AttributeMap : public std::map<std::string, boost::shared_ptr<Value> > {
            QPID_CONSOLE_EXTERN void addRef(const std::string& key, const ObjectId& val);
            QPID_CONSOLE_EXTERN void addUint(const std::string& key, uint32_t val);
            QPID_CONSOLE_EXTERN void addInt(const std::string& key, int32_t val);
            QPID_CONSOLE_EXTERN void addUint64(const std::string& key, uint64_t val);
            QPID_CONSOLE_EXTERN void addInt64(const std::string& key, int64_t val);
            QPID_CONSOLE_EXTERN void addString(const std::string& key, const std::string& val);
            QPID_CONSOLE_EXTERN void addBool(const std::string& key, bool val);
            QPID_CONSOLE_EXTERN void addFloat(const std::string& key, float val);
            QPID_CONSOLE_EXTERN void addDouble(const std::string& key, double val);
            QPID_CONSOLE_EXTERN void addUuid(const std::string& key, const framing::Uuid& val);
            QPID_CONSOLE_EXTERN void addMap(const std::string& key, const framing::FieldTable& val);
        };

        QPID_CONSOLE_EXTERN Object(Broker* broker, SchemaClass* schemaClass, framing::Buffer& buffer, bool prop, bool stat);
        QPID_CONSOLE_EXTERN ~Object();

        Broker* getBroker() const { return broker; }
        const ObjectId& getObjectId() const { return objectId; }
        QPID_CONSOLE_EXTERN const ClassKey& getClassKey() const;
        SchemaClass* getSchema() const { return schema; }
        uint64_t getCurrentTime() const { return currentTime; }
        uint64_t getCreateTime() const { return createTime; }
        uint64_t getDeleteTime() const { return deleteTime; }
        bool isDeleted() const { return deleteTime != 0; }
        QPID_CONSOLE_EXTERN std::string getIndex() const;
        QPID_CONSOLE_EXTERN void mergeUpdate(const Object& updated);
        const AttributeMap& getAttributes() const { return attributes; }
        QPID_CONSOLE_EXTERN void invokeMethod(const std::string name,
                                              const AttributeMap& args,
                                              MethodResponse& result);
        QPID_CONSOLE_EXTERN void handleMethodResp(framing::Buffer& buffer,
                                                  uint32_t sequence);

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
        ObjectId objectId;
        uint64_t currentTime;
        uint64_t createTime;
        uint64_t deleteTime;
        AttributeMap attributes;
        SchemaMethod* pendingMethod;
        MethodResponse methodResponse;

        void parsePresenceMasks(framing::Buffer& buffer, std::set<std::string>& excludeList);
    };

    QPID_CONSOLE_EXTERN std::ostream& operator<<(std::ostream& o, const Object& object);
}
}


#endif
