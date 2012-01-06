#ifndef _QmfEngineValue_
#define _QmfEngineValue_

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
#include <qmf/engine/Typecode.h>

namespace qmf {
namespace engine {

    class Object;
    struct ValueImpl;

    class Value {
    public:
        //        Value();
        Value(const Value& from);
        Value(Typecode t, Typecode arrayType = TYPE_UINT8);
        ~Value();

        Typecode getType() const;
        bool isNull() const;
        void setNull();

        bool isObjectId() const;
        const ObjectId& asObjectId() const;
        void setObjectId(const ObjectId& oid);

        bool isUint() const;
        uint32_t asUint() const;
        void setUint(uint32_t val);

        bool isInt() const;
        int32_t asInt() const;
        void setInt(int32_t val);

        bool isUint64() const;
        uint64_t asUint64() const;
        void setUint64(uint64_t val);

        bool isInt64() const;
        int64_t asInt64() const;
        void setInt64(int64_t val);

        bool isString() const;
        const char* asString() const;
        void setString(const char* val);

        bool isBool() const;
        bool asBool() const;
        void setBool(bool val);

        bool isFloat() const;
        float asFloat() const;
        void setFloat(float val);

        bool isDouble() const;
        double asDouble() const;
        void setDouble(double val);

        bool isUuid() const;
        const uint8_t* asUuid() const;
        void setUuid(const uint8_t* val);

        bool isObject() const;
        const Object* asObject() const;
        void setObject(Object* val);

        bool isMap() const;
        bool keyInMap(const char* key) const;
        Value* byKey(const char* key);
        const Value* byKey(const char* key) const;
        void deleteKey(const char* key);
        void insert(const char* key, Value* val);
        uint32_t keyCount() const;
        const char* key(uint32_t idx) const;

        bool isList() const;
        uint32_t listItemCount() const;
        Value* listItem(uint32_t idx);
        void appendToList(Value* val);
        void deleteListItem(uint32_t idx);

        bool isArray() const;
        Typecode arrayType() const;
        uint32_t arrayItemCount() const;
        Value* arrayItem(uint32_t idx);
        void appendToArray(Value* val);
        void deleteArrayItem(uint32_t idx);

    private:
        friend struct ValueImpl;
        friend class  BrokerProxyImpl;
        friend struct ObjectImpl;
        friend class  AgentImpl;
        Value(ValueImpl* impl);
        ValueImpl* impl;
    };
}
}

#endif

