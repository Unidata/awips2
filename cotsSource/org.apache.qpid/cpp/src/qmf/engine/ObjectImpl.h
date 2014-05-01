#ifndef _QmfEngineObjectImpl_
#define _QmfEngineObjectImpl_

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

#include <qmf/engine/Object.h>
#include <qmf/engine/ObjectIdImpl.h>
#include <map>
#include <set>
#include <string>
#include <qpid/framing/Buffer.h>
#include <boost/shared_ptr.hpp>
#include <qpid/sys/Mutex.h>

namespace qmf {
namespace engine {

    class BrokerProxyImpl;

    typedef boost::shared_ptr<Object> ObjectPtr;

    struct ObjectImpl {
        typedef boost::shared_ptr<Value> ValuePtr;
        const SchemaObjectClass* objectClass;
        BrokerProxyImpl* broker;
        boost::shared_ptr<ObjectId> objectId;
        uint64_t createTime;
        uint64_t destroyTime;
        uint64_t lastUpdatedTime;
        mutable std::map<std::string, ValuePtr> properties;
        mutable std::map<std::string, ValuePtr> statistics;

        ObjectImpl(const SchemaObjectClass* type);
        ObjectImpl(const SchemaObjectClass* type, BrokerProxyImpl* b, qpid::framing::Buffer& buffer,
                   bool prop, bool stat, bool managed);
        static Object* factory(const SchemaObjectClass* type, BrokerProxyImpl* b, qpid::framing::Buffer& buffer,
                               bool prop, bool stat, bool managed);
        ~ObjectImpl();

        void destroy();
        const ObjectId* getObjectId() const { return objectId.get(); }
        void setObjectId(ObjectId* oid) { objectId.reset(new ObjectId(*oid)); }
        const SchemaObjectClass* getClass() const { return objectClass; }
        Value* getValue(const std::string& key) const;
        void invokeMethod(const std::string& methodName, const Value* inArgs, void* context) const;
        bool isDeleted() const { return destroyTime != 0; }
        void merge(const Object& from);

        void parsePresenceMasks(qpid::framing::Buffer& buffer, std::set<std::string>& excludeList);
        void encodeSchemaKey(qpid::framing::Buffer& buffer) const;
        void encodeManagedObjectData(qpid::framing::Buffer& buffer) const;
        void encodeProperties(qpid::framing::Buffer& buffer) const;
        void encodeStatistics(qpid::framing::Buffer& buffer) const;
    };
}
}

#endif

