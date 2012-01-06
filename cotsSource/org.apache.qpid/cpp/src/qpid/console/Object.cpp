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

#include "qpid/console/SessionManager.h"
#include "qpid/console/Broker.h"
#include "qpid/console/Object.h"
#include "qpid/console/Schema.h"
#include "qpid/console/ClassKey.h"
#include "qpid/console/Value.h"
#include "qpid/framing/Buffer.h"
#include "qpid/sys/Mutex.h"

using namespace qpid::console;
using namespace qpid::sys;
using namespace qpid;
using namespace std;
using qpid::framing::Uuid;
using qpid::framing::FieldTable;

void Object::AttributeMap::addRef(const string& key, const ObjectId& val)
{
    (*this)[key] = Value::Ptr(new RefValue(val));
}

void Object::AttributeMap::addUint(const string& key, uint32_t val)
{
    (*this)[key] = Value::Ptr(new UintValue(val));
}

void Object::AttributeMap::addInt(const string& key, int32_t val)
{
    (*this)[key] = Value::Ptr(new IntValue(val));
}

void Object::AttributeMap::addUint64(const string& key, uint64_t val)
{
    (*this)[key] = Value::Ptr(new Uint64Value(val));
}

void Object::AttributeMap::addInt64(const string& key, int64_t val)
{
    (*this)[key] = Value::Ptr(new Int64Value(val));
}

void Object::AttributeMap::addString(const string& key, const string& val)
{
    (*this)[key] = Value::Ptr(new StringValue(val));
}

void Object::AttributeMap::addBool(const string& key, bool val)
{
    (*this)[key] = Value::Ptr(new BoolValue(val));
}

void Object::AttributeMap::addFloat(const string& key, float val)
{
    (*this)[key] = Value::Ptr(new FloatValue(val));
}

void Object::AttributeMap::addDouble(const string& key, double val)
{
    (*this)[key] = Value::Ptr(new DoubleValue(val));
}

void Object::AttributeMap::addUuid(const string& key, const Uuid& val)
{
    (*this)[key] = Value::Ptr(new UuidValue(val));
}

void Object::AttributeMap::addMap(const string& key, const FieldTable& val)
{
    (*this)[key] = Value::Ptr(new MapValue(val));
}

Object::Object(Broker* b, SchemaClass* s, framing::Buffer& buffer, bool prop, bool stat) :
    broker(b), schema(s), pendingMethod(0)
{
    currentTime = buffer.getLongLong();
    createTime = buffer.getLongLong();
    deleteTime = buffer.getLongLong();
    objectId.decode(buffer);

    if (prop) {
        set<string> excludes;
        parsePresenceMasks(buffer, excludes);
        for (vector<SchemaProperty*>::const_iterator pIter = schema->properties.begin();
             pIter != schema->properties.end(); pIter++) {
            SchemaProperty* property = *pIter;
            if (excludes.count(property->name) != 0) {
                attributes[property->name] = Value::Ptr(new NullValue());
            } else {
                attributes[property->name] = property->decodeValue(buffer);
            }
        }
    }

    if (stat) {
        for (vector<SchemaStatistic*>::const_iterator sIter = schema->statistics.begin();
             sIter != schema->statistics.end(); sIter++) {
            SchemaStatistic* statistic = *sIter;
            attributes[statistic->name] = statistic->decodeValue(buffer);
        }
    }
}

Object::~Object() {}

const ClassKey& Object::getClassKey() const
{
    return schema->getClassKey();
}

string Object::getIndex() const
{
    string result;

    for (vector<SchemaProperty*>::const_iterator pIter = schema->properties.begin();
         pIter != schema->properties.end(); pIter++) {
        SchemaProperty* property = *pIter;
        if (property->isIndex) {
            AttributeMap::const_iterator vIter = attributes.find(property->name);
            if (vIter != attributes.end()) {
                if (!result.empty())
                    result += ":";
                result += vIter->second->str();
            }
        }
    }
    return result;
}

void Object::mergeUpdate(const Object& /*updated*/)
{
    // TODO
}

void Object::invokeMethod(const string name, const AttributeMap& args, MethodResponse& result)
{
    for (vector<SchemaMethod*>::const_iterator iter = schema->methods.begin();
         iter != schema->methods.end(); iter++) {
        if ((*iter)->name == name) {
            SchemaMethod* method = *iter;
            char    rawbuffer[65536];
            framing::Buffer  buffer(rawbuffer, 65536);
            uint32_t sequence = broker->sessionManager.sequenceManager.reserve("method");
            pendingMethod = method;
            broker->methodObject = this;
            broker->encodeHeader(buffer, 'M', sequence);
            objectId.encode(buffer);
            schema->key.encode(buffer);
            buffer.putShortString(name);

            for (vector<SchemaArgument*>::const_iterator aIter = method->arguments.begin();
                 aIter != method->arguments.end(); aIter++) {
                SchemaArgument* arg = *aIter;
                if (arg->dirInput) {
                    AttributeMap::const_iterator attr = args.find(arg->name);
                    if (attr != args.end()) {
                        ValueFactory::encodeValue(arg->typeCode, attr->second, buffer);
                    } else {
                        // TODO Use the default value instead of throwing
                        throw Exception("Missing arguments in method call");
                    }
                }
            }

            uint32_t length = buffer.getPosition();
            buffer.reset();
            stringstream routingKey;
            routingKey << "agent." << objectId.getBrokerBank() << "." << objectId.getAgentBank();
            broker->connThreadBody.sendBuffer(buffer, length, "qpid.management", routingKey.str());

            {
                Mutex::ScopedLock l(broker->lock);
                bool ok = true;
                while (pendingMethod != 0 && ok) {
                    ok = broker->cond.wait(broker->lock, AbsTime(now(), broker->sessionManager.settings.methodTimeout * TIME_SEC));
                }

                if (!ok) {
                    result.code = 0x1001;
                    result.text.assign("Method call timed out");
                    result.arguments.clear();
                } else {
                    result = methodResponse;
                }
            }
        }
    }
}

void Object::handleMethodResp(framing::Buffer& buffer, uint32_t sequence)
{
    broker->sessionManager.sequenceManager.release(sequence);
    methodResponse.code = buffer.getLong();
    buffer.getMediumString(methodResponse.text);
    methodResponse.arguments.clear();

    for (vector<SchemaArgument*>::const_iterator aIter = pendingMethod->arguments.begin();
         aIter != pendingMethod->arguments.end(); aIter++) {
        SchemaArgument* arg = *aIter;
        if (arg->dirOutput) {
            methodResponse.arguments[arg->name] = arg->decodeValue(buffer);
        }
    }
    
    {
        Mutex::ScopedLock l(broker->lock);
        pendingMethod = 0;
        broker->cond.notify();
    }
}

ObjectId Object::attrRef(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return ObjectId();
    Value::Ptr val = iter->second;
    if (!val->isObjectId())
        return ObjectId();
    return val->asObjectId();
}

uint32_t Object::attrUint(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isUint())
        return 0;
    return val->asUint();
}

int32_t Object::attrInt(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isInt())
        return 0;
    return val->asInt();
}

uint64_t Object::attrUint64(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isUint64())
        return 0;
    return val->asUint64();
}

int64_t Object::attrInt64(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isInt64())
        return 0;
    return val->asInt64();
}

string Object::attrString(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return string();
    Value::Ptr val = iter->second;
    if (!val->isString())
        return string();
    return val->asString();
}

bool Object::attrBool(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return false;
    Value::Ptr val = iter->second;
    if (!val->isBool())
        return false;
    return val->asBool();
}

float Object::attrFloat(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0.0;
    Value::Ptr val = iter->second;
    if (!val->isFloat())
        return 0.0;
    return val->asFloat();
}

double Object::attrDouble(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0.0;
    Value::Ptr val = iter->second;
    if (!val->isDouble())
        return 0.0;
    return val->asDouble();
}

Uuid Object::attrUuid(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return Uuid();
    Value::Ptr val = iter->second;
    if (!val->isUuid())
        return Uuid();
    return val->asUuid();
}

FieldTable Object::attrMap(const string& key) const
{
    AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return FieldTable();
    Value::Ptr val = iter->second;
    if (!val->isMap())
        return FieldTable();
    return val->asMap();
}

void Object::parsePresenceMasks(framing::Buffer& buffer, set<string>& excludeList)
{
    excludeList.clear();
    uint8_t bit = 0;
    uint8_t mask = 0;

    for (vector<SchemaProperty*>::const_iterator pIter = schema->properties.begin();
         pIter != schema->properties.end(); pIter++) {
        SchemaProperty* property = *pIter;
        if (property->isOptional) {
            if (bit == 0) {
                mask = buffer.getOctet();
                bit = 1;
            }
            if ((mask & bit) == 0)
                excludeList.insert(property->name);
            if (bit == 0x80)
                bit = 0;
            else
                bit = bit << 1;
        }
    }
}

ostream& qpid::console::operator<<(ostream& o, const Object& object)
{
    const ClassKey& key = object.getClassKey();
    o << key.getPackageName() << ":" << key.getClassName() << "[" << object.getObjectId() << "] " <<
        object.getIndex();
    return o;
}

