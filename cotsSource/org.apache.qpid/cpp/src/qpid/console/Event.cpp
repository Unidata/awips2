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

#include "qpid/console/Broker.h"
#include "qpid/console/ClassKey.h"
#include "qpid/console/Schema.h"
#include "qpid/console/Event.h"
#include "qpid/console/Value.h"
#include "qpid/sys/Time.h"
#include "qpid/framing/Buffer.h"

using namespace qpid::console;
using namespace std;
using qpid::framing::Uuid;
using qpid::framing::FieldTable;

Event::Event(Broker* _broker, SchemaClass* _schema, qpid::framing::Buffer& buffer) :
    broker(_broker), schema(_schema)
{
    timestamp = buffer.getLongLong();
    severity = (Severity) buffer.getOctet();
    for (vector<SchemaArgument*>::const_iterator aIter = schema->arguments.begin();
         aIter != schema->arguments.end(); aIter++) {
        SchemaArgument* argument = *aIter;
        attributes[argument->name] = argument->decodeValue(buffer);
    }
}

const ClassKey& Event::getClassKey() const
{
    return schema->getClassKey();
}

string Event::getSeverityString() const
{
    switch (severity) {
    case SEV_EMERGENCY : return string("EMER");
    case SEV_ALERT     : return string("ALERT");
    case SEV_CRITICAL  : return string("CRIT");
    case SEV_ERROR     : return string("ERROR");
    case SEV_WARNING   : return string("WARN");
    case SEV_NOTICE    : return string("NOTIC");
    case SEV_INFO      : return string("INFO");
    case SEV_DEBUG     : return string("DEBUG");
    }
    return string("<UNKNOWN>");
}

ObjectId Event::attrRef(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return ObjectId();
    Value::Ptr val = iter->second;
    if (!val->isObjectId())
        return ObjectId();
    return val->asObjectId();
}

uint32_t Event::attrUint(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isUint())
        return 0;
    return val->asUint();
}

int32_t Event::attrInt(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isInt())
        return 0;
    return val->asInt();
}

uint64_t Event::attrUint64(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isUint64())
        return 0;
    return val->asUint64();
}

int64_t Event::attrInt64(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0;
    Value::Ptr val = iter->second;
    if (!val->isInt64())
        return 0;
    return val->asInt64();
}

string Event::attrString(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return string();
    Value::Ptr val = iter->second;
    if (!val->isString())
        return string();
    return val->asString();
}

bool Event::attrBool(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return false;
    Value::Ptr val = iter->second;
    if (!val->isBool())
        return false;
    return val->asBool();
}

float Event::attrFloat(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0.0;
    Value::Ptr val = iter->second;
    if (!val->isFloat())
        return 0.0;
    return val->asFloat();
}

double Event::attrDouble(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return 0.0;
    Value::Ptr val = iter->second;
    if (!val->isDouble())
        return 0.0;
    return val->asDouble();
}

Uuid Event::attrUuid(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return Uuid();
    Value::Ptr val = iter->second;
    if (!val->isUuid())
        return Uuid();
    return val->asUuid();
}

FieldTable Event::attrMap(const string& key) const
{
    Object::AttributeMap::const_iterator iter = attributes.find(key);
    if (iter == attributes.end())
        return FieldTable();
    Value::Ptr val = iter->second;
    if (!val->isMap())
        return FieldTable();
    return val->asMap();
}


std::ostream& qpid::console::operator<<(std::ostream& o, const Event& event)
{
    const ClassKey& key = event.getClassKey();
    sys::AbsTime aTime(sys::AbsTime(), sys::Duration(event.getTimestamp()));
    o << aTime << " " << event.getSeverityString() << " " <<
        key.getPackageName() << ":" << key.getClassName() <<
        " broker=" << event.getBroker()->getUrl();

    const Object::AttributeMap& attributes = event.getAttributes();
    for (Object::AttributeMap::const_iterator iter = attributes.begin();
         iter != attributes.end(); iter++) {
        o << " " << iter->first << "=" << iter->second->str();
    }
    return o;
}


