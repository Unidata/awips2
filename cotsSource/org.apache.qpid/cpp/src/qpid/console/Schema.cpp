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

#include "qpid/console/Schema.h"
#include "qpid/console/Value.h"
#include "qpid/framing/FieldTable.h"

using namespace qpid::console;
using namespace qpid;
using std::string;
using std::vector;

SchemaArgument::SchemaArgument(framing::Buffer& buffer, bool forMethod)
{
    framing::FieldTable map;
    map.decode(buffer);

    name = map.getAsString("name");
    typeCode = map.getAsInt("type");
    unit = map.getAsString("unit");
    min = map.getAsInt("min");
    max = map.getAsInt("max");
    maxLen = map.getAsInt("maxlen");
    desc = map.getAsString("desc");

    dirInput = false;
    dirOutput = false;
    if (forMethod) {
        string dir(map.getAsString("dir"));
        if (dir.find('I') != dir.npos || dir.find('i') != dir.npos)
            dirInput = true;
        if (dir.find('O') != dir.npos || dir.find('o') != dir.npos)
            dirOutput = true;
    }
}

Value::Ptr SchemaArgument::decodeValue(framing::Buffer& buffer)
{
    return ValueFactory::newValue(typeCode, buffer);
}

SchemaProperty::SchemaProperty(framing::Buffer& buffer)
{
    framing::FieldTable map;
    map.decode(buffer);

    name = map.getAsString("name");
    typeCode = map.getAsInt("type");
    accessCode = map.getAsInt("access");
    isIndex = map.getAsInt("index") != 0;
    isOptional = map.getAsInt("optional") != 0;
    unit = map.getAsString("unit");
    min = map.getAsInt("min");
    max = map.getAsInt("max");
    maxLen = map.getAsInt("maxlen");
    desc = map.getAsString("desc");
}

Value::Ptr SchemaProperty::decodeValue(framing::Buffer& buffer)
{
    return ValueFactory::newValue(typeCode, buffer);
}

SchemaStatistic::SchemaStatistic(framing::Buffer& buffer)
{
    framing::FieldTable map;
    map.decode(buffer);

    name = map.getAsString("name");
    typeCode = map.getAsInt("type");
    unit = map.getAsString("unit");
    desc = map.getAsString("desc");
}

Value::Ptr SchemaStatistic::decodeValue(framing::Buffer& buffer)
{
    return ValueFactory::newValue(typeCode, buffer);
}

SchemaMethod::SchemaMethod(framing::Buffer& buffer)
{
    framing::FieldTable map;
    map.decode(buffer);

    name = map.getAsString("name");
    desc = map.getAsString("desc");
    int argCount = map.getAsInt("argCount");

    for (int i = 0; i < argCount; i++)
        arguments.push_back(new SchemaArgument(buffer, true));
}

SchemaMethod::~SchemaMethod()
{
    for (vector<SchemaArgument*>::iterator iter = arguments.begin();
         iter != arguments.end(); iter++)
        delete *iter;
}

SchemaClass::SchemaClass(const uint8_t _kind, const ClassKey& _key, framing::Buffer& buffer) :
    kind(_kind), key(_key)
{
    if (kind == KIND_TABLE) {
        uint8_t  hasSupertype = 0; //buffer.getOctet();
        uint16_t propCount    = buffer.getShort();
        uint16_t statCount    = buffer.getShort();
        uint16_t methodCount  = buffer.getShort();

        if (hasSupertype) {
            string unused;
            buffer.getShortString(unused);
            buffer.getShortString(unused);
            buffer.getLongLong();
            buffer.getLongLong();
        }

        for (uint16_t idx = 0; idx < propCount; idx++)
            properties.push_back(new SchemaProperty(buffer));
        for (uint16_t idx = 0; idx < statCount; idx++)
            statistics.push_back(new SchemaStatistic(buffer));
        for (uint16_t idx = 0; idx < methodCount; idx++)
            methods.push_back(new SchemaMethod(buffer));

    } else if (kind == KIND_EVENT) {
        uint16_t argCount = buffer.getShort();

        for (uint16_t idx = 0; idx < argCount; idx++)
            arguments.push_back(new SchemaArgument(buffer));
    }
}

SchemaClass::~SchemaClass()
{
    for (vector<SchemaProperty*>::iterator iter = properties.begin();
         iter != properties.end(); iter++)
        delete *iter;
    for (vector<SchemaStatistic*>::iterator iter = statistics.begin();
         iter != statistics.end(); iter++)
        delete *iter;
    for (vector<SchemaMethod*>::iterator iter = methods.begin();
         iter != methods.end(); iter++)
        delete *iter;
    for (vector<SchemaArgument*>::iterator iter = arguments.begin();
         iter != arguments.end(); iter++)
        delete *iter;
}

