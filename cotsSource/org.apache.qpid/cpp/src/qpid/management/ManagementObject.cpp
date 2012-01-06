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
 
#include "qpid/management/Manageable.h"
#include "qpid/management/ManagementObject.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/sys/Thread.h"

#include <stdlib.h>

using namespace qpid;
using namespace qpid::management;

void AgentAttachment::setBanks(uint32_t broker, uint32_t bank)
{
    first =
        ((uint64_t) (broker & 0x000fffff)) << 28 |
        ((uint64_t) (bank   & 0x0fffffff));
}

ObjectId::ObjectId(uint8_t flags, uint16_t seq, uint32_t broker, uint32_t bank, uint64_t object)
    : agent(0)
{
    first =
        ((uint64_t) (flags  &       0x0f)) << 60 |
        ((uint64_t) (seq    &     0x0fff)) << 48 |
        ((uint64_t) (broker & 0x000fffff)) << 28 |
        ((uint64_t) (bank   & 0x0fffffff));
    second = object;
}

ObjectId::ObjectId(AgentAttachment* _agent, uint8_t flags, uint16_t seq, uint64_t object)
    : agent(_agent)
{
    first =
        ((uint64_t) (flags &   0x0f)) << 60 |
        ((uint64_t) (seq   & 0x0fff)) << 48;
    second = object;
}

ObjectId::ObjectId(std::istream& in) : agent(0)
{
    std::string text;
    in >> text;
    fromString(text);
}

ObjectId::ObjectId(const std::string& text) : agent(0)
{
    fromString(text);
}

void ObjectId::fromString(const std::string& text)
{
#define FIELDS 5
#if defined (_WIN32) && !defined (atoll)
#  define atoll(X) _atoi64(X)
#endif

    std::string copy(text.c_str());
    char* cText;
    char* field[FIELDS];
    bool  atFieldStart = true;
    int   idx = 0;

    cText = const_cast<char*>(copy.c_str());
    for (char* cursor = cText; *cursor; cursor++) {
        if (atFieldStart) {
            if (idx >= FIELDS)
                throw Exception("Invalid ObjectId format");
            field[idx++] = cursor;
            atFieldStart = false;
        } else {
            if (*cursor == '-') {
                *cursor = '\0';
                atFieldStart = true;
            }
        }
    }

    if (idx != FIELDS)
        throw Exception("Invalid ObjectId format");

    first = (atoll(field[0]) << 60) +
        (atoll(field[1]) << 48) +
        (atoll(field[2]) << 28) +
        atoll(field[3]);
    second = atoll(field[4]);
}


bool ObjectId::operator==(const ObjectId &other) const
{
    uint64_t otherFirst = agent == 0 ? other.first : other.first & 0xffff000000000000LL;

    return first == otherFirst && second == other.second;
}

bool ObjectId::operator<(const ObjectId &other) const
{
    uint64_t otherFirst = agent == 0 ? other.first : other.first & 0xffff000000000000LL;

    return (first < otherFirst) || ((first == otherFirst) && (second < other.second));
}

void ObjectId::encode(framing::Buffer& buffer)
{
    if (agent == 0)
        buffer.putLongLong(first);
    else
        buffer.putLongLong(first | agent->first);
    buffer.putLongLong(second);
}

void ObjectId::decode(framing::Buffer& buffer)
{
    first  = buffer.getLongLong();
    second = buffer.getLongLong();
}

namespace qpid {
namespace management {

std::ostream& operator<<(std::ostream& out, const ObjectId& i)
{
    uint64_t virtFirst = i.first;
    if (i.agent)
        virtFirst |= i.agent->getFirst();

    out << ((virtFirst & 0xF000000000000000LL) >> 60) <<
        "-" << ((virtFirst & 0x0FFF000000000000LL) >> 48) <<
        "-" << ((virtFirst & 0x0000FFFFF0000000LL) >> 28) <<
        "-" <<  (virtFirst & 0x000000000FFFFFFFLL) <<
        "-" << i.second;
    return out;
}

}}

int ManagementObject::maxThreads = 1;
int ManagementObject::nextThreadIndex = 0;

void ManagementObject::writeTimestamps (framing::Buffer& buf)
{
    buf.putShortString (getPackageName ());
    buf.putShortString (getClassName ());
    buf.putBin128      (getMd5Sum ());
    buf.putLongLong    (updateTime);
    buf.putLongLong    (createTime);
    buf.putLongLong    (destroyTime);
    objectId.encode(buf);
}

void ManagementObject::setReference(ObjectId) {}

int ManagementObject::getThreadIndex() {
    static QPID_TSS int thisIndex = -1;
    if (thisIndex == -1) {
        sys::Mutex::ScopedLock mutex(accessLock);
        thisIndex = nextThreadIndex;
        if (nextThreadIndex < maxThreads - 1)
            nextThreadIndex++;
    }
    return thisIndex;
}
