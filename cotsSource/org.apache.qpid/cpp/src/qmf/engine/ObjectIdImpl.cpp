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

#include "qmf/engine/ObjectIdImpl.h"
#include <stdlib.h>

using namespace std;
using namespace qmf::engine;
using qpid::framing::Buffer;

void AgentAttachment::setBanks(uint32_t broker, uint32_t agent)
{
    first =
        ((uint64_t) (broker & 0x000fffff)) << 28 |
        ((uint64_t) (agent  & 0x0fffffff));
}

ObjectIdImpl::ObjectIdImpl(Buffer& buffer) : agent(0)
{
    decode(buffer);
}

ObjectIdImpl::ObjectIdImpl(AgentAttachment* a, uint8_t flags, uint16_t seq, uint64_t object) : agent(a)
{
    first =
        ((uint64_t) (flags &   0x0f)) << 60 |
        ((uint64_t) (seq   & 0x0fff)) << 48;
    second = object;
}

ObjectId* ObjectIdImpl::factory(Buffer& buffer)
{
    ObjectIdImpl* impl(new ObjectIdImpl(buffer));
    return new ObjectId(impl);
}

ObjectId* ObjectIdImpl::factory(AgentAttachment* agent, uint8_t flags, uint16_t seq, uint64_t object)
{
    ObjectIdImpl* impl(new ObjectIdImpl(agent, flags, seq, object));
    return new ObjectId(impl);
}

void ObjectIdImpl::decode(Buffer& buffer)
{
    first = buffer.getLongLong();
    second = buffer.getLongLong();
}

void ObjectIdImpl::encode(Buffer& buffer) const
{
    if (agent == 0)
        buffer.putLongLong(first);
    else
        buffer.putLongLong(first | agent->first);
    buffer.putLongLong(second);
}

void ObjectIdImpl::fromString(const std::string& repr)
{
#define FIELDS 5
#if defined (_WIN32) && !defined (atoll)
#  define atoll(X) _atoi64(X)
#endif

    std::string copy(repr.c_str());
    char* cText;
    char* field[FIELDS];
    bool  atFieldStart = true;
    int   idx = 0;

    cText = const_cast<char*>(copy.c_str());
    for (char* cursor = cText; *cursor; cursor++) {
        if (atFieldStart) {
            if (idx >= FIELDS)
                return; // TODO error
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
        return;  // TODO error

    first = (atoll(field[0]) << 60) +
        (atoll(field[1]) << 48) +
        (atoll(field[2]) << 28) +
        atoll(field[3]);
    second = atoll(field[4]);
    agent = 0;
}

const string& ObjectIdImpl::asString() const
{
    stringstream val;

    val << (int) getFlags() << "-" << getSequence() << "-" << getBrokerBank() << "-" <<
        getAgentBank() << "-" << getObjectNum();
    repr = val.str();
    return repr;
}

#define ACTUAL_FIRST (agent == 0 ? first : first | agent->first)
#define ACTUAL_OTHER (other.agent == 0 ? other.first : other.first | other.agent->first)

uint8_t ObjectIdImpl::getFlags() const
{
    return (ACTUAL_FIRST & 0xF000000000000000LL) >> 60;
}

uint16_t ObjectIdImpl::getSequence() const
{
    return (ACTUAL_FIRST & 0x0FFF000000000000LL) >> 48;
}

uint32_t ObjectIdImpl::getBrokerBank() const
{
    return (ACTUAL_FIRST & 0x0000FFFFF0000000LL) >> 28;
}

uint32_t ObjectIdImpl::getAgentBank() const
{
    return ACTUAL_FIRST & 0x000000000FFFFFFFLL;
}

uint64_t ObjectIdImpl::getObjectNum() const
{
    return second;
}

uint32_t ObjectIdImpl::getObjectNumHi() const
{
    return (uint32_t) (second >> 32);
}

uint32_t ObjectIdImpl::getObjectNumLo() const
{
    return (uint32_t) (second & 0x00000000FFFFFFFFLL);
}

bool ObjectIdImpl::operator==(const ObjectIdImpl& other) const
{
    return ACTUAL_FIRST == ACTUAL_OTHER && second == other.second;
}

bool ObjectIdImpl::operator<(const ObjectIdImpl& other) const
{
    return (ACTUAL_FIRST < ACTUAL_OTHER) || ((ACTUAL_FIRST == ACTUAL_OTHER) && (second < other.second));
}

bool ObjectIdImpl::operator>(const ObjectIdImpl& other) const
{
    return (ACTUAL_FIRST > ACTUAL_OTHER) || ((ACTUAL_FIRST == ACTUAL_OTHER) && (second > other.second));
}


//==================================================================
// Wrappers
//==================================================================

ObjectId::ObjectId() : impl(new ObjectIdImpl()) {}
ObjectId::ObjectId(const ObjectId& from) : impl(new ObjectIdImpl(*(from.impl))) {}
ObjectId::ObjectId(ObjectIdImpl* i) : impl(i) {}
ObjectId::~ObjectId() { delete impl; }
uint64_t ObjectId::getObjectNum() const { return impl->getObjectNum(); }
uint32_t ObjectId::getObjectNumHi() const { return impl->getObjectNumHi(); }
uint32_t ObjectId::getObjectNumLo() const { return impl->getObjectNumLo(); }
bool ObjectId::isDurable() const { return impl->isDurable(); }
const char* ObjectId::str() const { return impl->asString().c_str(); }
uint8_t ObjectId::getFlags() const { return impl->getFlags(); }
uint16_t ObjectId::getSequence() const { return impl->getSequence(); }
uint32_t ObjectId::getBrokerBank() const { return impl->getBrokerBank(); }
uint32_t ObjectId::getAgentBank() const { return impl->getAgentBank(); }
bool ObjectId::operator==(const ObjectId& other) const { return *impl == *other.impl; }
bool ObjectId::operator<(const ObjectId& other) const { return *impl < *other.impl; }
bool ObjectId::operator>(const ObjectId& other) const { return *impl > *other.impl; }
bool ObjectId::operator<=(const ObjectId& other) const { return !(*impl > *other.impl); }
bool ObjectId::operator>=(const ObjectId& other) const { return !(*impl < *other.impl); }

