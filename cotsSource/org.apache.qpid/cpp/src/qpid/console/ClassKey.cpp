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

#include "qpid/console/ClassKey.h"
#include <string.h>
#include <cstdio>

using namespace std;
using namespace qpid::console;

ClassKey::ClassKey(const string& _package, const string& _name, const uint8_t* _hash) :
    package(_package), name(_name)
{
    ::memcpy(hash, _hash, HASH_SIZE);
}

string ClassKey::getHashString() const
{
    char cstr[36];
    ::sprintf(cstr, "%02x%02x%02x%02x-%02x%02x%02x%02x-%02x%02x%02x%02x-%02x%02x%02x%02x",
              hash[0], hash[1], hash[2], hash[3], hash[4], hash[5], hash[6], hash[7],
              hash[8], hash[9], hash[10], hash[11], hash[12], hash[13], hash[14], hash[15]);
    return string(cstr);
}

string ClassKey::str() const
{
    string result(package + ":" + name + "(" + getHashString() + ")");
    return result;
}

bool ClassKey::operator==(const ClassKey& other) const
{
    return ::memcmp(hash, other.hash, HASH_SIZE) == 0 &&
        name == other.name &&
        package == other.package;
}

bool ClassKey::operator!=(const ClassKey& other) const
{
    return !(*this == other);
}

bool ClassKey::operator<(const ClassKey& other) const
{
    int cmp = ::memcmp(hash, other.hash, HASH_SIZE);
    if (cmp != 0)
        return cmp < 0;
    cmp = name.compare(other.name);
    if (cmp != 0)
        return cmp < 0;
    return package < other.package;
}

bool ClassKey::operator>(const ClassKey& other) const
{
    int cmp = ::memcmp(hash, other.hash, HASH_SIZE);
    if (cmp != 0)
        return cmp > 0;
    cmp = name.compare(other.name);
    if (cmp != 0)
        return cmp > 0;
    return package > other.package;
}

bool ClassKey::operator<=(const ClassKey& other) const
{
    return !(*this > other);
}

bool ClassKey::operator>=(const ClassKey& other) const
{
    return !(*this < other);
}

void ClassKey::encode(qpid::framing::Buffer& buffer) const
{
    buffer.putShortString(package);
    buffer.putShortString(name);
    buffer.putBin128(const_cast<uint8_t*>(hash));
}

ostream& qpid::console::operator<<(ostream& o, const ClassKey& k)
{
    o << k.str();
    return o;
}
