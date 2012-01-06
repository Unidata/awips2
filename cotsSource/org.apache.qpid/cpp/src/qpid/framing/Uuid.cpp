/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/framing/Uuid.h"

#include "qpid/sys/uuid.h"
#include "qpid/Exception.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/reply_exceptions.h"

namespace qpid {
namespace framing {

using namespace std;

static const size_t UNPARSED_SIZE=36; 

Uuid::Uuid(bool unique) {
    if (unique) {
        generate();
    } else {
        clear();
    }
}

Uuid::Uuid(const uint8_t* data) {
    assign(data);
}

void Uuid::assign(const uint8_t* data) {
    // This const cast is for Solaris which has a 
    // uuid_copy that takes a non const 2nd argument
    uuid_copy(c_array(), const_cast<uint8_t*>(data));
}

void Uuid::generate() {
    uuid_generate(c_array());
}

void Uuid::clear() {
    uuid_clear(c_array());
}

// Force int 0/!0 to false/true; avoids compile warnings.
bool Uuid::isNull() const {
    return !!uuid_is_null(data());
}

void Uuid::encode(Buffer& buf) const {
    buf.putRawData(data(), size());
}

void Uuid::decode(Buffer& buf) {
    if (buf.available() < size())
        throw IllegalArgumentException(QPID_MSG("Not enough data for UUID."));
    buf.getRawData(c_array(), size());
}

ostream& operator<<(ostream& out, Uuid uuid) {
    char unparsed[UNPARSED_SIZE + 1];
    uuid_unparse(uuid.data(), unparsed);
    return out << unparsed;
}

istream& operator>>(istream& in, Uuid& uuid) {
    char unparsed[UNPARSED_SIZE + 1] = {0};
    in.get(unparsed, sizeof(unparsed));
    if (uuid_parse(unparsed, uuid.c_array()) != 0) 
        in.setstate(ios::failbit);
    return in;
}

std::string Uuid::str() const {
    std::ostringstream os;
    os << *this;
    return os.str();
}

}} // namespace qpid::framing
