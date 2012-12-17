#ifndef QPID_FRAMING_UUID_H
#define QPID_FRAMING_UUID_H

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

#include "qpid/CommonImportExport.h"
#include "qpid/sys/IntegerTypes.h"

#include <boost/array.hpp>

#include <ostream>
#include <istream>

namespace qpid {
namespace framing {

class Buffer;

/**
 * A UUID is represented as a boost::array of 16 bytes.
 *
 * Full value semantics, operators ==, < etc.  are provided by
 * boost::array so Uuid can be the key type in a map etc.
 *
 * TODO: change this implementation as it leaks boost into the
 * client API
 */
struct Uuid : public boost::array<uint8_t, 16> {
    /** If unique is true, generate a unique ID else a null ID. */
    QPID_COMMON_EXTERN Uuid(bool unique=false);

    /** Copy from 16 bytes of data. */
    QPID_COMMON_EXTERN Uuid(const uint8_t* data);

    // Default op= and copy ctor are fine.
    // boost::array gives us ==, < etc.

    /** Copy from 16 bytes of data. */
    void assign(const uint8_t* data);

    /** Set to a new unique identifier. */
    QPID_COMMON_EXTERN void generate();

    /** Set to all zeros. */
    void clear();

    /** Test for null (all zeros). */
    bool isNull() const;
    operator bool() const { return !isNull(); }
    bool operator!() const { return isNull(); }

    QPID_COMMON_EXTERN void encode(framing::Buffer& buf) const;
    QPID_COMMON_EXTERN void decode(framing::Buffer& buf);
    QPID_COMMON_EXTERN uint32_t encodedSize() const { return size(); }

    /** String value in format 1b4e28ba-2fa1-11d2-883f-b9a761bde3fb. */
    QPID_COMMON_EXTERN std::string str() const;

    template <class S> void serialize(S& s) {
        s.raw(begin(), size());
    }
};

/** Print in format 1b4e28ba-2fa1-11d2-883f-b9a761bde3fb. */
QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream&, Uuid);

/** Read from format 1b4e28ba-2fa1-11d2-883f-b9a761bde3fb. */
QPID_COMMON_EXTERN std::istream& operator>>(std::istream&, Uuid&);

}} // namespace qpid::framing



#endif  /*!QPID_FRAMING_UUID_H*/
