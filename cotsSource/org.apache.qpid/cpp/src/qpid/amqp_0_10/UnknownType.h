#ifndef QPID_AMQP_0_10_UNKNOWNTYPE_H
#define QPID_AMQP_0_10_UNKNOWNTYPE_H

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
#include "qpid/sys/IntegerTypes.h"
#include <vector>
#include <iosfwd>

namespace qpid {
namespace amqp_0_10 {

/** Encode/decode an unknown type based on typecode. */
class UnknownType {
  public:
    UnknownType(uint8_t code=0);
    uint8_t getCode() const { return code; }
    /** Size of fixed type or 0 if not fixed/0-length. -1 invalid */
    int fixed() const;
    /** Bytes in size type for variable width. -1 invalid */
    int variable() const;

    typedef std::vector<char>::const_iterator const_iterator;
    const_iterator begin() const { return data.begin(); }
    const_iterator end() const { return data.end(); }
    size_t size() const { return data.size(); }

    template <class S> void serialize(S& s) { s.split(*this); }
    template <class S> void encode(S& s) const;
    template <class S> void decode(S& s);

  private:
    uint8_t code;
    struct Width { int fixed; int variable; };
    static Width WidthTable[16];

    std::vector<char> data;
};

template <class S> void UnknownType::encode(S& s) const {
    switch (variable()) {
      case 0: break;
      case 1: s(uint8_t(data.size())); break;
      case 2: s(uint16_t(data.size())); break;
      case 4: s(uint32_t(data.size())); break;
    }
    s(data.begin(), data.end());
}

template <class S> void UnknownType::decode(S& s) {
    uint32_t s8;
    uint32_t s16;
    uint32_t s32;
    switch (variable()) {
      case 0: break;
      case 1: s(s8); data.resize(s8); break;
      case 2: s(s16); data.resize(s16); break;
      case 4: s(s32); data.resize(s32); break;
    }
    s(data.begin(), data.end());
}

inline uint8_t codeFor(const UnknownType& u) { return u.getCode(); }

std::ostream& operator<<(std::ostream&, const UnknownType&);

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_UNKNOWNTYPE_H*/
