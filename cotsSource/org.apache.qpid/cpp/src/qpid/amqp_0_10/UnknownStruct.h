#ifndef QPID_AMQP_0_10_UNKNOWNSTRUCT_H
#define QPID_AMQP_0_10_UNKNOWNSTRUCT_H

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
#include "qpid/amqp_0_10/Struct.h"
#include <string>

namespace qpid {
namespace amqp_0_10 {

class UnknownStruct : public Struct {
  public:
    static const uint8_t SIZE=4;
    static const uint8_t PACK=2;

    template <class S> void serialize(S& s) { s.split(*this); s(data.begin(), data.end()); }
    template <class S> void encode(S&) const { }
    template <class S> void decode(S& s) { data.resize(s.bytesRemaining()); }

    UnknownStruct(uint8_t cc=0, uint8_t c=0) : classCode(cc), code(c) {}
    void accept(Visitor&);
    void accept(ConstVisitor&) const;

    uint8_t getClassCode() const { return classCode; }
    uint8_t getCode() const { return code; }
    
  private:
    uint8_t classCode, code;
    std::string data;
};

std::ostream& operator<<(std::ostream&, const UnknownStruct&);

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_UNKNOWNSTRUCT_H*/
