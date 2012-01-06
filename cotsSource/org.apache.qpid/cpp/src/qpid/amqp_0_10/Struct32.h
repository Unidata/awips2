#ifndef QPID_AMQP_0_10_STRUCT32_H
#define QPID_AMQP_0_10_STRUCT32_H

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

#include "qpid/amqp_0_10/StructHolder.h"

namespace qpid {
namespace amqp_0_10 {

class Struct32 : public StructHolder
{
  public:
    Struct32();

    template <class T> explicit Struct32(const T& t) : StructHolder(t) {}
    
    template <class S> void serialize(S& s) { s.split(*this); }

    using StructHolder::operator=;

    template <class S> void encode(S& s) const {
        s(contentSize());
        const_cast<Struct32*>(this)->StructHolder::serialize(s);
    }
    
    template <class S> void decode(S& s) {
        uint32_t contentSz;
        s(contentSz);
        typename S::ScopedLimit l(s, contentSz);
        StructHolder::serialize(s);
    }
    
  private:
    uint32_t contentSize() const {
        return Codec::size(static_cast<const StructHolder&>(*this));
    }
        
};

std::ostream& operator<<(std::ostream&, const Struct32&);

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_STRUCT32_H*/
