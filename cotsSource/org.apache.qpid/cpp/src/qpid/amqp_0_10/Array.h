#ifndef QPID_AMQP_0_10_ARRAY_H
#define QPID_AMQP_0_10_ARRAY_H

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

#include "qpid/amqp_0_10/TypeForCode.h"
#include "qpid/amqp_0_10/CodeForType.h"
#include "qpid/amqp_0_10/UnknownType.h"
#include "qpid/amqp_0_10/exceptions.h"
#include "qpid/amqp_0_10/Codec.h"
#include <vector>
#include <ostream>

namespace qpid {
namespace amqp_0_10 {

template <class T> class  ArrayDomain : public std::vector<T>  {
  public:
    template <class S> void serialize(S& s) { s.split(*this); }

    template <class S> void encode(S& s) const {
        s(contentSize())(CodeForType<T>::value)(uint32_t(this->size()));
        s(this->begin(), this->end()); 
    }

    void encode(Codec::Size& s) const  { s.raw(0, contentSize() + 4/*size*/); }
    
    template <class S> void decode(S& s) {
        uint32_t size; uint8_t type; uint32_t count;
        s(size);
        typename S::ScopedLimit l(s, size);
        s(type);
        if (type != CodeForType<T>::value)
            throw InvalidArgumentException(QPID_MSG("Array domain expected type " << CodeForType<T>::value << " but found " << type));
        s(count);
        this->resize(count);
        s(this->begin(), this->end()); 
    }

  private:
    uint32_t contentSize() const {
        return Codec::size(this->begin(), this->end()) + sizeof(uint32_t) /*count*/ + sizeof(uint8_t) /*type*/;
    }
};

template <class T>
std::ostream& operator<<(std::ostream& o, const ArrayDomain<T>& ad) {
    std::ostream_iterator<T> i(o, " ");
    o << "Array<" << typeName(CodeForType<T>::value) << ">[";
    std::copy(ad.begin(), ad.end(), i);
    o << "]";
    return o;
}

/** A non-domain array is represented as and array of UnknownType.
 * Special case templat.
 */ 
template<> class ArrayDomain<UnknownType> : public std::vector<UnknownType> { 
  public:
    ArrayDomain(uint8_t type_=0) : type(type_) {}
    
    template <class S> void serialize(S& s) { s.split(*this); }

    template <class S> void encode(S& s) const {
        s(contentSize())(type)(uint32_t(this->size()));
        s(this->begin(), this->end());        
    }

    void encode(Codec::Size& s) const  { s.raw(0, contentSize() + 4/*size*/); }

    template <class S> void decode(S& s) {
        uint32_t size; uint32_t count;
        s(size);
        typename S::ScopedLimit l(s, size);
        s(type)(count);
        this->clear();
        this->resize(count, UnknownType(type));
        s(this->begin(), this->end());
    }

    uint8_t getType() const { return type; }
    
  private:
    uint32_t contentSize() const {
        return Codec::size(this->begin(), this->end()) + sizeof(uint32_t) /*count*/ + sizeof(uint8_t) /*type*/;
    }
    uint8_t type;
};

std::ostream& operator<<(std::ostream& o, const Array& a);

// FIXME aconway 2008-04-08: hack to supress encoding of
// command-fragments and in-doubt as there is a problem with the spec
// (command-fragments does not have a one byte type code.)
namespace session { class CommandFragment; }
namespace dtx { class Xid; }

template <> struct ArrayDomain<session::CommandFragment> : public Void {};
template <> struct ArrayDomain<dtx::Xid> : public Void {};
inline std::ostream& operator<<(std::ostream& o, const ArrayDomain<session::CommandFragment>&) { return o; }
inline std::ostream& operator<<(std::ostream& o, const ArrayDomain<dtx::Xid>&) { return o; }

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_ARRAY_H*/
