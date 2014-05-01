#ifndef QPID_PACKER_H
#define QPID_PACKER_H

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

#include <boost/optional.hpp>
#include <boost/none.hpp>
#include "qpid/amqp_0_10/built_in_types.h"

namespace qpid {
namespace amqp_0_10 {

/** Serialization for optional values */
template <class T> struct SerializableOptional {
    boost::optional<T>& optional;
    SerializableOptional(boost::optional<T>& x) : optional(x) {}
    template <class S> void serialize(S& s) {
        if (optional)
            s(*optional);
    }
};

}}


namespace boost {               // For argument dependent lookup.

template <class T>
qpid::amqp_0_10::SerializableOptional<T> serializable(boost::optional<T>& x) {
    return qpid::amqp_0_10::SerializableOptional<T>(x);
}

} // namespace boost

namespace qpid {
namespace amqp_0_10 {

/** "Encoder" that encodes a struct as a set of bit flags
 * for all non-empty members.
 */
class PackBits {
  public:
    PackBits() : bit(1), bits(0) {}

    void setBit(bool b) { if (b) bits |= bit; bit <<= 1; }
    uint32_t getBits() { return bits; }

    /** The bit is always set for non-optional values. */
    template <class T>
    PackBits& operator()(const T&) { setBit(1); return *this; }

    /** For optional values the bit is set if the value is present. */
    template <class T> PackBits& operator()(const boost::optional<T>& opt) {
        setBit(opt); return *this;
    }

    /** Bits are special optional values */
    PackBits& operator()(Bit b) { setBit(b); return *this; }
    
  private:
    uint32_t bit;
    uint32_t bits;
};

/** Bit mask to encode a packable struct */
template<class T> uint32_t packBits(const T& t) {
    PackBits pack;
    const_cast<T&>(t).serialize(pack);
    return pack.getBits();
}

/** Decode members enabled by Bits */
template <class Decoder, class Bits>
class PackedDecoder {
  public:
    PackedDecoder(Decoder& d, Bits b) : decode(d), bits(b) {}

    template <class T> PackedDecoder& operator()(T& t) {
        if (bits & 1)
            decode(t);
        else
            t = T();
        // FIXME aconway 2008-04-10: When we have all optionals
        // represented by boost::optional the line above should be:
        // throw CommandInvalidException("A required value was omitted.");
        bits >>= 1;
        return *this;
    }
    
    template <class T> PackedDecoder& operator()(boost::optional<T>& opt) {
        if (bits & 1) {
            opt = T();          
            decode(*opt);
        }
        else
            opt = boost::none;
        bits >>= 1;
        return *this;
    }

  private:
    Decoder& decode;
    Bits bits;
};

/** Metafunction to compute type to contain pack bits. */
template <int Bytes> struct UintOfSize;
template <> struct UintOfSize<1> { typedef uint8_t type; };
template <> struct UintOfSize<2> { typedef uint16_t type; };
template <> struct UintOfSize<4> { typedef uint32_t type; };

/**
 * Helper to serialize packed structs.
 */
template <class T> class Packer
{
  public:
    typedef typename UintOfSize<T::PACK>::type Bits;

    Packer(T& t) : data(t) {}

    template <class S> void serialize(S& s) { s.split(*this); }

    template <class S> void encode(S& s) const {
        Bits bits = packBits(data);
        s.littleEnd(bits);
        data.serialize(s);
    }

    template <class S> void decode(S& s) {
        Bits bits;
        s.littleEnd(bits);
        PackedDecoder<S, Bits> decode(s, bits);
        data.serialize(decode);
    }
    

  protected:
    T& data;
};

template <class T, uint8_t=T::SIZE> struct SizedPacker : public Packer<T> {
    typedef typename UintOfSize<T::SIZE>::type Size;
    
    SizedPacker(T& t) : Packer<T>(t) {}

    template <class S> void serialize(S& s) {
        s.split(*this);
    }

    template <class S> void encode(S& s) const {
        Codec::Size sizer;
        this->data.serialize(sizer);
        Size size=size_t(sizer)+T::PACK; // Size with pack bits.
        s(size);
        Packer<T>::encode(s);
    }

    template <class S> void decode(S& s) {
        Size size;
        s(size);
        typename S::ScopedLimit l(s, size);
        Packer<T>::decode(s);
    }

};

template <class T> struct SizedPacker<T,0> : public Packer<T> {
    SizedPacker(T& t) : Packer<T>(t) {}
};

}} // namespace qpid::amqp_0_10



#endif  /*!QPID_PACKER_H*/
