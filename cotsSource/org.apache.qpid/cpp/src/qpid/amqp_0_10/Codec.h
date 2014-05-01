#ifndef QPID_AMQP_0_10_CODEC_H
#define QPID_AMQP_0_10_CODEC_H

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

#include "qpid/amqp_0_10/built_in_types.h"
#include "qpid/Serializer.h"
#include <boost/type_traits/is_integral.hpp>
#include <boost/type_traits/is_float.hpp>
#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/detail/endian.hpp>
#include <boost/static_assert.hpp>
#include <iterator>

namespace qpid {
namespace amqp_0_10 {

template <class T> void reverse(T& t) {
    char*p =reinterpret_cast<char*>(&t);
    std::reverse(p, p+sizeof(T));
}

#ifdef BOOST_LITTLE_ENDIAN
template <class T> void bigEndian(T& t) { reverse(t); }
template <class T> void littleEndian(T&) {}
#else
template <class T> void littleEndian(T& t) { reverse(t); }
template <class T> void bigEndian(T&) {}
#endif

/**
 * AMQP 0-10 encoding and decoding.
 */
struct Codec {
    /** Encode to an output byte iterator */
    template <class OutIter>
    class Encoder : public EncoderBase<Encoder<OutIter> >
    {
      public:
        typedef EncoderBase<Encoder<OutIter> > Base;
        typedef OutIter Iterator;

        Encoder(OutIter o, size_t limit=Base::maxLimit()) : out(o) {
            this->setLimit(limit);
        }

        using EncoderBase<Encoder<OutIter> >::operator();

        Encoder& operator()(bool x) { raw(x); return *this;} 
        Encoder& operator()(char x) { raw(x); return *this; }
        Encoder& operator()(int8_t x) { raw(x); return *this; }
        Encoder& operator()(uint8_t x) { raw(x); return *this; }

        Encoder& operator()(int16_t x) { return networkByteOrder(x); }
        Encoder& operator()(int32_t x) { return networkByteOrder(x); }
        Encoder& operator()(int64_t x) { return networkByteOrder(x); }

        Encoder& operator()(uint16_t x) { return networkByteOrder(x); }
        Encoder& operator()(uint32_t x) { return networkByteOrder(x); }
        Encoder& operator()(uint64_t x) { return networkByteOrder(x); }

        Encoder& operator()(float x) { return networkByteOrder(x); }
        Encoder& operator()(double x) { return networkByteOrder(x); }

        void raw(const void* p, size_t n) {
            this->addBytes(n);
            out = std::copy((const char*)p, (const char*)p+n, out);
        }

        void raw(char b) { this->addBytes(1); *out++=b; }

        template <class T> Encoder& littleEnd(T x) {
            littleEndian(x); raw(&x, sizeof(x)); return *this;
        }
        
        OutIter pos() const { return out; }

      private:

        template <class T> Encoder& networkByteOrder(T x) {
            bigEndian(x); raw(&x, sizeof(x)); return *this;
        }

        OutIter out;
    };

    template <class InIter>
    class Decoder : public DecoderBase<Decoder<InIter> > {
      public:
        typedef DecoderBase<Decoder<InIter> > Base;
        typedef InIter Iterator;

        Decoder(InIter i, size_t limit=Base::maxLimit()) : in(i) {
            this->setLimit(limit);
        }

        using DecoderBase<Decoder<InIter> >::operator();
        
        // FIXME aconway 2008-03-10:  wrong encoding, need packing support
        Decoder& operator()(bool& x) { raw((char&)x); return *this; }

        Decoder& operator()(char& x) { raw((char&)x); return *this; }
        Decoder& operator()(int8_t& x) { raw((char&)x); return *this; }
        Decoder& operator()(uint8_t& x) { raw((char&)x); return *this; }

        Decoder& operator()(int16_t& x) { return networkByteOrder(x); }
        Decoder& operator()(int32_t& x) { return networkByteOrder(x); }
        Decoder& operator()(int64_t& x) { return networkByteOrder(x); }

        Decoder& operator()(uint16_t& x) { return networkByteOrder(x); }
        Decoder& operator()(uint32_t& x) { return networkByteOrder(x); }
        Decoder& operator()(uint64_t& x) { return networkByteOrder(x); }

        Decoder& operator()(float& x) { return networkByteOrder(x); }
        Decoder& operator()(double& x) { return networkByteOrder(x); }

        void raw(void *p, size_t n) {
            this->addBytes(n);
            std::copy(in, in+n, (char*)p);
            std::advance(in, n);
        }

        void raw(char &b) { this->addBytes(1); b=*in++; }

        template <class T> Decoder& littleEnd(T& x) {
            raw(&x, sizeof(x)); littleEndian(x); return *this;
        }
        
        InIter pos() const { return in; }

      private:

        template <class T> Decoder& networkByteOrder(T& x) {
            raw(&x, sizeof(x)); bigEndian(x); return *this;
        }

        InIter in;
    };

    
    class Size : public EncoderBase<Size> {
      public:
        Size() : size(0) {}

        operator size_t() const { return size; }

        using EncoderBase<Size>::operator();

        // FIXME aconway 2008-03-10:  wrong encoding, need packing support
        Size& operator()(bool x)  { size += sizeof(x); return *this; }
        
        Size& operator()(char x)  { size += sizeof(x); return *this; }
        Size& operator()(int8_t x)  { size += sizeof(x); return *this; }
        Size& operator()(uint8_t x)  { size += sizeof(x); return *this; }

        Size& operator()(int16_t x)  { size += sizeof(x); return *this; }
        Size& operator()(int32_t x)  { size += sizeof(x); return *this; }
        Size& operator()(int64_t x)  { size += sizeof(x); return *this; }

        Size& operator()(uint16_t x)  { size += sizeof(x); return *this; }
        Size& operator()(uint32_t x)  { size += sizeof(x); return *this; }
        Size& operator()(uint64_t x)  { size += sizeof(x); return *this; }

        Size& operator()(float x)  { size += sizeof(x); return *this; }
        Size& operator()(double x)  { size += sizeof(x); return *this; }

        // FIXME aconway 2008-04-03: optimize op()(Iter,Iter)
        // for Iter with fixed-size value_type:
        // distance(begin,end)*sizeof(value_type)

        void raw(const void*, size_t n){ size += n; }

        template <class T> Size& littleEnd(T) { size+= sizeof(T); return *this; }

      private:
        size_t size;
    };

    // FIXME aconway 2008-03-11: rename to encoder(), decoder()
    template <class InIter> static Decoder<InIter> decode(const InIter &i) {
        return Decoder<InIter>(i);
    }

    template <class OutIter> static Encoder<OutIter> encode(OutIter i) {
        return Encoder<OutIter>(i);
    }

    template <class T> static size_t size(const T& x) { return Size()(x); }
    template <class Iter> static size_t size(const Iter& a, const Iter& z) { return Size()(a,z); }
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_CODEC_H*/
