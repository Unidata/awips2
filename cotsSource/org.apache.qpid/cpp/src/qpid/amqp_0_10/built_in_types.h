#ifndef QPID_AMQP_0_10_BUILT_IN_TYPES_H
#define QPID_AMQP_0_10_BUILT_IN_TYPES_H
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

#include "qpid/Serializer.h"
#include "qpid/framing/Uuid.h"
#include "qpid/sys/IntegerTypes.h"
#include "qpid/sys/Time.h"
#include "qpid/amqp_0_10/Decimal.h"
#include "qpid/amqp_0_10/SerializableString.h"
#include <boost/array.hpp>
#include <boost/range/iterator_range.hpp>
#include <string>
#include <ostream>
#include <vector>

/**@file Mapping from built-in AMQP types to C++ types */

namespace qpid {

namespace framing {
class SequenceNumber;
class SequenceSet;
}

namespace amqp_0_10 {

/** Wrapper that behaves like type T but is a distinct type for
 * overloading purposes. Unique allows multiple distinc wrappers.
 */
template <class T, int Unique=0> struct Wrapper {
    T value;
    Wrapper() {}
    Wrapper(const T& x) : value(x) {}
    Wrapper& operator=(const T& x) { value=x; return *this; }
    operator T&() { return value; }
    operator const T&() const { return value; }
    template <class S> void serialize(S& s) { s(value); }
};

template<class T>
inline std::ostream& operator<<(std::ostream& o, const Wrapper<T>& w) {
    return o << w.value;
}

/** Void type */
struct Void { template <class S> void serialize(S&) {} };
inline std::ostream& operator<<(std::ostream& o, const Void&) { return o; } 

/** Bit is a presence indicator - an optional value with no encoding. */
struct Bit : public Wrapper<bool> {
    Bit(bool b=false) : Wrapper<bool>(b) {}
    using Wrapper<bool>::operator=;
    template <class S> void serialize(S& s) { s.split(*this); } 
    template <class S> void encode(S&) const { }
    template <class S> void decode(S&) { *this = true; }
};

inline std::ostream& operator<<(std::ostream& o, const Bit& b) {
    return o << bool(b);
}

// Fixed size types
typedef bool Boolean;
typedef char Char;
typedef int8_t Int8;
typedef int16_t Int16;
typedef int32_t Int32;
typedef int64_t Int64;
typedef uint8_t Uint8;
typedef uint16_t Uint16;
typedef uint32_t Uint32;
typedef uint64_t Uint64;
typedef Wrapper<uint32_t> CharUtf32;

template <size_t N> struct Bin : public boost::array<char, N> {
    template <class S> void serialize(S& s) { s.raw(this->begin(), this->size()); }
};

template <size_t N> std::ostream& operator<<(std::ostream& o, const Bin<N>& b) {
    return o << boost::make_iterator_range(b.begin(), b.end());
}

template <> struct Bin<1> : public boost::array<char, 1> {
    Bin(char c=0) { this->front() = c; }
    operator char() { return this->front(); }
    template <class S> void serialize(S& s) { s(front()); }
};

typedef Bin<1> Bin8;
typedef Bin<128> Bin1024; 
typedef Bin<16> Bin128;
typedef Bin<2> Bin16;
typedef Bin<32> Bin256;
typedef Bin<4> Bin32;
typedef Bin<5> Bin40; 
typedef Bin<64> Bin512;
typedef Bin<8> Bin64;
typedef Bin<9> Bin72;

typedef double Double;
typedef float Float;
typedef framing::SequenceNumber SequenceNo;
using framing::Uuid;
typedef sys::AbsTime Datetime;

typedef Decimal<Uint8, Int32> Dec32;
typedef Decimal<Uint8, Int64> Dec64;

// Variable width types

typedef SerializableString<Uint8, Uint8> Vbin8;
typedef SerializableString<char, Uint8, 1> Str8Latin;
typedef SerializableString<char, Uint8> Str8;
typedef SerializableString<Uint16, Uint8> Str8Utf16;

typedef SerializableString<Uint8, Uint16> Vbin16;
typedef SerializableString<char, Uint16, 1> Str16Latin;
typedef SerializableString<char, Uint16> Str16;
typedef SerializableString<Uint16, Uint16> Str16Utf16;

typedef SerializableString<Uint8, Uint32> Vbin32;

typedef framing::SequenceSet SequenceSet;

// Forward declare class types.
class Map;
class Struct32;
class UnknownType;

template <class T> struct  ArrayDomain;
typedef ArrayDomain<UnknownType> Array;

// FIXME aconway 2008-04-08: TODO
struct ByteRanges { template <class S> void serialize(S&) {} };
struct List  { template <class S> void serialize(S&) {} };

// FIXME aconway 2008-03-10: dummy ostream operators
inline std::ostream& operator<<(std::ostream& o, const ByteRanges&) { return o; }
inline std::ostream& operator<<(std::ostream& o, const SequenceSet&) { return o; }
inline std::ostream& operator<<(std::ostream& o, const List&) { return o; }

enum SegmentType { CONTROL, COMMAND, HEADER, BODY };

inline SerializeAs<SegmentType, uint8_t> serializable(SegmentType& st) {
    return SerializeAs<SegmentType, uint8_t>(st);
}


}} // namespace qpid::amqp_0_10

#endif
