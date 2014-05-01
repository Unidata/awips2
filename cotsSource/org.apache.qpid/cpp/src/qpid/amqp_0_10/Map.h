#ifndef QPID_AMQP_0_10_MAP_H
#define QPID_AMQP_0_10_MAP_H

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
 * software distributed under the License is distributed on ang
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

#include "qpid/Exception.h"
#include "qpid/amqp_0_10/built_in_types.h"
#include "qpid/amqp_0_10/UnknownType.h"
#include "qpid/amqp_0_10/CodeForType.h"
#include "qpid/amqp_0_10/TypeForCode.h"
#include "qpid/amqp_0_10/Codec.h"
#include "qpid/framing/Blob.h"
#include <map>
#include <string>
#include <iosfwd>

namespace qpid {
namespace amqp_0_10 {

class Map;

class MapValue {
  public:
    struct BadTypeException : public Exception {};

    template <class R> struct Visitor { typedef R result_type; };

    MapValue();
    MapValue(const MapValue& x);
    template <class T> explicit MapValue(const T& t);
    template <class T> MapValue& operator=(const T& t);

    template <class T> T* get();
    template <class T> const T* get() const;

    template <class V> typename V::result_type apply_visitor(V&);
    template <class V> typename V::result_type apply_visitor(const V&);

    uint8_t getCode() const { return code; }
    
    bool operator==(const MapValue&) const;

    template <class S> void serialize(S& s) { s(code); s.split(*this); }
    template <class S> void encode(S& s) const {
        const_cast<MapValue*>(this)->apply_visitor(s);
    }
    template <class S> void decode(S& s) {
        DecodeVisitor<S> dv(blob, s);
        qpid::amqp_0_10::apply_visitor(dv, code);
    }
    

  private:
    // TODO aconway 2008-04-15: Estimate required size, we will get a
    // compile error from static_assert in Blob.h if the estimate is too
    // low. We can't use sizeof() directly because #include Struct32.h
    // creates a circular dependency. Needs a better solution.
    static const size_t SIZE=256;
    typedef framing::Blob<SIZE> Blob;

    template <class V> struct VisitVisitor;
    template <class T> struct GetVisitor;
    template <class D> struct DecodeVisitor;

    uint8_t code;
    Blob blob;
};

class Map : public std::map<Str8, MapValue> {
  public:
    template <class S> void serialize(S& s) { s.split(*this); }
    template <class S> void encode(S& s) const;
    // Shortcut calculation for size.
    void encode(Codec::Size& s) const  { s.raw(0, contentSize() + 4/*size*/); }

    template <class S> void decode(S& s);
    
  private:
    uint32_t contentSize() const;
};

std::ostream& operator<<(std::ostream&, const MapValue&);
std::ostream& operator<<(std::ostream&, const Map::value_type&);
std::ostream& operator<<(std::ostream&, const Map&);

using framing::in_place;

template <class T> MapValue::MapValue(const T& t) : code(codeFor(t)), blob(in_place<t>()) {}

template <class T> MapValue& MapValue::operator=(const T& t) {
    code=codeFor(t);
    blob=t;
    return *this;
}

template <class V> struct MapValue::VisitVisitor {
    typedef typename V::result_type result_type;
    V& visitor;
    Blob& blob;
    VisitVisitor(V& v, Blob& b) : visitor(v), blob(b) {}

    template <class T> result_type operator()(T*) {
        return visitor(*reinterpret_cast<T*>(blob.get()));
    }
};
    
template <class V> typename V::result_type MapValue::apply_visitor(V& v) {
    VisitVisitor<V> visitor(v, blob);
    return qpid::amqp_0_10::apply_visitor(visitor, code);
}

template <class R> struct MapValue::GetVisitor {
    typedef R* result_type;
    const MapValue::Blob& blob;

    GetVisitor(const MapValue::Blob& b) : blob(b) {}

    R* operator()(R& r) { return &r; }
    template <class T> R* operator()(T&) { return 0; }
};
    
template <class D> struct MapValue::DecodeVisitor {
    typedef void result_type;
    MapValue::Blob& blob;
    D& decoder;
    DecodeVisitor(Blob& b, D& d) : blob(b), decoder(d) {}
    
    template <class T> void operator()(T*) {
        T t;
        decoder(t);
        blob = t;
    }
};

template <class T> T* MapValue::get() { return apply_visitor(GetVisitor<T>(blob)); }
template <class T> const T* MapValue::get() const { return apply_visitor(GetVisitor<const T>()); }

template <class V> typename V::result_type MapValue::apply_visitor(const V& v) {
    return apply_visitor(const_cast<V&>(v));
}

template <class S> void Map::encode(S& s) const {
    // FIXME aconway 2008-04-03: replace preview mapping with 0-10 mapping:
    // s(contentSize())(uint32_t(size())); // size, count
    s(contentSize());
    for (const_iterator i = begin(); i != end(); ++i)
        s(i->first)(i->second); // key (type value)
}

template <class S> void Map::decode(S& s) {
    uint32_t decodedSize /*, count*/;
    // FIXME aconway 2008-04-03: replace preview mapping with 0-10 mapping:
    // s(contentSize())(uint32_t(size())); // size, count
    // s(decodedSize)(count);
    s(decodedSize);
    typename S::ScopedLimit l(s, decodedSize); // Make sure we don't overrun.
    // FIXME aconway 2008-04-03:  replace preview with 0-10:
    // for ( ; count > 0; --count) {
    while (s.bytesRemaining() > 0) {
        key_type k; MapValue v;
        s(k)(v);
        insert(value_type(k,v));
    }
}


}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_MAP_H*/
