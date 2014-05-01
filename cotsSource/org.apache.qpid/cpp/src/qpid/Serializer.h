#ifndef QPID_SERIALIZER_H
#define QPID_SERIALIZER_H

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

#include <limits>
#include <algorithm>
#include "qpid/Exception.h"     // FIXME aconway 2008-04-03: proper exception class.

namespace qpid {

/**
 * Overload for types that do not provide a serialize() member.
 * It should retrun a wrapper holding a reference to t that implements
 * serialize()
 */
template <class T> T& serializable(T& t) { return t; }

/** Serialize std::pair */
template <class T, class U> struct SerializablePair {
    std::pair<T,U>& value;
    SerializablePair(std::pair<T,U>& x) : value(x) {}
    template <class S> void serialize(S& s) { s(value.first)(value.second); }
};

template <class T, class U>
SerializablePair<T,U> serializable(std::pair<T,U>& p) {
    return SerializablePair<T,U>(p);
}

/**
 * Base class for all serializers.
 * Derived serializers inherit from either Encoder or Decoder.
 * Serializers can be used as functors or static_visitors.
 */
template <class Derived> class Serializer {
  public:
    /** Temporarily set a lower relative limit on the serializer */
    class ScopedLimit {
      public:
        ScopedLimit(Serializer& s, size_t l)
            : serializer(s), save(serializer.setLimit(l)) {}
        
        ~ScopedLimit() { serializer.setAbsLimit(save); }
        
      private:
        Serializer& serializer;
        size_t save;
    };

    static size_t maxLimit() { return  std::numeric_limits<size_t>::max(); }
    
    Serializer() : bytes(0), limit(maxLimit()) {}
    
    typedef Derived& result_type; // unary functor requirement.

    /** Wrapper functor to pass serializer functors by reference. */
    template <class S> struct Ref {
        typedef  typename S::result_type result_type;
        S& s;
        Ref(S& ss) : s(ss) {}
        template <class T> result_type operator()(T& x) { return s(x); }
        template <class T> result_type operator()(const T& x) { return s(x); }
    };

    /** Reference wrapper to pass serializers by reference,
     * e.g. to std:: functions that take functors.
     */
    template <class S> static Ref<S> ref(S& s) { return Ref<S>(s); }

    /** Generic rule to serialize an iterator range */
    template <class Iter> Derived& operator()(Iter begin, Iter end) {
        std::for_each(begin, end, ref(this->self()));
        return self();
    }

    /** Set limit relative to current position.
     * @return old absolute limit.
     */
    size_t setLimit(size_t n) {
        size_t l=limit;
        limit = bytes+n;
        return l;
    }

    /** Get the max number of bytes that can be processed under the
     * current limit.
     */
    size_t bytesRemaining() const {
        return limit - bytes;
    }
    /** Set absolute limit. */
    void setAbsLimit(size_t n) {
        limit = n;
        if (bytes > limit)
            throw Exception("Framing error: data overrun"); // FIXME aconway 2008-04-03: proper exception.
    }

  protected:
    Derived& self() { return *static_cast<Derived*>(this); }
    void addBytes(size_t n) {
        size_t newBytes=bytes+n;
        if (newBytes > limit)
            throw Exception("Framing error: data overrun"); // FIXME aconway 2008-04-03: proper exception.
        bytes = newBytes;
    }
    
  private:
    void checkLimit() {
    }
        
    size_t bytes;               // how many bytes serialized.
    size_t limit;               // bytes may not exceed this limit.
};

/**
 * Base class for encoders, provides generic encode functions.
 *
 * A derived encoder must provide operator(const T&) to encode all
 * primitive types T.
 */
template <class Derived> class EncoderBase : public Serializer<Derived> {
  public:
    using Serializer<Derived>::operator();
    using Serializer<Derived>::self;
    
    /** Default op() for non-primitive types. */
    template <class T> Derived& operator()(const T& t) {
        serializable(const_cast<T&>(t)).serialize(self()); return self();
    }

    /** Split serialize() into encode()/decode() */
    template <class T> Derived& split(const T& t) {
        t.encode(self()); return self();
    }
};

/**
 * Base class for decoders, provides generic decode functions.
 *
 * A derived encoder must provide operator(T&) to encode all
 * primitive types T.
 */
template <class Derived> class DecoderBase : public Serializer<Derived> {
  public:
    using Serializer<Derived>::operator();
    using Serializer<Derived>::self;

    /** Default op() for non-primitive types. */
    template <class T> Derived& operator()(T& t) {

        serializable(t).serialize(self()); return self();
    }

    /** Split serialize() into encode()/decode() */
    template <class T> Derived& split(T& t) {
        t.decode(self()); return self();
    }
};

/** Serialize a type by converting it to/from another type.
 * To serialize type Foo by converting to/from type Bar create
 * a serializable() overload like this:
 * 
 * SerializeAs<Foo,Bar> serializable(Foo& t) { return SerializeAs<Foo,Bar>(t); }
 */
template <class Type, class AsType>
struct SerializeAs {
    Type& value;
    SerializeAs(Type & t) : value(t) {}
    template <class S> void serialize(S& s) { s.split(*this); }
    template <class S> void encode(S& s) const { s(AsType(value)); }
    template <class S> void decode(S& s) { AsType x; s(x); value=Type(x); }
};

} // namespace qpid

#endif  /*!QPID_SERIALIZER_H*/
