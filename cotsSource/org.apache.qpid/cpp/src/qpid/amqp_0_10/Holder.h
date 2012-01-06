#ifndef QPID_AMQP_0_10_HOLDER_H
#define QPID_AMQP_0_10_HOLDER_H

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
#include "qpid/framing/Blob.h"
#include "qpid/amqp_0_10/apply.h"

namespace qpid {
namespace amqp_0_10 {

using framing::in_place;

template <class Invokable> struct InvokeVisitor {
    typedef void result_type;
    Invokable& target;     
    InvokeVisitor(Invokable& i) : target(i) {}

    template <class Action>
    void operator()(const Action& action) { action.invoke(target); }
};

template <class DerivedHolder, class BaseHeld, size_t Size>
class Holder : public framing::Blob<Size, BaseHeld> {
    typedef framing::Blob<Size, BaseHeld> Base;

  public:
    
    Holder() {}
    template <class T> explicit Holder(const T& value) : Base(value) {}

    using Base::operator=;
    Holder& operator=(const BaseHeld& rhs);
    
    uint8_t getCode() const { return this->get()->getCode(); }
    uint8_t getClassCode() const { return this->get()->getClassCode(); }

    template <class Invokable> void invoke(Invokable& i) const {
        InvokeVisitor<Invokable> v(i);
        apply(v, *this->get());
    }
    
    template <class S> void encode(S& s) const {
        s(getClassCode())(getCode());
    }

    template <class S> void decode(S& s) {
        uint8_t code, classCode;
        s(classCode)(code);
        static_cast<DerivedHolder*>(this)->set(classCode, code);
    }

    template <class S> void serialize(S& s) {
        s.split(*this);
        qpid::amqp_0_10::apply(s, *this->get());
    }

    template <class T> T* getIf() {
        return (getClassCode()==T::CLASS_CODE && getCode()==T::CODE) ? static_cast<T*>(this->get()) : 0;
    }

    template <class T> const T* getIf() const {
        return (getClassCode()==T::CLASS_CODE && getCode()==T::CODE) ? static_cast<T*>(this->get()) : 0;
    }
    
  private:
    struct Assign : public ApplyFunctor<void> {
        Holder& holder;
        Assign(Holder& x) : holder(x) {}
        template <class T> void operator()(const T& rhs) { holder=rhs; }
    };
};

template <class D, class B, size_t S>
Holder<D,B,S>& Holder<D,B,S>::operator=(const B& rhs) {
    Assign assign(*this);
    qpid::amqp_0_10::apply(assign, rhs);
    return *this;
}



}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_HOLDER_H*/
