#ifndef QPID_AMQP_0_10_APPLY_H
#define QPID_AMQP_0_10_APPLY_H

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

namespace qpid {
namespace amqp_0_10 {

template <class F, class R=typename F::result_type> struct FunctionAndResult {
    F* functor;
    boost::optional<R> result;

    FunctionAndResult() : functor(0) {}
    template <class T> void invoke(T& t) { result=(*functor)(t); }
    template <class T> void invoke(const T& t) { result=(*functor)(t); }
    R getResult() { return *result; }
};

// void result is special case.
template <class F> struct FunctionAndResult<F, void> {
    F* functor;
    
    FunctionAndResult() : functor(0) {}
    template <class T> void invoke(T& t) { (*functor)(t); }
    void getResult() {}
};

// Metafunction returning correct abstract visitor for Visitable type. 
template <class Visitable> struct VisitorType {
    typedef typename Visitable::Visitor type;
};
template <class Visitable> struct VisitorType<const Visitable> {
    typedef typename Visitable::ConstVisitor type;
};
    
template <class Visitor, class F>
struct ApplyVisitorBase : public Visitor, public FunctionAndResult<F> {};

// Specialize for each visitor type
template <class Visitable, class F> struct ApplyVisitor;

/** Apply a functor to a visitable object.
 * The functor can have operator() overloads for each visitable type
 * and/or templated operator().
 */
template <class F, class Visitable>
typename F::result_type apply(F& functor, Visitable& visitable) {
    ApplyVisitor<typename VisitorType<Visitable>::type, F> visitor;
    visitor.functor=&functor;
    visitable.accept(visitor);
    return visitor.getResult();
}

template <class F, class Visitable>
typename F::result_type apply(const F& functor, Visitable& visitable) {
    ApplyVisitor<typename VisitorType<Visitable>::type, const F> visitor;
    visitor.functor=&functor;
    visitable.accept(visitor);
    return visitor.getResult();
}

template <class R> struct ApplyFunctor { typedef R result_type; };

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_APPLY_H*/
