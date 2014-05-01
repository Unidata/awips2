#ifndef QPID_FRAMING_VISITOR_H
#define QPID_FRAMING_VISITOR_H

/*
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

#include <boost/mpl/vector.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/preprocessor/seq/for_each.hpp>

namespace qpid {
namespace framing {

/** @file Generic visitor pattern. */

/** visit() interface for type T (optional return type R, default is void.)
 * To create  a visitor for a set of types T1, T2 ... do this:
 * struct MyVisitor : public Visit<T1>, public Visit<T2> ... {};
 *@param T Type to visit. This must be forward declared, and need not be defined.
 */
template <class T, class R=void> struct Visit {
    typedef R ReturnType;
    typedef T VisitType;
    
    virtual ~Visit() {}
    virtual R visit(T&) = 0;
};


#define QPID_VISITOR_DECL(_1,_2,T) class T;

#define QPID_VISITOR_BASE(_1,_2,T) , public ::qpid::framing::Visit<T>

/** Convenience macro to generate a visitor interface.
 * QPID_VISITOR(MyVisitor,(A)(B)(C)); is equivalent to:
 * @code
 * class A; class B; class C;
 * class MyVisitor : public Visit<A> , public Visit<B> , public Visit<C> {};
 * @endcode
 * @param visitor name of the generated visitor class.
 * @param bases a sequence of visitable types in the form (T1)(T2)...
 * Any parenthesized notations are due to quirks of the preprocesser.
 */
#define QPID_VISITOR(visitor,types) \
    BOOST_PP_SEQ_FOR_EACH(QPID_VISITOR_DECL, _, types) \
    class visitor : public ::qpid::framing::Visit<BOOST_PP_SEQ_HEAD(types)> \
    BOOST_PP_SEQ_FOR_EACH(QPID_VISITOR_BASE, _, BOOST_PP_SEQ_TAIL(types)) \
    {}

/** The root class for the hierarchy of objects visitable by Visitor V.
 * Defines virtual accept().
 */
template <class V, class R=void>
struct VisitableRoot {
    typedef V VisitorType;
    typedef R ReturnType;
    virtual ~VisitableRoot() {}
    virtual R accept(V& v) = 0;
};

/** The base class for concrete visitable classes.
 * Implements accept().
 * @param T type of visitable class (CRTP).
 * @param Base base class to inherit from.
 */
template <class T, class Base>
struct Visitable : public Base {
    void accept(typename Base::VisitorType& v) {
        static_cast<Visit<T>& >(v).visit(static_cast<T&>(*this));
    }
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_VISITOR_H*/
