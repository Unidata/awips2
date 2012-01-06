#ifndef QPID_FRAMING_TEMPLATEVISITOR_H
#define QPID_FRAMING_TEMPLATEVISITOR_H

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
#include <boost/mpl/fold.hpp>
#include <boost/utility/value_init.hpp>

namespace qpid {
namespace framing {

/**
 * Metafunction to generate a visitor class derived from Base, with a
 * visit for each type in TypeList calling functor F. TypeList may be
 * any boost::mpl type collection e.g. mpl::list. 
 *
 * Generated class is: TemplateVisitor<Base, F, TypeList>::type
 *
 * @see make_visitor
 */
template <class VisitTemplate, class TypeList, class F>
class TemplateVisitor
{
    struct Base : public VisitorBase {
        F action;
        Base(F f) : action(f) {}
        using VisitorBase::visit;
    };
    
    template <class B, class T> struct Visit : public B {
        Visit(F action) : B(action) {}
        using B::visit;
        void visit(const T& body) { action(body); }
    };

    typedef typename boost::mpl::fold<
        TypeList, Base, Visit<boost::mpl::placeholders::_1,
                              boost::mpl::placeholders::_2>
        >::type type;
};

/**
 * Construct a TemplateVisitor to perform the given action,
 * for example:
 * @code 
 */
template <class VisitorBase, class TypeList, class F>
TemplateVisitor<VisitorBase,TypeList,F>::type make_visitor(F action) {
    return TemplateVisitor<VisitorBase,TypeList,F>::type(action);
};

/**
 * For method body classes in TypeList, invoke the corresponding function
 * on Target and return true. For other body types return false.
 */
template <class TypeList, class Target>
bool invoke(const AMQBody& body, Target& target) {
    typename InvokeVisitor<TypeList, Target>::type v(target);
    body.accept(v);
    return v.target;
}

}} // namespace qpid::framing


#endif  /*!QPID_FRAMING_INVOKEVISITOR_H*/

}} // namespace qpid::framing



#endif  /*!QPID_FRAMING_TEMPLATEVISITOR_H*/
