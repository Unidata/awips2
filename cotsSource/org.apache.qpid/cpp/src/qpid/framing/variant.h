#ifndef QPID_FRAMING_VARIANT_H
#define QPID_FRAMING_VARIANT_H

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

/**@file Tools for using boost::variant. */


#include <boost/variant.hpp>

namespace qpid {
namespace framing {
class Buffer;

/** boost::static_visitor that throws an exception if variant contains a blank.
 * Subclasses need to have a using() declaration, which can be generated
 * with QPID_USING_NOBLANK(R)
 */
template <class R=void>
struct NoBlankVisitor : public boost::static_visitor<R> {
    R foundBlank() const {
        assert(0);
        throw Exception(QPID_MSG("Invalid variant value."));
    }
    R operator()(const boost::blank&) const { return foundBlank(); }
    R operator()(boost::blank&) const { return foundBlank(); }
};


}} // qpid::framing


/** Generate a using statement, needed in visitors inheriting NoBlankVisitor
 *  @param R return type.
 */
#define QPID_USING_NOBLANK(R) using ::qpid::framing::NoBlankVisitor<R>::operator()

namespace qpid {
namespace framing {

/** Convert the variant value to type R. */
template <class R> struct ConvertVisitor : public NoBlankVisitor<R> {
    QPID_USING_NOBLANK(R);
    template <class T> R operator()(T& t) const { return t; }
};

/** Convert the address of variant value to type R. */
template <class R> struct AddressVisitor : public NoBlankVisitor<R> {
    QPID_USING_NOBLANK(R);
    template <class T> R operator()(T& t) const { return &t; }
};

/** Apply a visitor to the nested variant.*/
template<class V>
struct ApplyVisitor : public NoBlankVisitor<typename V::result_type> {
    QPID_USING_NOBLANK(typename V::result_type);
    const V& visitor;
    ApplyVisitor(const V& v) : visitor(v) {}
    template <class T> typename V::result_type operator()(T& t) const {
        return boost::apply_visitor(visitor, t);
    }
};

/** Convenience function to construct and apply an ApplyVisitor */
template <class Visitor, class Visitable>
typename Visitor::result_type applyApplyVisitor(const Visitor& visitor, Visitable& visitable) {
    return boost::apply_visitor(ApplyVisitor<Visitor>(visitor), visitable);
}

}} // namespace qpid::framing

    
#endif  /*!QPID_FRAMING_VARIANT_H*/
