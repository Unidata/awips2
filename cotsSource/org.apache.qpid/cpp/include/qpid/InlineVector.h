#ifndef QPID_INLINEVECTOR_H
#define QPID_INLINEVECTOR_H

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

#include "qpid/InlineAllocator.h"
#include <vector>

namespace qpid {

/**
 * A vector that stores up to Max elements in inline storage,
 * otherwise uses normal vector allocation.
 *
 * NOTE: depends on some non-standard but highly probably assumptions
 * about how std::vector uses its allocator, they are true for g++.
 *  - default constructor does not allocate.
 *  - reserve(N) does not allocate more than N elements.
 *  - vector never re-allocates when size() < capacity()
 */
template <class T, size_t Max, class Alloc=std::allocator<T> >
class InlineVector : public std::vector<T, InlineAllocator<Alloc, Max> >
{
    typedef std::vector<T, InlineAllocator<Alloc, Max> > Base;
  public:
    typedef typename Base::allocator_type allocator_type;
    typedef typename Base::value_type value_type;
    typedef typename Base::size_type size_type;

    explicit InlineVector(const allocator_type& a=allocator_type()) : Base(a) {
        this->reserve(Max);
    }

    explicit InlineVector(size_type n, const value_type& x = value_type(),
                          const allocator_type& a=allocator_type()) : Base(a)
    {
        this->reserve(std::max(n, Max));
        this->insert(this->end(), n, x);
    }

    InlineVector(const InlineVector& x) : Base() {
        this->reserve(std::max(x.size(), Max));
        *this = x;
    }
};

} // namespace qpid

#endif  /*!QPID_INLINEVECTOR_H*/
