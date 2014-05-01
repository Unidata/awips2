#ifndef QPID_INLINEALLOCATOR_H
#define QPID_INLINEALLOCATOR_H

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

#include <memory>
#include <assert.h>
#include <boost/type_traits/type_with_alignment.hpp>
#include <boost/type_traits/alignment_of.hpp>

namespace qpid {

template <typename RequestedType, typename InlineType, typename BaseAllocator, size_t Max>
struct InlineRebind;


/**
 * An allocator that has inline storage for up to Max objects
 * of type BaseAllocator::value_type.
 */
template <class BaseAllocator, size_t Max>
class InlineAllocator : public BaseAllocator {
  public:
    typedef typename BaseAllocator::pointer pointer;
    typedef typename BaseAllocator::size_type size_type;
    typedef typename BaseAllocator::value_type value_type;

    InlineAllocator() : allocated(false) {}
    InlineAllocator(const InlineAllocator& x) : BaseAllocator(x), allocated(false) {}

    pointer allocate(size_type n) {
        if (n <= Max && !allocated) {
            allocated=true;
            return reinterpret_cast<value_type*>(address());
        }
        else
            return BaseAllocator::allocate(n, 0);
    }

    void deallocate(pointer p, size_type n) {
        if (p == address()) {
            assert(allocated);
            allocated=false;
        }
        else
            BaseAllocator::deallocate(p, n);
    }

    template<typename T1>
    struct rebind {
        typedef typename InlineRebind<T1, value_type, BaseAllocator, Max>::other other;
    };

  private:
    // POD object with alignment and size to hold Max value_types.
    static const size_t ALIGNMENT=boost::alignment_of<value_type>::value;
    typedef typename boost::type_with_alignment<ALIGNMENT>::type Aligner;
    union Store {
      Aligner aligner_;
      char sizer_[sizeof(value_type)*Max];
    } store;
    value_type* address() { return reinterpret_cast<value_type*>(&store); }
    bool allocated;
};


// Rebind: if RequestedType == InlineType, use the InlineAllocator,
// otherwise, use the BaseAllocator without any inlining.

template <typename RequestedType, typename InlineType, typename BaseAllocator, size_t Max>
struct InlineRebind {
    typedef typename BaseAllocator::template rebind<RequestedType>::other other;
};

template <typename T, typename BaseAllocator, size_t Max>
struct InlineRebind<T, T, BaseAllocator, Max> {
    typedef typename qpid::InlineAllocator<BaseAllocator, Max> other;
};

} // namespace qpid

#endif  /*!QPID_INLINEALLOCATOR_H*/
