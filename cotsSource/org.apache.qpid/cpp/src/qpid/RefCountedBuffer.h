#ifndef QPID_REFCOUNTEDBUFFER_H
#define QPID_REFCOUNTEDBUFFER_H

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

#include <boost/utility.hpp>
#include <boost/detail/atomic_count.hpp>
#include <boost/intrusive_ptr.hpp>

namespace qpid {
// FIXME aconway 2008-09-06: easy to add alignment
/**
 * Reference-counted byte buffer.
 * No alignment guarantees.
 */
class RefCountedBuffer : boost::noncopyable {
    mutable boost::detail::atomic_count count;
    RefCountedBuffer();
    void destroy() const;
    char* addr() const;

public:
    /** Smart char pointer to a reference counted buffer */
    class pointer {
        boost::intrusive_ptr<RefCountedBuffer> p;
        char* cp() const;
        pointer(RefCountedBuffer* x);
      friend class RefCountedBuffer;

      public:
        pointer();
        pointer(const pointer&);
        ~pointer();
        pointer& operator=(const pointer&);
        
        char* get() { return cp(); }
        operator char*() { return cp(); }
        char& operator*() { return *cp(); }
        char& operator[](size_t i) { return cp()[i]; }

        const char* get() const { return cp(); }
        operator const char*() const { return cp(); }
        const char& operator*() const { return *cp(); }
        const char& operator[](size_t i) const { return cp()[i]; }
    };
    
    /** Create a reference counted buffer of size n */
    static pointer create(size_t n);

    /** Get a pointer to the start of the buffer. */
    char* get() { return addr(); }
    const char* get() const { return addr(); }
    char& operator[](size_t i) { return get()[i]; }
    const char& operator[](size_t i) const { return get()[i]; }

    void addRef() const { ++count; }
    void release() const { if (--count==0) destroy(); }
    long refCount() { return count; }
};

} // namespace qpid

// intrusive_ptr support.
namespace boost {
inline void intrusive_ptr_add_ref(const qpid::RefCountedBuffer* p) { p->addRef(); }
inline void intrusive_ptr_release(const qpid::RefCountedBuffer* p) { p->release(); }
}


#endif  /*!QPID_REFCOUNTEDBUFFER_H*/
