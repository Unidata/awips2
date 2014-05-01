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

#include "qpid/RefCountedBuffer.h"
#include <new>

namespace qpid {

RefCountedBuffer::RefCountedBuffer() : count(0) {}

void RefCountedBuffer::destroy() const {
    this->~RefCountedBuffer();
    ::delete[] reinterpret_cast<const char*>(this);
}

char* RefCountedBuffer::addr() const {
    return const_cast<char*>(reinterpret_cast<const char*>(this)+sizeof(RefCountedBuffer));
}

RefCountedBuffer::pointer RefCountedBuffer::create(size_t n) {
    char* store=::new char[n+sizeof(RefCountedBuffer)];
    new(store) RefCountedBuffer;
    return pointer(reinterpret_cast<RefCountedBuffer*>(store));
}

RefCountedBuffer::pointer::pointer() {}
RefCountedBuffer::pointer::pointer(RefCountedBuffer* x) : p(x) {}
RefCountedBuffer::pointer::pointer(const pointer& x) : p(x.p) {}
RefCountedBuffer::pointer::~pointer() {}
RefCountedBuffer::pointer& RefCountedBuffer::pointer::operator=(const RefCountedBuffer::pointer& x) { p = x.p; return *this; }

char* RefCountedBuffer::pointer::cp() const { return p ? p->get() : 0; }
} // namespace qpid


