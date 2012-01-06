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
#include "qpid/client/Bounds.h"

#include "qpid/log/Statement.h"
#include "qpid/sys/Waitable.h"

namespace qpid {
namespace client {

using sys::Waitable;

Bounds::Bounds(size_t maxSize) : max(maxSize), current(0) {}

bool Bounds::expand(size_t sizeRequired, bool block) {
    if (!max) return true;
    Waitable::ScopedLock l(lock);
    current += sizeRequired;
    if (block) {
        Waitable::ScopedWait w(lock);
        while (current > max) 
            lock.wait();
    }
    return current <= max;
}

void Bounds::reduce(size_t size) {
    if (!max || size == 0) return;
    Waitable::ScopedLock l(lock);
    if (current == 0) return;
    current -= std::min(size, current);
    if (current < max && lock.hasWaiters()) {
        lock.notifyAll();
    }
}

size_t Bounds::getCurrentSize() {
    Waitable::ScopedLock l(lock);
    return current;
}

std::ostream& operator<<(std::ostream& out, const Bounds& bounds) {
    out << "current=" << bounds.current << ", max=" << bounds.max << " [" << &bounds << "]";
    return out;
}

void Bounds::setException(const sys::ExceptionHolder& e) {
    Waitable::ScopedLock l(lock);    
    lock.setException(e);
    lock.waitWaiters();         // Wait for waiting threads to exit.
}

}} // namespace qpid::client
