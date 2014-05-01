#ifndef QPID_SYS_ATOMICVALUE_GCC_H
#define QPID_SYS_ATOMICVALUE_GCC_H

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

#if !defined(QPID_SYS_ATOMICVALUE_H)
#error "This file should only be included via AtomicValue.h."
#endif

namespace qpid {
namespace sys {

/**
 * Atomic value of type T. T must be an integral type of size 1,2,4 or 8 bytes.
 * All operations are atomic and preform a full memory barrier unless otherwise noted.
 */
template <class T>
class AtomicValue
{
  public:
    AtomicValue(T init=0) : value(init) {}

    // Update and return new value.
    inline T operator+=(T n) { return __sync_add_and_fetch(&value, n); }
    inline T operator-=(T n) { return __sync_sub_and_fetch(&value, n); }
    inline T operator++() { return *this += 1; }
    inline T operator--() { return *this -= 1; }

    // Update and return old value.
    inline T fetchAndAdd(T n) { return __sync_fetch_and_add(&value, n); }
    inline T fetchAndSub(T n) { return __sync_fetch_and_sub(&value, n); }
    inline T operator++(int) { return fetchAndAdd(1); }
    inline T operator--(int) { return fetchAndSub(1); }

    /** If current value == testval then set to newval. Returns the old value. */
    T valueCompareAndSwap(T testval, T newval) { return __sync_val_compare_and_swap(&value, testval, newval); }

    /** If current value == testval then set to newval. Returns true if the swap was performed. */    
    bool boolCompareAndSwap(T testval, T newval) { return __sync_bool_compare_and_swap(&value, testval, newval); }

    T get() const { return const_cast<AtomicValue<T>*>(this)->fetchAndAdd(static_cast<T>(0)); }
        
  private:
    T value;
};

}} // namespace qpid::sys

#endif  /*!QPID_SYS_ATOMICVALUE_GCC_H*/
