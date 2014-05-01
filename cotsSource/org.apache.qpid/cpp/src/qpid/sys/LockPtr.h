#ifndef QPID_SYS_LOCKPTR_H
#define QPID_SYS_LOCKPTR_H

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

#include "qpid/sys/Mutex.h"
#include <boost/noncopyable.hpp>

namespace qpid {
namespace sys {

class Mutex;

/**
 * LockPtr is a smart pointer to T. It is constructed from a volatile
 * T* and a Lock (by default a Mutex). It const_casts away the
 * volatile qualifier and locks the Lock for the duration of its  
 * 
 * Used in conjuntion with the "volatile" keyword to get the compiler
 * to help enforce correct concurrent use of mutli-threaded objects.
 * See ochttp://www.ddj.com/cpp/184403766 for a detailed discussion.
 * 
 * To summarize the convention: 
 *  - Declare thread-safe member functions as volatile.
 *  - Declare instances of the class that may be called concurrently as volatile.
 *  - Use LockPtr to cast away the volatile qualifier while taking a lock.
 *
 * This means that code calling on a concurrently-used object
 * (declared volatile) can only call thread-safe (volatile) member
 * functions. Code that needs to use thread-unsafe members must use a
 * LockPtr, thereby acquiring the lock and making it safe to do so.
 *
 * A good type-safe pattern is the internally-locked object:
 *  - It has it's own private lock member.
 *  - All public functions are thread safe and declared volatile.
 *  - Any thread-unsafe, non-volatile functions are private.
 *  - Only member function implementations use LockPtr to access private functions.
 *  
 * This encapsulates all the locking logic inside the class.
 *
 * One nice feature of this convention is the common case where you
 * need a public, locked version of some function foo() and also a
 * private unlocked version to avoid recursive locks. They can be declared as
 * volatile and non-volatile overloads of the same function:
 *
 * // public 
 * void Thing::foo() volatile { LockPtr<Thing>(this, myLock)->foo(); }
 * // private 
 * void Thing::foo() { ... do stuff ...}
 */

template <class T, class Lock> class LockPtr : public boost::noncopyable {
  public:
    LockPtr(volatile T* p, Lock& l) :  ptr(const_cast<T*>(p)), lock(l) { lock.lock(); }
    LockPtr(volatile T* p, volatile Lock& l) :  ptr(const_cast<T*>(p)), lock(const_cast<Lock&>(l)) { lock.lock(); }
    ~LockPtr() { lock.unlock(); }

   T& operator*() { return *ptr; }
   T* operator->() { return ptr; }

  private:
   T* ptr;
   Lock& lock;
};


}} // namespace qpid::sys


#endif  /*!QPID_SYS_LOCKPTR_H*/
