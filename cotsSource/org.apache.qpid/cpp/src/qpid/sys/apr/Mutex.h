#ifndef _sys_apr_Mutex_h
#define _sys_apr_Mutex_h

/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/sys/apr/APRBase.h"
#include "qpid/sys/apr/APRPool.h"

#include <boost/noncopyable.hpp>
#include <apr_thread_mutex.h>

namespace qpid {
namespace sys {

class Condition;

/**
 * Mutex lock.
 */
class Mutex : private boost::noncopyable {
  public:
    typedef ScopedLock<Mutex> ScopedLock;
    typedef ScopedUnlock<Mutex> ScopedUnlock;
    
    inline Mutex();
    inline ~Mutex();
    inline void lock();
    inline void unlock();
    inline bool trylock();

  protected:
    apr_thread_mutex_t* mutex;
  friend class Condition;
};

Mutex::Mutex() {
    CHECK_APR_SUCCESS(apr_thread_mutex_create(&mutex, APR_THREAD_MUTEX_NESTED, APRPool::get()));
}

Mutex::~Mutex(){
    CHECK_APR_SUCCESS(apr_thread_mutex_destroy(mutex));
}

void Mutex::lock() {
    CHECK_APR_SUCCESS(apr_thread_mutex_lock(mutex));
}
void Mutex::unlock() {
    CHECK_APR_SUCCESS(apr_thread_mutex_unlock(mutex));
}

bool Mutex::trylock() {
    return apr_thread_mutex_trylock(mutex) == 0;
}


/**
 * RW lock.
 */
class RWlock : private boost::noncopyable {
    friend class Condition;

public:
    typedef ScopedRlock<RWlock> ScopedRlock;
    typedef ScopedWlock<RWlock> ScopedWlock;
    
    inline RWlock();
    inline ~RWlock();
    inline void wlock();  // will write-lock
    inline void rlock();  // will read-lock
    inline void unlock();
    inline bool trywlock();  // will write-try
    inline bool tryrlock();  // will read-try

  protected:
    apr_thread_mutex_t* mutex;
};

RWlock::RWlock() {
    CHECK_APR_SUCCESS(apr_thread_mutex_create(&mutex, APR_THREAD_MUTEX_NESTED, APRPool::get()));
}

RWlock::~RWlock(){
    CHECK_APR_SUCCESS(apr_thread_mutex_destroy(mutex));
}

void RWlock::wlock() {
    CHECK_APR_SUCCESS(apr_thread_mutex_lock(mutex));
}

void RWlock::rlock() {
    CHECK_APR_SUCCESS(apr_thread_mutex_lock(mutex));
}

void RWlock::unlock() {
    CHECK_APR_SUCCESS(apr_thread_mutex_unlock(mutex));
}

bool RWlock::trywlock() {
    return apr_thread_mutex_trylock(mutex) == 0;
}

bool RWlock::tryrlock() {
    return apr_thread_mutex_trylock(mutex) == 0;
}


}}
#endif  /*!_sys_apr_Mutex_h*/
