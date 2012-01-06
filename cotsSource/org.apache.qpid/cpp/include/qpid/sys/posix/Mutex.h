#ifndef _sys_posix_Mutex_h
#define _sys_posix_Mutex_h

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

#include "qpid/sys/posix/check.h"

#include <pthread.h>
#include <boost/noncopyable.hpp>

namespace qpid {
namespace sys {

class Condition;

/**
 * Mutex lock.
 */
class Mutex : private boost::noncopyable {
    friend class Condition;
    static const pthread_mutexattr_t* getAttribute();

public:
    typedef ::qpid::sys::ScopedLock<Mutex> ScopedLock;
    typedef ::qpid::sys::ScopedUnlock<Mutex> ScopedUnlock;

    inline Mutex();
    inline ~Mutex();
    inline void lock();
    inline void unlock();
    inline bool trylock();


protected:
    pthread_mutex_t mutex;
};

/**
 * RW lock.
 */
class RWlock : private boost::noncopyable {
    friend class Condition;

public:
    typedef ::qpid::sys::ScopedRlock<RWlock> ScopedRlock;
    typedef ::qpid::sys::ScopedWlock<RWlock> ScopedWlock;

    inline RWlock();
    inline ~RWlock();
    inline void wlock();  // will write-lock
    inline void rlock();  // will read-lock
    inline void unlock();
    inline void trywlock();  // will write-try
    inline void tryrlock();  // will read-try

protected:
    pthread_rwlock_t rwlock;
};


/**
 * PODMutex is a POD, can be static-initialized with
 * PODMutex m = QPID_PODMUTEX_INITIALIZER
 */
struct PODMutex
{
    typedef ::qpid::sys::ScopedLock<PODMutex> ScopedLock;

    inline void lock();
    inline void unlock();
    inline bool trylock();

    // Must be public to be a POD:
    pthread_mutex_t mutex;
};

#define QPID_MUTEX_INITIALIZER { PTHREAD_MUTEX_INITIALIZER }

void PODMutex::lock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_mutex_lock(&mutex));
}

void PODMutex::unlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_mutex_unlock(&mutex));
}

bool PODMutex::trylock() {
    return pthread_mutex_trylock(&mutex) == 0;
}

Mutex::Mutex() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_mutex_init(&mutex, getAttribute()));
}

Mutex::~Mutex(){
    QPID_POSIX_ABORT_IF(pthread_mutex_destroy(&mutex));
}

void Mutex::lock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_mutex_lock(&mutex));
}

void Mutex::unlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_mutex_unlock(&mutex));
}

bool Mutex::trylock() {
    return pthread_mutex_trylock(&mutex) == 0;
}


RWlock::RWlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_rwlock_init(&rwlock, NULL));
}

RWlock::~RWlock(){
    QPID_POSIX_ABORT_IF(pthread_rwlock_destroy(&rwlock));
}

void RWlock::wlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_rwlock_wrlock(&rwlock));
}

void RWlock::rlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_rwlock_rdlock(&rwlock));
}

void RWlock::unlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_rwlock_unlock(&rwlock));
}

void RWlock::trywlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_rwlock_trywrlock(&rwlock));
}

void RWlock::tryrlock() {
    QPID_POSIX_ASSERT_THROW_IF(pthread_rwlock_tryrdlock(&rwlock));
}


}}
#endif  /*!_sys_posix_Mutex_h*/
