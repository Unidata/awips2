#ifndef _sys_windows_Mutex_h
#define _sys_windows_Mutex_h

/*
 *
 * Copyright (c) 2008 The Apache Software Foundation
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

#include "qpid/sys/windows/check.h"

#include <boost/version.hpp>
#if (BOOST_VERSION < 103500)
#error The Windows port requires Boost version 1.35.0 or later
#endif

#include <boost/noncopyable.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/shared_mutex.hpp>
#include <boost/thread/thread_time.hpp>
#include <boost/thread/tss.hpp>

namespace qpid {
namespace sys {

class Condition;

/**
 * Mutex lock.
 */
class Mutex : private boost::noncopyable {
    friend class Condition;

public:
    typedef ::qpid::sys::ScopedLock<Mutex> ScopedLock;
    typedef ::qpid::sys::ScopedUnlock<Mutex> ScopedUnlock;
     
    inline Mutex();
    inline ~Mutex();
    inline void lock();  
    inline void unlock();
    inline bool trylock();  


protected:
    boost::recursive_mutex mutex;
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
    boost::shared_mutex rwMutex;
    boost::thread_specific_ptr<bool> haveWrite;

    inline bool &write (void);
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
    boost::recursive_mutex mutex;
};

#define QPID_MUTEX_INITIALIZER 0

void PODMutex::lock() {
    mutex.lock();
}

void PODMutex::unlock() {
    mutex.unlock();
}

bool PODMutex::trylock() {
    return mutex.try_lock();
}

Mutex::Mutex() {
}

Mutex::~Mutex(){
}

void Mutex::lock() {
    mutex.lock();
}

void Mutex::unlock() {
    mutex.unlock();
}

bool Mutex::trylock() {
    return mutex.try_lock();
}


RWlock::RWlock() {
}

RWlock::~RWlock(){
}

void RWlock::wlock() {
    bool &writer = write();
    rwMutex.lock();
    writer = true;    // Remember this thread has write lock held.
}

void RWlock::rlock() {
    bool &writer = write();
    rwMutex.lock_shared();
    writer = false;   // Remember this thread has shared lock held.
}

void RWlock::unlock() {
    bool &writer = write();
    if (writer)
        rwMutex.unlock();
    else
        rwMutex.unlock_shared();
}

void RWlock::trywlock() {
    bool &writer = write();
    // shared_mutex::try_lock() seems to not be available... emulate it with
    // a timed lock().
    boost::system_time now = boost::get_system_time();
    if (rwMutex.timed_lock(now))
        writer = true;
}

void RWlock::tryrlock() {
    bool &writer = write();
    if (rwMutex.try_lock_shared())
        writer = false;
}

bool & RWlock::write (void) {
    // Accessing thread-specific and stack-local info, so no locks needed.
    bool *writePtr = haveWrite.get();
    if (writePtr == 0) {
        writePtr = new bool(false);
        haveWrite.reset(writePtr);
    }
    return *writePtr;
}

}}
#endif  /*!_sys_windows_Mutex_h*/
