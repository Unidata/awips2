#ifndef _management_Mutex_h
#define _management_Mutex_h

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

#include "qpid/CommonImportExport.h"

namespace qpid {
    namespace sys {
        class Mutex;
    }

    namespace management {

        /**
         * Scoped lock template: calls lock() in ctor, unlock() in dtor.
         * L can be any class with lock() and unlock() functions.
         */
        template <class L> class ScopedLockTemplate {
        public:
            ScopedLockTemplate(L& l) : mutex(l) { mutex.lock(); }
            ~ScopedLockTemplate() { mutex.unlock(); }
        private:
            L& mutex;
        };

        template <class L> class ScopedUnlockTemplate {
        public:
            ScopedUnlockTemplate(L& l) : mutex(l) { mutex.unlock(); }
            ~ScopedUnlockTemplate() { mutex.lock(); }
        private:
            L& mutex;
        };

        class Mutex {
        public:
            typedef ScopedLockTemplate<Mutex> ScopedLock;
            typedef ScopedUnlockTemplate<Mutex> ScopedUnlock;

            QPID_COMMON_EXTERN Mutex();
            QPID_COMMON_EXTERN ~Mutex();
            QPID_COMMON_EXTERN void lock();
            QPID_COMMON_EXTERN void unlock();
        private:
            sys::Mutex* impl;
        };
    }
}

#endif

