#ifndef _sys_DeletionManager_h
#define _sys_DeletionManager_h

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

#include <vector>
#include <algorithm>
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace sys {

struct deleter
{
  template <typename T>
  void operator()(T* ptr){ delete ptr;}
};

/**
 * DeletionManager keeps track of handles that need to be deleted but may still be
 * in use by one of the threads concurrently.
 * 
 * The mode of operation is like this:
 * - When we want to delete but we might still be using the handle we
 *   * Transfer ownership of the handle to this class
 *   * Mark the handle as (potentially) in use by every thread
 * - Then subsequently at points where the thread code knows it isn't
 *   using any handles it declares that it is using no handles
 * - When the last thread declares no use of a handle it automatically
 *   gets deleted by the shared_ptr implementation
 * 
 * The class only has static members and data and so can only be used once for
 * any particular handle type
 */
template <typename H>
class DeletionManager 
{
    struct ThreadStatus;

public:
    // Mark every thread as using the handle - it will be deleted
    // below after every thread marks the handle as unused
    static void markForDeletion(H* handle) {
        allThreadsStatuses.addHandle(shared_ptr(handle));
    }
    
    // Mark this thread is not using any handle -
    // handles get deleted here when no one else
    // is using them either 
    static void markAllUnusedInThisThread() {
        ThreadStatus* threadStatus = getThreadStatus();
        ScopedLock<Mutex> l(threadStatus->lock);

        // The actual deletions will happen here when all the shared_ptr
        // ref counts hit 0 (that is when every thread marks the handle unused)
        threadStatus->handles.clear();
    }

    static void destroyThreadState() {
        ThreadStatus* threadStatus = getThreadStatus();
        allThreadsStatuses.delThreadStatus(threadStatus);
        delete threadStatus;
        threadStatus = 0;
    }

private:

    static ThreadStatus*& getThreadStatus() {
        static __thread ThreadStatus* threadStatus = 0;

        // Thread local vars can't be dynamically constructed so we need
        // to check whether we've made it yet and construct it if not
        // (no locking necessary for the check as it's thread local!)
        if (!threadStatus) {
            threadStatus = new ThreadStatus;
            allThreadsStatuses.addThreadStatus(threadStatus);
        }

        return threadStatus;
    }

    typedef boost::shared_ptr<H> shared_ptr;

    // In theory we know that we never need more handles than the number of
    // threads runnning so we could use a fixed size array. However at this point
    // in the code we don't have easy access to this information. 
    struct ThreadStatus
    {
        Mutex lock;
        std::vector<shared_ptr> handles;
    };

    class AllThreadsStatuses
    {
        Mutex lock;
        std::vector<ThreadStatus*> statuses;

        struct handleAdder
        {
            shared_ptr handle;

            handleAdder(shared_ptr h): handle(h) {}
        
            void operator()(ThreadStatus* ptr) {
                ScopedLock<Mutex> l(ptr->lock);
                ptr->handles.push_back(handle);
            }
        };

    public:
        // Need this to be able to do static initialisation
        explicit AllThreadsStatuses(int) {}

        ~AllThreadsStatuses() {
            ScopedLock<Mutex> l(lock);
            std::for_each(statuses.begin(), statuses.end(), deleter());
        }

        void addThreadStatus(ThreadStatus* t) {
            ScopedLock<Mutex> l(lock);
            statuses.push_back(t);
        }

        void delThreadStatus(ThreadStatus* t) {
            ScopedLock<Mutex> l(lock);
            typename std::vector<ThreadStatus*>::iterator it =
                std::find(statuses.begin(),statuses.end(), t);
            if (it != statuses.end()) {
                statuses.erase(it);
            }
        }

        void addHandle(shared_ptr h) {
            ScopedLock<Mutex> l(lock);
            std::for_each(statuses.begin(), statuses.end(), handleAdder(h));
        }
    };
    
    static AllThreadsStatuses allThreadsStatuses;
};

}}
#endif // _sys_DeletionManager_h
