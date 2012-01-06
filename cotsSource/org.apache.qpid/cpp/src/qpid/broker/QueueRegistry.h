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
#ifndef _QueueRegistry_
#define _QueueRegistry_

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/Queue.h"
#include "qpid/sys/Mutex.h"
#include "qpid/management/Manageable.h"
#include <boost/bind.hpp>
#include <algorithm>
#include <map>

namespace qpid {
namespace broker {

class QueueEvents;

/**
 * A registry of queues indexed by queue name.
 *
 * Queues are reference counted using shared_ptr to ensure that they
 * are deleted when and only when they are no longer in use.
 *
 */
class QueueRegistry {
  public:
    QPID_BROKER_EXTERN QueueRegistry(Broker* b = 0);
    QPID_BROKER_EXTERN ~QueueRegistry();

    /**
     * Declare a queue.
     *
     * @return The queue and a boolean flag which is true if the queue
     * was created by this declare call false if it already existed.
     */
    QPID_BROKER_EXTERN std::pair<Queue::shared_ptr, bool> declare
      (const string& name,
       bool durable = false,
       bool autodelete = false, 
       const OwnershipToken* owner = 0);

    /**
     * Destroy the named queue.
     *
     * Note: if the queue is in use it is not actually destroyed until
     * all shared_ptrs to it are destroyed. During that time it is
     * possible that a new queue with the same name may be
     * created. This should not create any problems as the new and
     * old queues exist independently. The registry has
     * forgotten the old queue so there can be no confusion for
     * subsequent calls to find or declare with the same name.
     *
     */
    QPID_BROKER_EXTERN void destroy(const string& name);
    template <class Test> bool destroyIf(const string& name, Test test)
    {
        qpid::sys::RWlock::ScopedWlock locker(lock);
        if (test()) {
            destroyLH (name);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Find the named queue. Return 0 if not found.
     */
    QPID_BROKER_EXTERN Queue::shared_ptr find(const string& name);

    /**
     * Generate unique queue name.
     */
    string generateName();

    void setQueueEvents(QueueEvents*);

    /**
     * Set the store to use.  May only be called once.
     */
    void setStore (MessageStore*);

    /**
     * Return the message store used.
     */
    MessageStore* getStore() const;

    /**
     * Register the manageable parent for declared queues
     */
    void setParent (management::Manageable* _parent) { parent = _parent; }

    /** Call f for each queue in the registry. */
    template <class F> void eachQueue(F f) const {
        qpid::sys::RWlock::ScopedRlock l(lock);
        for (QueueMap::const_iterator i = queues.begin(); i != queues.end(); ++i)
            f(i->second);
    }
	
	/**
	* Change queue mode when cluster size drops to 1 node, expands again
	* in practice allows flow queue to disk when last name to be exectuted
	*/
	void updateQueueClusterState(bool lastNode);
    
private:
    typedef std::map<string, Queue::shared_ptr> QueueMap;
    QueueMap queues;
    mutable qpid::sys::RWlock lock;
    int counter;
    MessageStore* store;
    QueueEvents* events;
    management::Manageable* parent;
    bool lastNode; //used to set mode on queue declare
    Broker* broker;

    //destroy impl that assumes lock is already held:
    void destroyLH (const string& name);
};

    
}} // namespace qpid::broker


#endif
