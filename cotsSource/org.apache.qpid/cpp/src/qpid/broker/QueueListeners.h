#ifndef QPID_BROKER_QUEUELISTENERS_H
#define QPID_BROKER_QUEUELISTENERS_H

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
#include "qpid/broker/Consumer.h"
#include <vector>

namespace qpid {
namespace broker {

/**
 * Track and notify components that wish to be notified of messages
 * that become available on a queue.
 * 
 * None of the methods defined here are protected by locking. However
 * the populate method allows a 'snapshot' to be taken of the
 * listeners to be notified. NotificationSet::notify() may then be
 * called outside of any lock that protects the QueueListeners
 * instance from concurrent access.
 */
class QueueListeners
{
  public:
    typedef std::vector<Consumer::shared_ptr> Listeners;

    class NotificationSet
    {
      public:
        void notify();
      private:
        Listeners browsers;
        Consumer::shared_ptr consumer;
      friend class QueueListeners;
    };

    void addListener(Consumer::shared_ptr);    
    void removeListener(Consumer::shared_ptr);    
    void populate(NotificationSet&);
    bool contains(Consumer::shared_ptr c) const;

    template <class F> void eachListener(F f) {
        std::for_each(browsers.begin(), browsers.end(), f);
        std::for_each(consumers.begin(), consumers.end(), f);
    }

  private:
    Listeners consumers;
    Listeners browsers;

    void add(Listeners&, Consumer::shared_ptr);
    void remove(Listeners&, Consumer::shared_ptr);

};
}} // namespace qpid::broker

#endif  /*!QPID_BROKER_QUEUELISTENERS_H*/
