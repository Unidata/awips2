#ifndef QPID_BROKER_QUEUECLEANER_H
#define QPID_BROKER_QUEUECLEANER_H

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

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/sys/Timer.h"

namespace qpid {
namespace broker {

class QueueRegistry;
/**
 * TimerTask to purge expired messages from queues
 */
class QueueCleaner
{
  public:
    QPID_BROKER_EXTERN QueueCleaner(QueueRegistry& queues, sys::Timer& timer);
    QPID_BROKER_EXTERN ~QueueCleaner();
    QPID_BROKER_EXTERN void start(qpid::sys::Duration period);
  private:
    class Task : public sys::TimerTask
    {
      public:
        Task(QueueCleaner& parent, qpid::sys::Duration duration);
        void fire();
      private:
        QueueCleaner& parent;
    };

    boost::intrusive_ptr<sys::TimerTask> task;
    QueueRegistry& queues;
    sys::Timer& timer;

    void fired();
};
}} // namespace qpid::broker

#endif  /*!QPID_BROKER_QUEUECLEANER_H*/
