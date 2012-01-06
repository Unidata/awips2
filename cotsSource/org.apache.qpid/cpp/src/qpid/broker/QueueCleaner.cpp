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
#include "qpid/broker/QueueCleaner.h"

#include "qpid/broker/Broker.h"
#include <boost/bind.hpp>

namespace qpid {
namespace broker {

QueueCleaner::QueueCleaner(QueueRegistry& q, sys::Timer& t) : queues(q), timer(t) {}

QueueCleaner::~QueueCleaner()
{
    if (task) task->cancel();
}

void QueueCleaner::start(qpid::sys::Duration p)
{
    task = new Task(*this, p);
    timer.add(task);
}

QueueCleaner::Task::Task(QueueCleaner& p, qpid::sys::Duration d) : sys::TimerTask(d), parent(p) {}

void QueueCleaner::Task::fire()
{
    parent.fired();
}

void QueueCleaner::fired()
{
    queues.eachQueue(boost::bind(&Queue::purgeExpired, _1));
    task->setupNextFire();
    timer.add(task);
}


}} // namespace qpid::broker
