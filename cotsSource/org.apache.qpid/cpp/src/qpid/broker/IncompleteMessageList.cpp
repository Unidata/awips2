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

#include "qpid/broker/IncompleteMessageList.h"

namespace qpid {
namespace broker {

IncompleteMessageList::IncompleteMessageList() :
    callback(boost::bind(&IncompleteMessageList::enqueueComplete, this, _1))
{}

IncompleteMessageList::~IncompleteMessageList() 
{
    sys::Mutex::ScopedLock l(lock);
    for (Messages::iterator i = incomplete.begin(); i != incomplete.end(); ++i) {
        (*i)->resetEnqueueCompleteCallback();
        (*i)->resetDequeueCompleteCallback();
    }
}

void IncompleteMessageList::add(boost::intrusive_ptr<Message> msg)
{
    sys::Mutex::ScopedLock l(lock);
    msg->setEnqueueCompleteCallback(callback);
    incomplete.push_back(msg);
}

void IncompleteMessageList::enqueueComplete(const boost::intrusive_ptr<Message>& ) {
    sys::Mutex::ScopedLock l(lock);
    lock.notify();
}

void IncompleteMessageList::process(const CompletionListener& listen, bool sync)
{
    sys::Mutex::ScopedLock l(lock);
    while (!incomplete.empty()) {
        boost::intrusive_ptr<Message>& msg = incomplete.front();
        if (!msg->isEnqueueComplete()) {
            if (sync){
                {
                    sys::Mutex::ScopedUnlock u(lock);
                    msg->flush(); // Can re-enter IncompleteMessageList::enqueueComplete
                }
                while (!msg->isEnqueueComplete())
                    lock.wait();
            } else {
                //leave the message as incomplete for now
                return;
            }            
        }
        listen(msg);
        incomplete.pop_front();
    }
}

void IncompleteMessageList::each(const CompletionListener& listen) {
    Messages snapshot;
    {
        sys::Mutex::ScopedLock l(lock);
        snapshot = incomplete;
    }
    std::for_each(incomplete.begin(), incomplete.end(), listen); // FIXME aconway 2008-11-07: passed by ref or value?
}

}}
