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
#ifndef _RecoveredEnqueue_
#define _RecoveredEnqueue_

#include "qpid/broker/Deliverable.h"
#include "qpid/broker/Message.h"
#include "qpid/broker/MessageStore.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/TxOp.h"

#include <boost/intrusive_ptr.hpp>

#include <algorithm>
#include <functional>
#include <list>

namespace qpid {
    namespace broker {
        class RecoveredEnqueue : public TxOp{
            Queue::shared_ptr queue;
            boost::intrusive_ptr<Message> msg;

        public:
            RecoveredEnqueue(Queue::shared_ptr queue, boost::intrusive_ptr<Message> msg);
            virtual bool prepare(TransactionContext* ctxt) throw();
            virtual void commit() throw();
            virtual void rollback() throw();
            virtual ~RecoveredEnqueue(){}
            virtual void accept(TxOpConstVisitor& visitor) const { visitor(*this); }

            Queue::shared_ptr getQueue() const { return queue; }
            boost::intrusive_ptr<Message> getMessage() const { return msg; }
            
        };
    }
}


#endif
