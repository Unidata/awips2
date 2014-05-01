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
#ifndef _DeliverableMessage_
#define _DeliverableMessage_

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/Deliverable.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/Message.h"

#include <boost/intrusive_ptr.hpp>

namespace qpid {
    namespace broker {
        class DeliverableMessage : public Deliverable{
            boost::intrusive_ptr<Message> msg;
        public:
            QPID_BROKER_EXTERN DeliverableMessage(const boost::intrusive_ptr<Message>& msg);
            QPID_BROKER_EXTERN virtual void deliverTo(const boost::shared_ptr<Queue>& queue);
            QPID_BROKER_EXTERN Message& getMessage();
            QPID_BROKER_EXTERN uint64_t contentSize();
            virtual ~DeliverableMessage(){}
        };
    }
}


#endif
