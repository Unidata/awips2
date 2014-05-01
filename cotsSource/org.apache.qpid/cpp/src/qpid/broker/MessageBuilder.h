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
#ifndef _MessageBuilder_
#define _MessageBuilder_

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/framing/FrameHandler.h"
#include "qpid/framing/SequenceNumber.h"
#include "qpid/RefCounted.h"

#include <boost/intrusive_ptr.hpp>

namespace qpid {
    namespace broker {
        class Message;
        class MessageStore;

        class MessageBuilder : public framing::FrameHandler{
        public:
            QPID_BROKER_EXTERN MessageBuilder(MessageStore* const store,
                                              uint64_t stagingThreshold);
            QPID_BROKER_EXTERN void handle(framing::AMQFrame& frame);
            boost::intrusive_ptr<Message> getMessage() { return message; }
            QPID_BROKER_EXTERN void start(const framing::SequenceNumber& id);
            void end();
        private:
            enum State {DORMANT, METHOD, HEADER, CONTENT};
            State state;
            boost::intrusive_ptr<Message> message;
            MessageStore* const store;
            const uint64_t stagingThreshold;
            bool staging;

            void checkType(uint8_t expected, uint8_t actual);
        };
    }
}


#endif

