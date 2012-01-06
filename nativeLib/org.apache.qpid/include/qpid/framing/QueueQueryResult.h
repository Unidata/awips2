#ifndef QPID_FRAMING_QUEUEQUERYRESULT_H
#define QPID_FRAMING_QUEUEQUERYRESULT_H
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

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///



#include <ostream>
#include "qpid/framing/amqp_types_full.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class QueueQueryResult  {
    string queue;
    string alternateExchange;
    FieldTable arguments;
    uint32_t messageCount;
    uint32_t subscriberCount;
    uint16_t flags;
public:
    static const uint16_t TYPE = 2049;
    QueueQueryResult(
        const string& _queue,
        const string& _alternateExchange,
        bool _durable,
        bool _exclusive,
        bool _autoDelete,
        const FieldTable& _arguments,
        uint32_t _messageCount,
        uint32_t _subscriberCount) : 
        queue(_queue),
        alternateExchange(_alternateExchange),
        arguments(_arguments),
        messageCount(_messageCount),
        subscriberCount(_subscriberCount),
        flags(0){
        setDurable(_durable);
        setExclusive(_exclusive);
        setAutoDelete(_autoDelete);
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 13);
        flags |= (1 << 14);
        flags |= (1 << 15);
    }
    QueueQueryResult()  : messageCount(0), subscriberCount(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setQueue(const string& _queue);
    QPID_COMMON_EXTERN const string& getQueue() const;
    QPID_COMMON_EXTERN bool hasQueue() const;
    QPID_COMMON_EXTERN void clearQueueFlag();
    QPID_COMMON_EXTERN void setAlternateExchange(const string& _alternateExchange);
    QPID_COMMON_EXTERN const string& getAlternateExchange() const;
    QPID_COMMON_EXTERN bool hasAlternateExchange() const;
    QPID_COMMON_EXTERN void clearAlternateExchangeFlag();
    QPID_COMMON_EXTERN void setDurable(bool _durable);
    QPID_COMMON_EXTERN bool getDurable() const;
    QPID_COMMON_EXTERN void setExclusive(bool _exclusive);
    QPID_COMMON_EXTERN bool getExclusive() const;
    QPID_COMMON_EXTERN void setAutoDelete(bool _autoDelete);
    QPID_COMMON_EXTERN bool getAutoDelete() const;
    QPID_COMMON_EXTERN void setArguments(const FieldTable& _arguments);
    QPID_COMMON_EXTERN const FieldTable& getArguments() const;
    QPID_COMMON_EXTERN FieldTable& getArguments();
    QPID_COMMON_EXTERN bool hasArguments() const;
    QPID_COMMON_EXTERN void clearArgumentsFlag();
    QPID_COMMON_EXTERN void setMessageCount(uint32_t _messageCount);
    QPID_COMMON_EXTERN uint32_t getMessageCount() const;
    QPID_COMMON_EXTERN bool hasMessageCount() const;
    QPID_COMMON_EXTERN void clearMessageCountFlag();
    QPID_COMMON_EXTERN void setSubscriberCount(uint32_t _subscriberCount);
    QPID_COMMON_EXTERN uint32_t getSubscriberCount() const;
    QPID_COMMON_EXTERN bool hasSubscriberCount() const;
    QPID_COMMON_EXTERN void clearSubscriberCountFlag();
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const QueueQueryResult&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class QueueQueryResult */

}}
#endif  /*!QPID_FRAMING_QUEUEQUERYRESULT_H*/
