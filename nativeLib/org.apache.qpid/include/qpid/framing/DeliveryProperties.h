#ifndef QPID_FRAMING_DELIVERYPROPERTIES_H
#define QPID_FRAMING_DELIVERYPROPERTIES_H
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

class DeliveryProperties  {
    uint8_t priority;
    uint8_t deliveryMode;
    uint64_t ttl;
    uint64_t timestamp;
    uint64_t expiration;
    string exchange;
    string routingKey;
    string resumeId;
    uint64_t resumeTtl;
    uint16_t flags;
public:
    static const uint16_t TYPE = 1025;
    DeliveryProperties(
        bool _discardUnroutable,
        bool _immediate,
        bool _redelivered,
        uint8_t _priority,
        uint8_t _deliveryMode,
        uint64_t _ttl,
        uint64_t _timestamp,
        uint64_t _expiration,
        const string& _exchange,
        const string& _routingKey,
        const string& _resumeId,
        uint64_t _resumeTtl) : 
        priority(_priority),
        deliveryMode(_deliveryMode),
        ttl(_ttl),
        timestamp(_timestamp),
        expiration(_expiration),
        exchange(_exchange),
        routingKey(_routingKey),
        resumeId(_resumeId),
        resumeTtl(_resumeTtl),
        flags(0){
        setDiscardUnroutable(_discardUnroutable);
        setImmediate(_immediate);
        setRedelivered(_redelivered);
        flags |= (1 << 11);
        flags |= (1 << 12);
        flags |= (1 << 13);
        flags |= (1 << 14);
        flags |= (1 << 15);
        flags |= (1 << 0);
        flags |= (1 << 1);
        flags |= (1 << 2);
        flags |= (1 << 3);
    }
    DeliveryProperties()  : priority(0), deliveryMode(0), ttl(0), timestamp(0), expiration(0), resumeTtl(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setDiscardUnroutable(bool _discardUnroutable);
    QPID_COMMON_EXTERN bool getDiscardUnroutable() const;
    QPID_COMMON_EXTERN void setImmediate(bool _immediate);
    QPID_COMMON_EXTERN bool getImmediate() const;
    QPID_COMMON_EXTERN void setRedelivered(bool _redelivered);
    QPID_COMMON_EXTERN bool getRedelivered() const;
    QPID_COMMON_EXTERN void setPriority(uint8_t _priority);
    QPID_COMMON_EXTERN uint8_t getPriority() const;
    QPID_COMMON_EXTERN bool hasPriority() const;
    QPID_COMMON_EXTERN void clearPriorityFlag();
    QPID_COMMON_EXTERN void setDeliveryMode(uint8_t _deliveryMode);
    QPID_COMMON_EXTERN uint8_t getDeliveryMode() const;
    QPID_COMMON_EXTERN bool hasDeliveryMode() const;
    QPID_COMMON_EXTERN void clearDeliveryModeFlag();
    QPID_COMMON_EXTERN void setTtl(uint64_t _ttl);
    QPID_COMMON_EXTERN uint64_t getTtl() const;
    QPID_COMMON_EXTERN bool hasTtl() const;
    QPID_COMMON_EXTERN void clearTtlFlag();
    QPID_COMMON_EXTERN void setTimestamp(uint64_t _timestamp);
    QPID_COMMON_EXTERN uint64_t getTimestamp() const;
    QPID_COMMON_EXTERN bool hasTimestamp() const;
    QPID_COMMON_EXTERN void clearTimestampFlag();
    QPID_COMMON_EXTERN void setExpiration(uint64_t _expiration);
    QPID_COMMON_EXTERN uint64_t getExpiration() const;
    QPID_COMMON_EXTERN bool hasExpiration() const;
    QPID_COMMON_EXTERN void clearExpirationFlag();
    QPID_COMMON_EXTERN void setExchange(const string& _exchange);
    QPID_COMMON_EXTERN const string& getExchange() const;
    QPID_COMMON_EXTERN bool hasExchange() const;
    QPID_COMMON_EXTERN void clearExchangeFlag();
    QPID_COMMON_EXTERN void setRoutingKey(const string& _routingKey);
    QPID_COMMON_EXTERN const string& getRoutingKey() const;
    QPID_COMMON_EXTERN bool hasRoutingKey() const;
    QPID_COMMON_EXTERN void clearRoutingKeyFlag();
    QPID_COMMON_EXTERN void setResumeId(const string& _resumeId);
    QPID_COMMON_EXTERN const string& getResumeId() const;
    QPID_COMMON_EXTERN bool hasResumeId() const;
    QPID_COMMON_EXTERN void clearResumeIdFlag();
    QPID_COMMON_EXTERN void setResumeTtl(uint64_t _resumeTtl);
    QPID_COMMON_EXTERN uint64_t getResumeTtl() const;
    QPID_COMMON_EXTERN bool hasResumeTtl() const;
    QPID_COMMON_EXTERN void clearResumeTtlFlag();
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const DeliveryProperties&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class DeliveryProperties */

}}
#endif  /*!QPID_FRAMING_DELIVERYPROPERTIES_H*/
