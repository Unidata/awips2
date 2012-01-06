#ifndef QPID_FRAMING_MESSAGEPROPERTIES_H
#define QPID_FRAMING_MESSAGEPROPERTIES_H
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


#include "qpid/framing/ReplyTo.h"

#include <ostream>
#include "qpid/framing/amqp_types_full.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class MessageProperties  {
    uint64_t contentLength;
    Uuid messageId;
    string correlationId;
    ReplyTo replyTo;
    string contentType;
    string contentEncoding;
    string userId;
    string appId;
    FieldTable applicationHeaders;
    uint16_t flags;
public:
    static const uint16_t TYPE = 1027;
    MessageProperties(
        uint64_t _contentLength,
        const Uuid& _messageId,
        const string& _correlationId,
        const ReplyTo& _replyTo,
        const string& _contentType,
        const string& _contentEncoding,
        const string& _userId,
        const string& _appId,
        const FieldTable& _applicationHeaders) : 
        contentLength(_contentLength),
        messageId(_messageId),
        correlationId(_correlationId),
        replyTo(_replyTo),
        contentType(_contentType),
        contentEncoding(_contentEncoding),
        userId(_userId),
        appId(_appId),
        applicationHeaders(_applicationHeaders),
        flags(0){
        flags |= (1 << 8);
        flags |= (1 << 9);
        flags |= (1 << 10);
        flags |= (1 << 11);
        flags |= (1 << 12);
        flags |= (1 << 13);
        flags |= (1 << 14);
        flags |= (1 << 15);
        flags |= (1 << 0);
    }
    MessageProperties()  : contentLength(0), flags(0) {}
    
    QPID_COMMON_EXTERN void setContentLength(uint64_t _contentLength);
    QPID_COMMON_EXTERN uint64_t getContentLength() const;
    QPID_COMMON_EXTERN bool hasContentLength() const;
    QPID_COMMON_EXTERN void clearContentLengthFlag();
    QPID_COMMON_EXTERN void setMessageId(const Uuid& _messageId);
    QPID_COMMON_EXTERN const Uuid& getMessageId() const;
    QPID_COMMON_EXTERN bool hasMessageId() const;
    QPID_COMMON_EXTERN void clearMessageIdFlag();
    QPID_COMMON_EXTERN void setCorrelationId(const string& _correlationId);
    QPID_COMMON_EXTERN const string& getCorrelationId() const;
    QPID_COMMON_EXTERN bool hasCorrelationId() const;
    QPID_COMMON_EXTERN void clearCorrelationIdFlag();
    QPID_COMMON_EXTERN void setReplyTo(const ReplyTo& _replyTo);
    QPID_COMMON_EXTERN const ReplyTo& getReplyTo() const;
    QPID_COMMON_EXTERN bool hasReplyTo() const;
    QPID_COMMON_EXTERN void clearReplyToFlag();
    QPID_COMMON_EXTERN void setContentType(const string& _contentType);
    QPID_COMMON_EXTERN const string& getContentType() const;
    QPID_COMMON_EXTERN bool hasContentType() const;
    QPID_COMMON_EXTERN void clearContentTypeFlag();
    QPID_COMMON_EXTERN void setContentEncoding(const string& _contentEncoding);
    QPID_COMMON_EXTERN const string& getContentEncoding() const;
    QPID_COMMON_EXTERN bool hasContentEncoding() const;
    QPID_COMMON_EXTERN void clearContentEncodingFlag();
    QPID_COMMON_EXTERN void setUserId(const string& _userId);
    QPID_COMMON_EXTERN const string& getUserId() const;
    QPID_COMMON_EXTERN bool hasUserId() const;
    QPID_COMMON_EXTERN void clearUserIdFlag();
    QPID_COMMON_EXTERN void setAppId(const string& _appId);
    QPID_COMMON_EXTERN const string& getAppId() const;
    QPID_COMMON_EXTERN bool hasAppId() const;
    QPID_COMMON_EXTERN void clearAppIdFlag();
    QPID_COMMON_EXTERN void setApplicationHeaders(const FieldTable& _applicationHeaders);
    QPID_COMMON_EXTERN const FieldTable& getApplicationHeaders() const;
    QPID_COMMON_EXTERN FieldTable& getApplicationHeaders();
    QPID_COMMON_EXTERN bool hasApplicationHeaders() const;
    QPID_COMMON_EXTERN void clearApplicationHeadersFlag();
    QPID_COMMON_EXTERN friend std::ostream& operator<<(std::ostream&, const MessageProperties&);
    QPID_COMMON_EXTERN void encode(Buffer&) const;
    QPID_COMMON_EXTERN void decode(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN void encodeStructBody(Buffer&) const;
    QPID_COMMON_EXTERN void decodeStructBody(Buffer&, uint32_t=0);
    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN uint32_t bodySize() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
}; /* class MessageProperties */

}}
#endif  /*!QPID_FRAMING_MESSAGEPROPERTIES_H*/
