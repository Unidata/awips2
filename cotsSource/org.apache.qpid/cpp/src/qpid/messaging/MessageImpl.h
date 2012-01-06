#ifndef QPID_MESSAGING_MESSAGEIMPL_H
#define QPID_MESSAGING_MESSAGEIMPL_H

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
#include "qpid/messaging/Address.h"
#include "qpid/messaging/Variant.h"
#include "qpid/framing/SequenceNumber.h"

namespace qpid {
namespace messaging {

struct MessageImpl
{
    Address replyTo;
    std::string subject;
    std::string contentType;
    Variant::Map headers;

    std::string bytes;

    qpid::framing::SequenceNumber internalId;

    MessageImpl(const std::string& c);
    MessageImpl(const char* chars, size_t count);

    void setReplyTo(const Address& d);
    const Address& getReplyTo() const;
    
    void setSubject(const std::string& s);
    const std::string& getSubject() const;
    
    void setContentType(const std::string& s);
    const std::string& getContentType() const;
    
    const Variant::Map& getHeaders() const;
    Variant::Map& getHeaders();
    
    void setBytes(const std::string& bytes);
    void setBytes(const char* chars, size_t count);
    const std::string& getBytes() const;
    std::string& getBytes();

    void setInternalId(qpid::framing::SequenceNumber id);
    qpid::framing::SequenceNumber getInternalId();

};

class Message;

/**
 * Provides access to the internal MessageImpl for a message which is
 * useful when accessing any message state not exposed directly
 * through the public API.
 */
struct MessageImplAccess
{
    static MessageImpl& get(Message&);
    static const MessageImpl& get(const Message&);
};

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_MESSAGEIMPL_H*/
