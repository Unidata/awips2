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
#include "MessageImpl.h"
#include "qpid/messaging/Message.h"

namespace qpid {
namespace messaging {

namespace {
const std::string EMPTY_STRING = "";
}

MessageImpl::MessageImpl(const std::string& c) : bytes(c), internalId(0) {}
MessageImpl::MessageImpl(const char* chars, size_t count) : bytes(chars, count), internalId(0) {}

void MessageImpl::setReplyTo(const Address& d) { replyTo = d; }
const Address& MessageImpl::getReplyTo() const { return replyTo; }

void MessageImpl::setSubject(const std::string& s) { subject = s; }
const std::string& MessageImpl::getSubject() const { return subject; }

void MessageImpl::setContentType(const std::string& s) { contentType = s; }
const std::string& MessageImpl::getContentType() const { return contentType; }

const VariantMap& MessageImpl::getHeaders() const { return headers; }
VariantMap& MessageImpl::getHeaders() { return headers; }

//should these methods be on MessageContent?
void MessageImpl::setBytes(const std::string& c) { bytes = c; }
void MessageImpl::setBytes(const char* chars, size_t count) { bytes.assign(chars, count); }
const std::string& MessageImpl::getBytes() const { return bytes; }
std::string& MessageImpl::getBytes() { return bytes; }

void MessageImpl::setInternalId(qpid::framing::SequenceNumber i) { internalId = i; }
qpid::framing::SequenceNumber MessageImpl::getInternalId() { return internalId; }

MessageImpl& MessageImplAccess::get(Message& msg)
{
    return *msg.impl;
}
const MessageImpl& MessageImplAccess::get(const Message& msg)
{
    return *msg.impl;
}

}} // namespace qpid::messaging
