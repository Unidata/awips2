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

#include "qpid/client/MessageImpl.h"

namespace qpid {
namespace client {

MessageImpl::MessageImpl(const std::string& data, const std::string& routingKey) : TransferContent(data, routingKey) {}

std::string MessageImpl::getDestination() const 
{ 
    return method.getDestination(); 
}

bool MessageImpl::isRedelivered() const 
{ 
    return hasDeliveryProperties() && getDeliveryProperties().getRedelivered(); 
}

void MessageImpl::setRedelivered(bool redelivered) 
{ 
    getDeliveryProperties().setRedelivered(redelivered); 
}

framing::FieldTable& MessageImpl::getHeaders() 
{ 
    return getMessageProperties().getApplicationHeaders(); 
}

const framing::FieldTable& MessageImpl::getHeaders() const
{ 
    return getMessageProperties().getApplicationHeaders(); 
}

const framing::MessageTransferBody& MessageImpl::getMethod() const
{
    return method;
}

const framing::SequenceNumber& MessageImpl::getId() const
{
    return id;
}

/**@internal for incoming messages */
MessageImpl::MessageImpl(const framing::FrameSet& frameset) :
    method(*frameset.as<framing::MessageTransferBody>()), id(frameset.getId())
{
    populate(frameset);
}

}}
