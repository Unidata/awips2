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

#include "qpid/framing/TransferContent.h"

namespace qpid {
namespace framing {

TransferContent::TransferContent(const std::string& data, const std::string& key) {
    setData(data);
    if (!key.empty()) getDeliveryProperties().setRoutingKey(key);
}


AMQHeaderBody TransferContent::getHeader() const
{
    return header;
}

const std::string& TransferContent::getData() const {
    return data;
}

std::string& TransferContent::getData() {
    return data;
}

void TransferContent::setData(const std::string& _data)
{
    data = _data;
    header.get<MessageProperties>(true)->setContentLength(data.size());
}

void TransferContent::appendData(const std::string& _data)
{
    data += _data;
    header.get<MessageProperties>(true)->setContentLength(data.size());
}

MessageProperties& TransferContent::getMessageProperties()
{
    return *header.get<MessageProperties>(true);
}

DeliveryProperties& TransferContent::getDeliveryProperties()
{
    return *header.get<DeliveryProperties>(true);
}

void TransferContent::populate(const FrameSet& frameset)
{
    const AMQHeaderBody* h = frameset.getHeaders();
    if (h) {
        header = *h;
    }
    frameset.getContent(data);
}

const MessageProperties& TransferContent::getMessageProperties() const
{
    const MessageProperties* props = header.get<MessageProperties>();
    if (!props) throw Exception("No message properties.");
    return *props;
}

const DeliveryProperties& TransferContent::getDeliveryProperties() const
{
    const DeliveryProperties* props = header.get<DeliveryProperties>();
    if (!props) throw Exception("No message properties.");
    return *props;
}

bool TransferContent::hasMessageProperties() const
{
    return header.get<MessageProperties>();
}

bool TransferContent::hasDeliveryProperties() const
{
    return header.get<DeliveryProperties>();
}


}}
