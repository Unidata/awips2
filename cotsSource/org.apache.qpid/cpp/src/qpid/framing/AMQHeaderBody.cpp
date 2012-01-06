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
#include "qpid/framing/AMQHeaderBody.h"
#include "qpid/Exception.h"
#include "qpid/log/Statement.h"

uint32_t qpid::framing::AMQHeaderBody::encodedSize() const {
    return properties.encodedSize();
}

void qpid::framing::AMQHeaderBody::encode(Buffer& buffer) const {
    properties.encode(buffer);
}

void qpid::framing::AMQHeaderBody::decode(Buffer& buffer, uint32_t size) {
    uint32_t limit = buffer.available() - size;
    while (buffer.available() > limit + 2) {
        uint32_t len = buffer.getLong();
        uint16_t type = buffer.getShort();
        if (!properties.decode(buffer, len, type)) {
            // TODO: should just skip & keep for later dispatch.
            throw Exception(QPID_MSG("Unexpected property type: " << type));
        }
    }
}

uint64_t qpid::framing::AMQHeaderBody::getContentLength() const
{    
    const MessageProperties* mProps = get<MessageProperties>();
    if (mProps) 
        return mProps->getContentLength();
    return 0;
}

void qpid::framing::AMQHeaderBody::print(std::ostream& out) const
{
    out << "header (" << encodedSize() << " bytes)";
    out << "; properties={";
    properties.print(out);
    out << "}";
}

void qpid::framing::AMQHeaderBody::accept(AMQBodyConstVisitor& v) const {
    v.visit(*this);
}
