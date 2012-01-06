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

#include "qpid/framing/SequenceNumber.h"
#include "qpid/framing/Buffer.h"
#include <ostream>

using qpid::framing::SequenceNumber;
using qpid::framing::Buffer;

void SequenceNumber::encode(Buffer& buffer) const
{
    buffer.putLong(value);
}

void SequenceNumber::decode(Buffer& buffer)
{
    value = buffer.getLong();
}

uint32_t SequenceNumber::encodedSize() const {
    return 4;
}

namespace qpid {
namespace framing {

std::ostream& operator<<(std::ostream& o, const SequenceNumber& n) {
    return o << n.getValue();
}

}}
