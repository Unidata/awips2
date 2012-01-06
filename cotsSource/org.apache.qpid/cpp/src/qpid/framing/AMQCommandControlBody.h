#ifndef QPID_FRAMING_AMQCOMMANDCONTROLBODY_H
#define QPID_FRAMING_AMQCOMMANDCONTROLBODY_H

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

#include "qpid/amqp_0_10/helpers.h"
#include "qpid/framing/AMQBody.h"

namespace qpid {
namespace framing {

/**
 * AMQBody wrapper for Command and Control.
 * Temporary measure to fit with old code.
 */
template <class T> class AMQCommandControlBody : public AMQBody, public T
{
  public:
    virtual uint8_t type() const { return 100+T::SEGMENT_TYPE; }

    virtual void encode(Buffer& buffer) const {
        Codec::encode(buffer.getIterator(), static_cast<const T&>(*this));
    }
    virtual void decode(Buffer& buffer, uint32_t=0) {
        Codec::decode(buffer.getIterator(), static_cast<T&>(*this));
    }
    virtual uint32_t encodedSize() const {
        Codec::size(buffer.getIterator(), static_cast<const T&>(*this));
    }

    virtual void print(std::ostream& out) const {
        out << static_cast<const T&>(*this) << endl;
    }
    virtual void AMQBody::accept(AMQBodyConstVisitor&) const { assert(0); }
};

class CommandBody : public AMQCommandControlBody<amqp_0_10::Command> {
    using Command::accept;      // Hide AMQBody::accept
    virtual Command* getCommand() { return this; }
    virtual const Command* getCommand() const { return this; }
};

class ControlBody : public AMQCommandControlBody<amqp_0_10::Control> {
    using Control::accept;      // Hide AMQBody::accept
    virtual Control* getControl() { return this; }
    virtual const Control* getControl() const { return this; }
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_AMQCOMMANDCONTROLBODY_H*/
