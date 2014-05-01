#ifndef QPID_AMQP_0_10_COMMAND_H
#define QPID_AMQP_0_10_COMMAND_H

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

#include "qpid/amqp_0_10/Control.h"
#include "qpid/amqp_0_10/structs.h"

namespace qpid {
namespace amqp_0_10 {

struct CommandVisitor;
struct ConstCommandVisitor;
struct CommandHolder;
struct Command
    : public Action,
      public Visitable<CommandVisitor, ConstCommandVisitor, CommandHolder>
{
    using Action::getCommand;
    Command* getCommand() { return this; }
    uint8_t getCode() const;
    uint8_t getClassCode() const;
    const char* getName() const;
    const char* getClassName() const;

    session::Header sessionHeader;
};

std::ostream& operator<<(std::ostream&, const Command&);

template <class T>
struct CommandPacker : Packer<T> {
    CommandPacker(T& t) : Packer<T>(t) {}
    
    template <class S> void serialize(S& s) { 
        s(this->data.sessionHeader);
        Packer<T>::serialize(s);
    }
};
    
}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_COMMAND_H*/
