#ifndef QPID_AMQP_0_10_CONTROL_H
#define QPID_AMQP_0_10_CONTROL_H

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

#include "qpid/amqp_0_10/Struct.h"

namespace qpid {
namespace amqp_0_10 {

struct Command;
struct Control;

struct Action {  // Base for commands & controls
    virtual ~Action() {}
    virtual Command* getCommand() { return 0; }
    virtual Control* getControl() { return 0; }

    virtual const Command* getCommand() const {
        return const_cast<Action*>(this)->getCommand();
    }
    virtual const Control* getControl() const {
        return const_cast<Action*>(this)->getControl();
    }
    static const uint8_t SIZE=0;
    static const uint8_t PACK=2;
};

struct ControlVisitor;
struct ConstControlVisitor;
struct ControlHolder;
struct Control
    : public Action,
      public Visitable<ControlVisitor, ConstControlVisitor, ControlHolder>
{
    using Action::getControl;
    Control* getControl() { return this; }
    uint8_t getCode() const;
    uint8_t getClassCode() const;
    const char* getName() const;
    const char* getClassName() const;
};
std::ostream& operator<<(std::ostream&, const Control&);

template <SegmentType E> struct ActionType;
template <> struct ActionType<CONTROL> { typedef Control type; };
template <> struct ActionType<COMMAND> { typedef Command type; };
    
}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_CONTROL_H*/
