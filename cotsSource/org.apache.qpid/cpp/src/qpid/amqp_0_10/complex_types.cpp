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

#include "qpid/amqp_0_10/UnknownStruct.h"
#include "qpid/amqp_0_10/ApplyCommand.h"
#include "qpid/amqp_0_10/ApplyControl.h"
#include "qpid/amqp_0_10/ApplyStruct.h"
#include "qpid/amqp_0_10/apply.h"
#include <iostream>

namespace qpid {
namespace amqp_0_10 {
// Functors for getting static values from a visitable base type.

#define QPID_STATIC_VALUE_GETTER(NAME, TYPE, VALUE) \
    struct NAME : public ApplyFunctor<TYPE> {  \
        template <class T> TYPE operator()(const T&) const { return T::VALUE; }\
    }

QPID_STATIC_VALUE_GETTER(GetCode, uint8_t, CODE);
QPID_STATIC_VALUE_GETTER(GetSize, uint8_t, SIZE);
QPID_STATIC_VALUE_GETTER(GetPack, uint8_t, PACK);
QPID_STATIC_VALUE_GETTER(GetClassCode, uint8_t, CLASS_CODE);
QPID_STATIC_VALUE_GETTER(GetName, const char*, NAME);
QPID_STATIC_VALUE_GETTER(GetClassName, const char*, CLASS_NAME);


uint8_t Command::getCode() const { return apply(GetCode(), *this); }
uint8_t Command::getClassCode() const { return apply(GetClassCode(), *this); }
const char* Command::getName() const { return apply(GetName(), *this); }
const char* Command::getClassName() const { return apply(GetClassName(), *this); }

uint8_t Control::getCode() const { return apply(GetCode(), *this); }
uint8_t Control::getClassCode() const { return apply(GetClassCode(), *this); }
const char* Control::getName() const { return apply(GetName(), *this); }
const char* Control::getClassName() const { return apply(GetClassName(), *this); }

// Special cases for UnknownStruct
struct GetStructCode : public GetCode {
    using GetCode::operator();
    uint8_t operator()(const UnknownStruct& u) const { return u.getCode(); }
};

struct GetStructClassCode : public GetClassCode {
    using GetClassCode::operator();
    uint8_t operator()(const UnknownStruct& u) const { return u.getClassCode(); }
};

uint8_t Struct::getCode() const { return apply(GetStructCode(), *this); }
uint8_t Struct::getClassCode() const { return apply(GetStructClassCode(), *this); }
uint8_t Struct::getPack() const { return apply(GetPack(), *this); }
uint8_t Struct::getSize() const { return apply(GetSize(), *this); }

struct PrintVisitor {
    typedef std::ostream& result_type;
    std::ostream& out;
    PrintVisitor(std::ostream& o) : out(o) {}
    template <class T> result_type operator()(const T& t) const { return out << t; }
};

std::ostream& operator<<(std::ostream& o, const Command& x) { return apply(PrintVisitor(o), x); }
std::ostream& operator<<(std::ostream& o, const Control& x) { return apply(PrintVisitor(o), x); }
std::ostream& operator<<(std::ostream& o, const Struct& x)  { return apply(PrintVisitor(o), x); }
    
}} // namespace qpid::amqp_0_10

