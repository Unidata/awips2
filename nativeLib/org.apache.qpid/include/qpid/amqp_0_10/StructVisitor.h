#ifndef QPID_AMQP_0_10_STRUCTVISITOR_H
#define QPID_AMQP_0_10_STRUCTVISITOR_H
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

///
/// This file was automatically generated from the AMQP specification.
/// Do not edit.
///


#include "qpid/amqp_0_10/structs.h"

namespace qpid {
namespace amqp_0_10 {

struct StructVisitor
{
    virtual ~StructVisitor() {}
    typedef Struct BaseType;
    virtual void visit(message::DeliveryProperties&) = 0;
    virtual void visit(message::FragmentProperties&) = 0;
    virtual void visit(message::MessageProperties&) = 0;
    virtual void visit(message::Acquired&) = 0;
    virtual void visit(message::MessageResumeResult&) = 0;
    virtual void visit(dtx::XaResult&) = 0;
    virtual void visit(dtx::Xid&) = 0;
    virtual void visit(dtx::GetTimeoutResult&) = 0;
    virtual void visit(dtx::RecoverResult&) = 0;
    virtual void visit(exchange::ExchangeQueryResult&) = 0;
    virtual void visit(exchange::ExchangeBoundResult&) = 0;
    virtual void visit(queue::QueueQueryResult&) = 0;
    virtual void visit(file::FileProperties&) = 0;
    virtual void visit(stream::StreamProperties&) = 0;
    virtual void visit(UnknownStruct&) = 0;
};
struct ConstStructVisitor
{
    virtual ~ConstStructVisitor() {}
    typedef const Struct BaseType;
    virtual void visit(const message::DeliveryProperties&) = 0;
    virtual void visit(const message::FragmentProperties&) = 0;
    virtual void visit(const message::MessageProperties&) = 0;
    virtual void visit(const message::Acquired&) = 0;
    virtual void visit(const message::MessageResumeResult&) = 0;
    virtual void visit(const dtx::XaResult&) = 0;
    virtual void visit(const dtx::Xid&) = 0;
    virtual void visit(const dtx::GetTimeoutResult&) = 0;
    virtual void visit(const dtx::RecoverResult&) = 0;
    virtual void visit(const exchange::ExchangeQueryResult&) = 0;
    virtual void visit(const exchange::ExchangeBoundResult&) = 0;
    virtual void visit(const queue::QueueQueryResult&) = 0;
    virtual void visit(const file::FileProperties&) = 0;
    virtual void visit(const stream::StreamProperties&) = 0;
    virtual void visit(const UnknownStruct&) = 0;
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_STRUCTVISITOR_H*/
