#ifndef QPID_AMQP_0_10_APPLYSTRUCT_H
#define QPID_AMQP_0_10_APPLYSTRUCT_H
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


#include "qpid/amqp_0_10/StructVisitor.h"
#include "qpid/amqp_0_10/apply.h"

namespace qpid {
namespace amqp_0_10 {

template <class F>
struct ApplyVisitor<StructVisitor, F>:
    public ApplyVisitorBase<StructVisitor, F>
{
    virtual void visit(message::DeliveryProperties& x) { this->invoke(x); }
    virtual void visit(message::FragmentProperties& x) { this->invoke(x); }
    virtual void visit(message::MessageProperties& x) { this->invoke(x); }
    virtual void visit(message::Acquired& x) { this->invoke(x); }
    virtual void visit(message::MessageResumeResult& x) { this->invoke(x); }
    virtual void visit(dtx::XaResult& x) { this->invoke(x); }
    virtual void visit(dtx::Xid& x) { this->invoke(x); }
    virtual void visit(dtx::GetTimeoutResult& x) { this->invoke(x); }
    virtual void visit(dtx::RecoverResult& x) { this->invoke(x); }
    virtual void visit(exchange::ExchangeQueryResult& x) { this->invoke(x); }
    virtual void visit(exchange::ExchangeBoundResult& x) { this->invoke(x); }
    virtual void visit(queue::QueueQueryResult& x) { this->invoke(x); }
    virtual void visit(file::FileProperties& x) { this->invoke(x); }
    virtual void visit(stream::StreamProperties& x) { this->invoke(x); }
    virtual void visit(UnknownStruct& x) { this->invoke(x); }
};
template <class F>
struct ApplyVisitor<ConstStructVisitor, F>:
    public ApplyVisitorBase<ConstStructVisitor, F>
{
    virtual void visit(const message::DeliveryProperties& x) { this->invoke(x); }
    virtual void visit(const message::FragmentProperties& x) { this->invoke(x); }
    virtual void visit(const message::MessageProperties& x) { this->invoke(x); }
    virtual void visit(const message::Acquired& x) { this->invoke(x); }
    virtual void visit(const message::MessageResumeResult& x) { this->invoke(x); }
    virtual void visit(const dtx::XaResult& x) { this->invoke(x); }
    virtual void visit(const dtx::Xid& x) { this->invoke(x); }
    virtual void visit(const dtx::GetTimeoutResult& x) { this->invoke(x); }
    virtual void visit(const dtx::RecoverResult& x) { this->invoke(x); }
    virtual void visit(const exchange::ExchangeQueryResult& x) { this->invoke(x); }
    virtual void visit(const exchange::ExchangeBoundResult& x) { this->invoke(x); }
    virtual void visit(const queue::QueueQueryResult& x) { this->invoke(x); }
    virtual void visit(const file::FileProperties& x) { this->invoke(x); }
    virtual void visit(const stream::StreamProperties& x) { this->invoke(x); }
    virtual void visit(const UnknownStruct& x) { this->invoke(x); }
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_APPLYSTRUCT_H*/
