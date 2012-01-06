#ifndef QPID_AMQP_0_10_COMMANDVISITOR_H
#define QPID_AMQP_0_10_COMMANDVISITOR_H
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


#include "qpid/amqp_0_10/specification.h"

namespace qpid {
namespace amqp_0_10 {

struct CommandVisitor
{
    virtual ~CommandVisitor() {}
    typedef Command BaseType;
    virtual void visit(execution::Sync&) = 0;
    virtual void visit(execution::Result&) = 0;
    virtual void visit(execution::Exception&) = 0;
    virtual void visit(message::Transfer&) = 0;
    virtual void visit(message::Accept&) = 0;
    virtual void visit(message::Reject&) = 0;
    virtual void visit(message::Release&) = 0;
    virtual void visit(message::Acquire&) = 0;
    virtual void visit(message::Resume&) = 0;
    virtual void visit(message::Subscribe&) = 0;
    virtual void visit(message::Cancel&) = 0;
    virtual void visit(message::SetFlowMode&) = 0;
    virtual void visit(message::Flow&) = 0;
    virtual void visit(message::Flush&) = 0;
    virtual void visit(message::Stop&) = 0;
    virtual void visit(tx::Select&) = 0;
    virtual void visit(tx::Commit&) = 0;
    virtual void visit(tx::Rollback&) = 0;
    virtual void visit(dtx::Select&) = 0;
    virtual void visit(dtx::Start&) = 0;
    virtual void visit(dtx::End&) = 0;
    virtual void visit(dtx::Commit&) = 0;
    virtual void visit(dtx::Forget&) = 0;
    virtual void visit(dtx::GetTimeout&) = 0;
    virtual void visit(dtx::Prepare&) = 0;
    virtual void visit(dtx::Recover&) = 0;
    virtual void visit(dtx::Rollback&) = 0;
    virtual void visit(dtx::SetTimeout&) = 0;
    virtual void visit(exchange::Declare&) = 0;
    virtual void visit(exchange::Delete&) = 0;
    virtual void visit(exchange::Query&) = 0;
    virtual void visit(exchange::Bind&) = 0;
    virtual void visit(exchange::Unbind&) = 0;
    virtual void visit(exchange::Bound&) = 0;
    virtual void visit(queue::Declare&) = 0;
    virtual void visit(queue::Delete&) = 0;
    virtual void visit(queue::Purge&) = 0;
    virtual void visit(queue::Query&) = 0;
    virtual void visit(file::Qos&) = 0;
    virtual void visit(file::QosOk&) = 0;
    virtual void visit(file::Consume&) = 0;
    virtual void visit(file::ConsumeOk&) = 0;
    virtual void visit(file::Cancel&) = 0;
    virtual void visit(file::Open&) = 0;
    virtual void visit(file::OpenOk&) = 0;
    virtual void visit(file::Stage&) = 0;
    virtual void visit(file::Publish&) = 0;
    virtual void visit(file::Return&) = 0;
    virtual void visit(file::Deliver&) = 0;
    virtual void visit(file::Ack&) = 0;
    virtual void visit(file::Reject&) = 0;
    virtual void visit(stream::Qos&) = 0;
    virtual void visit(stream::QosOk&) = 0;
    virtual void visit(stream::Consume&) = 0;
    virtual void visit(stream::ConsumeOk&) = 0;
    virtual void visit(stream::Cancel&) = 0;
    virtual void visit(stream::Publish&) = 0;
    virtual void visit(stream::Return&) = 0;
    virtual void visit(stream::Deliver&) = 0;
};
struct ConstCommandVisitor
{
    virtual ~ConstCommandVisitor() {}
    typedef const Command BaseType;
    virtual void visit(const execution::Sync&) = 0;
    virtual void visit(const execution::Result&) = 0;
    virtual void visit(const execution::Exception&) = 0;
    virtual void visit(const message::Transfer&) = 0;
    virtual void visit(const message::Accept&) = 0;
    virtual void visit(const message::Reject&) = 0;
    virtual void visit(const message::Release&) = 0;
    virtual void visit(const message::Acquire&) = 0;
    virtual void visit(const message::Resume&) = 0;
    virtual void visit(const message::Subscribe&) = 0;
    virtual void visit(const message::Cancel&) = 0;
    virtual void visit(const message::SetFlowMode&) = 0;
    virtual void visit(const message::Flow&) = 0;
    virtual void visit(const message::Flush&) = 0;
    virtual void visit(const message::Stop&) = 0;
    virtual void visit(const tx::Select&) = 0;
    virtual void visit(const tx::Commit&) = 0;
    virtual void visit(const tx::Rollback&) = 0;
    virtual void visit(const dtx::Select&) = 0;
    virtual void visit(const dtx::Start&) = 0;
    virtual void visit(const dtx::End&) = 0;
    virtual void visit(const dtx::Commit&) = 0;
    virtual void visit(const dtx::Forget&) = 0;
    virtual void visit(const dtx::GetTimeout&) = 0;
    virtual void visit(const dtx::Prepare&) = 0;
    virtual void visit(const dtx::Recover&) = 0;
    virtual void visit(const dtx::Rollback&) = 0;
    virtual void visit(const dtx::SetTimeout&) = 0;
    virtual void visit(const exchange::Declare&) = 0;
    virtual void visit(const exchange::Delete&) = 0;
    virtual void visit(const exchange::Query&) = 0;
    virtual void visit(const exchange::Bind&) = 0;
    virtual void visit(const exchange::Unbind&) = 0;
    virtual void visit(const exchange::Bound&) = 0;
    virtual void visit(const queue::Declare&) = 0;
    virtual void visit(const queue::Delete&) = 0;
    virtual void visit(const queue::Purge&) = 0;
    virtual void visit(const queue::Query&) = 0;
    virtual void visit(const file::Qos&) = 0;
    virtual void visit(const file::QosOk&) = 0;
    virtual void visit(const file::Consume&) = 0;
    virtual void visit(const file::ConsumeOk&) = 0;
    virtual void visit(const file::Cancel&) = 0;
    virtual void visit(const file::Open&) = 0;
    virtual void visit(const file::OpenOk&) = 0;
    virtual void visit(const file::Stage&) = 0;
    virtual void visit(const file::Publish&) = 0;
    virtual void visit(const file::Return&) = 0;
    virtual void visit(const file::Deliver&) = 0;
    virtual void visit(const file::Ack&) = 0;
    virtual void visit(const file::Reject&) = 0;
    virtual void visit(const stream::Qos&) = 0;
    virtual void visit(const stream::QosOk&) = 0;
    virtual void visit(const stream::Consume&) = 0;
    virtual void visit(const stream::ConsumeOk&) = 0;
    virtual void visit(const stream::Cancel&) = 0;
    virtual void visit(const stream::Publish&) = 0;
    virtual void visit(const stream::Return&) = 0;
    virtual void visit(const stream::Deliver&) = 0;
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_COMMANDVISITOR_H*/
