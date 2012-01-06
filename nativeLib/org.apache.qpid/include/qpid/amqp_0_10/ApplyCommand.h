#ifndef QPID_AMQP_0_10_APPLYCOMMAND_H
#define QPID_AMQP_0_10_APPLYCOMMAND_H
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


#include "qpid/amqp_0_10/CommandVisitor.h"
#include "qpid/amqp_0_10/apply.h"

namespace qpid {
namespace amqp_0_10 {

template <class F>
struct ApplyVisitor<CommandVisitor, F>:
    public ApplyVisitorBase<CommandVisitor, F>
{
    virtual void visit(execution::Sync& x) { this->invoke(x); }
    virtual void visit(execution::Result& x) { this->invoke(x); }
    virtual void visit(execution::Exception& x) { this->invoke(x); }
    virtual void visit(message::Transfer& x) { this->invoke(x); }
    virtual void visit(message::Accept& x) { this->invoke(x); }
    virtual void visit(message::Reject& x) { this->invoke(x); }
    virtual void visit(message::Release& x) { this->invoke(x); }
    virtual void visit(message::Acquire& x) { this->invoke(x); }
    virtual void visit(message::Resume& x) { this->invoke(x); }
    virtual void visit(message::Subscribe& x) { this->invoke(x); }
    virtual void visit(message::Cancel& x) { this->invoke(x); }
    virtual void visit(message::SetFlowMode& x) { this->invoke(x); }
    virtual void visit(message::Flow& x) { this->invoke(x); }
    virtual void visit(message::Flush& x) { this->invoke(x); }
    virtual void visit(message::Stop& x) { this->invoke(x); }
    virtual void visit(tx::Select& x) { this->invoke(x); }
    virtual void visit(tx::Commit& x) { this->invoke(x); }
    virtual void visit(tx::Rollback& x) { this->invoke(x); }
    virtual void visit(dtx::Select& x) { this->invoke(x); }
    virtual void visit(dtx::Start& x) { this->invoke(x); }
    virtual void visit(dtx::End& x) { this->invoke(x); }
    virtual void visit(dtx::Commit& x) { this->invoke(x); }
    virtual void visit(dtx::Forget& x) { this->invoke(x); }
    virtual void visit(dtx::GetTimeout& x) { this->invoke(x); }
    virtual void visit(dtx::Prepare& x) { this->invoke(x); }
    virtual void visit(dtx::Recover& x) { this->invoke(x); }
    virtual void visit(dtx::Rollback& x) { this->invoke(x); }
    virtual void visit(dtx::SetTimeout& x) { this->invoke(x); }
    virtual void visit(exchange::Declare& x) { this->invoke(x); }
    virtual void visit(exchange::Delete& x) { this->invoke(x); }
    virtual void visit(exchange::Query& x) { this->invoke(x); }
    virtual void visit(exchange::Bind& x) { this->invoke(x); }
    virtual void visit(exchange::Unbind& x) { this->invoke(x); }
    virtual void visit(exchange::Bound& x) { this->invoke(x); }
    virtual void visit(queue::Declare& x) { this->invoke(x); }
    virtual void visit(queue::Delete& x) { this->invoke(x); }
    virtual void visit(queue::Purge& x) { this->invoke(x); }
    virtual void visit(queue::Query& x) { this->invoke(x); }
    virtual void visit(file::Qos& x) { this->invoke(x); }
    virtual void visit(file::QosOk& x) { this->invoke(x); }
    virtual void visit(file::Consume& x) { this->invoke(x); }
    virtual void visit(file::ConsumeOk& x) { this->invoke(x); }
    virtual void visit(file::Cancel& x) { this->invoke(x); }
    virtual void visit(file::Open& x) { this->invoke(x); }
    virtual void visit(file::OpenOk& x) { this->invoke(x); }
    virtual void visit(file::Stage& x) { this->invoke(x); }
    virtual void visit(file::Publish& x) { this->invoke(x); }
    virtual void visit(file::Return& x) { this->invoke(x); }
    virtual void visit(file::Deliver& x) { this->invoke(x); }
    virtual void visit(file::Ack& x) { this->invoke(x); }
    virtual void visit(file::Reject& x) { this->invoke(x); }
    virtual void visit(stream::Qos& x) { this->invoke(x); }
    virtual void visit(stream::QosOk& x) { this->invoke(x); }
    virtual void visit(stream::Consume& x) { this->invoke(x); }
    virtual void visit(stream::ConsumeOk& x) { this->invoke(x); }
    virtual void visit(stream::Cancel& x) { this->invoke(x); }
    virtual void visit(stream::Publish& x) { this->invoke(x); }
    virtual void visit(stream::Return& x) { this->invoke(x); }
    virtual void visit(stream::Deliver& x) { this->invoke(x); }
};
template <class F>
struct ApplyVisitor<ConstCommandVisitor, F>:
    public ApplyVisitorBase<ConstCommandVisitor, F>
{
    virtual void visit(const execution::Sync& x) { this->invoke(x); }
    virtual void visit(const execution::Result& x) { this->invoke(x); }
    virtual void visit(const execution::Exception& x) { this->invoke(x); }
    virtual void visit(const message::Transfer& x) { this->invoke(x); }
    virtual void visit(const message::Accept& x) { this->invoke(x); }
    virtual void visit(const message::Reject& x) { this->invoke(x); }
    virtual void visit(const message::Release& x) { this->invoke(x); }
    virtual void visit(const message::Acquire& x) { this->invoke(x); }
    virtual void visit(const message::Resume& x) { this->invoke(x); }
    virtual void visit(const message::Subscribe& x) { this->invoke(x); }
    virtual void visit(const message::Cancel& x) { this->invoke(x); }
    virtual void visit(const message::SetFlowMode& x) { this->invoke(x); }
    virtual void visit(const message::Flow& x) { this->invoke(x); }
    virtual void visit(const message::Flush& x) { this->invoke(x); }
    virtual void visit(const message::Stop& x) { this->invoke(x); }
    virtual void visit(const tx::Select& x) { this->invoke(x); }
    virtual void visit(const tx::Commit& x) { this->invoke(x); }
    virtual void visit(const tx::Rollback& x) { this->invoke(x); }
    virtual void visit(const dtx::Select& x) { this->invoke(x); }
    virtual void visit(const dtx::Start& x) { this->invoke(x); }
    virtual void visit(const dtx::End& x) { this->invoke(x); }
    virtual void visit(const dtx::Commit& x) { this->invoke(x); }
    virtual void visit(const dtx::Forget& x) { this->invoke(x); }
    virtual void visit(const dtx::GetTimeout& x) { this->invoke(x); }
    virtual void visit(const dtx::Prepare& x) { this->invoke(x); }
    virtual void visit(const dtx::Recover& x) { this->invoke(x); }
    virtual void visit(const dtx::Rollback& x) { this->invoke(x); }
    virtual void visit(const dtx::SetTimeout& x) { this->invoke(x); }
    virtual void visit(const exchange::Declare& x) { this->invoke(x); }
    virtual void visit(const exchange::Delete& x) { this->invoke(x); }
    virtual void visit(const exchange::Query& x) { this->invoke(x); }
    virtual void visit(const exchange::Bind& x) { this->invoke(x); }
    virtual void visit(const exchange::Unbind& x) { this->invoke(x); }
    virtual void visit(const exchange::Bound& x) { this->invoke(x); }
    virtual void visit(const queue::Declare& x) { this->invoke(x); }
    virtual void visit(const queue::Delete& x) { this->invoke(x); }
    virtual void visit(const queue::Purge& x) { this->invoke(x); }
    virtual void visit(const queue::Query& x) { this->invoke(x); }
    virtual void visit(const file::Qos& x) { this->invoke(x); }
    virtual void visit(const file::QosOk& x) { this->invoke(x); }
    virtual void visit(const file::Consume& x) { this->invoke(x); }
    virtual void visit(const file::ConsumeOk& x) { this->invoke(x); }
    virtual void visit(const file::Cancel& x) { this->invoke(x); }
    virtual void visit(const file::Open& x) { this->invoke(x); }
    virtual void visit(const file::OpenOk& x) { this->invoke(x); }
    virtual void visit(const file::Stage& x) { this->invoke(x); }
    virtual void visit(const file::Publish& x) { this->invoke(x); }
    virtual void visit(const file::Return& x) { this->invoke(x); }
    virtual void visit(const file::Deliver& x) { this->invoke(x); }
    virtual void visit(const file::Ack& x) { this->invoke(x); }
    virtual void visit(const file::Reject& x) { this->invoke(x); }
    virtual void visit(const stream::Qos& x) { this->invoke(x); }
    virtual void visit(const stream::QosOk& x) { this->invoke(x); }
    virtual void visit(const stream::Consume& x) { this->invoke(x); }
    virtual void visit(const stream::ConsumeOk& x) { this->invoke(x); }
    virtual void visit(const stream::Cancel& x) { this->invoke(x); }
    virtual void visit(const stream::Publish& x) { this->invoke(x); }
    virtual void visit(const stream::Return& x) { this->invoke(x); }
    virtual void visit(const stream::Deliver& x) { this->invoke(x); }
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_APPLYCOMMAND_H*/
