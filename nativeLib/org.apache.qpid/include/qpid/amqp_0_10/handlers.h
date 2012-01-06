#ifndef QPID_AMQP_0_10_HANDLERS_H
#define QPID_AMQP_0_10_HANDLERS_H
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


struct CommandHandler:
    public execution::Sync::Handler,
    public execution::Result::Handler,
    public execution::Exception::Handler,
    public message::Transfer::Handler,
    public message::Accept::Handler,
    public message::Reject::Handler,
    public message::Release::Handler,
    public message::Acquire::Handler,
    public message::Resume::Handler,
    public message::Subscribe::Handler,
    public message::Cancel::Handler,
    public message::SetFlowMode::Handler,
    public message::Flow::Handler,
    public message::Flush::Handler,
    public message::Stop::Handler,
    public tx::Select::Handler,
    public tx::Commit::Handler,
    public tx::Rollback::Handler,
    public dtx::Select::Handler,
    public dtx::Start::Handler,
    public dtx::End::Handler,
    public dtx::Commit::Handler,
    public dtx::Forget::Handler,
    public dtx::GetTimeout::Handler,
    public dtx::Prepare::Handler,
    public dtx::Recover::Handler,
    public dtx::Rollback::Handler,
    public dtx::SetTimeout::Handler,
    public exchange::Declare::Handler,
    public exchange::Delete::Handler,
    public exchange::Query::Handler,
    public exchange::Bind::Handler,
    public exchange::Unbind::Handler,
    public exchange::Bound::Handler,
    public queue::Declare::Handler,
    public queue::Delete::Handler,
    public queue::Purge::Handler,
    public queue::Query::Handler,
    public file::Qos::Handler,
    public file::QosOk::Handler,
    public file::Consume::Handler,
    public file::ConsumeOk::Handler,
    public file::Cancel::Handler,
    public file::Open::Handler,
    public file::OpenOk::Handler,
    public file::Stage::Handler,
    public file::Publish::Handler,
    public file::Return::Handler,
    public file::Deliver::Handler,
    public file::Ack::Handler,
    public file::Reject::Handler,
    public stream::Qos::Handler,
    public stream::QosOk::Handler,
    public stream::Consume::Handler,
    public stream::ConsumeOk::Handler,
    public stream::Cancel::Handler,
    public stream::Publish::Handler,
    public stream::Return::Handler,
    public stream::Deliver::Handler
{
};

struct ControlHandler:
    public connection::Start::Handler,
    public connection::StartOk::Handler,
    public connection::Secure::Handler,
    public connection::SecureOk::Handler,
    public connection::Tune::Handler,
    public connection::TuneOk::Handler,
    public connection::Open::Handler,
    public connection::OpenOk::Handler,
    public connection::Redirect::Handler,
    public connection::Heartbeat::Handler,
    public connection::Close::Handler,
    public connection::CloseOk::Handler,
    public session::Attach::Handler,
    public session::Attached::Handler,
    public session::Detach::Handler,
    public session::Detached::Handler,
    public session::RequestTimeout::Handler,
    public session::Timeout::Handler,
    public session::CommandPoint::Handler,
    public session::Expected::Handler,
    public session::Confirmed::Handler,
    public session::Completed::Handler,
    public session::KnownCompleted::Handler,
    public session::Flush::Handler,
    public session::Gap::Handler,
    public cluster::UpdateRequest::Handler,
    public cluster::UpdateOffer::Handler,
    public cluster::RetractOffer::Handler,
    public cluster::InitialStatus::Handler,
    public cluster::Ready::Handler,
    public cluster::ConfigChange::Handler,
    public cluster::MessageExpired::Handler,
    public cluster::ErrorCheck::Handler,
    public cluster::Shutdown::Handler,
    public cluster-connection::Announce::Handler,
    public cluster-connection::DeliverClose::Handler,
    public cluster-connection::DeliverDoOutput::Handler,
    public cluster-connection::Abort::Handler,
    public cluster-connection::ConsumerState::Handler,
    public cluster-connection::DeliveryRecord::Handler,
    public cluster-connection::TxStart::Handler,
    public cluster-connection::TxAccept::Handler,
    public cluster-connection::TxDequeue::Handler,
    public cluster-connection::TxEnqueue::Handler,
    public cluster-connection::TxPublish::Handler,
    public cluster-connection::TxEnd::Handler,
    public cluster-connection::AccumulatedAck::Handler,
    public cluster-connection::OutputTask::Handler,
    public cluster-connection::SessionState::Handler,
    public cluster-connection::ShadowReady::Handler,
    public cluster-connection::Membership::Handler,
    public cluster-connection::RetractOffer::Handler,
    public cluster-connection::QueuePosition::Handler,
    public cluster-connection::Exchange::Handler,
    public cluster-connection::Queue::Handler,
    public cluster-connection::ExpiryId::Handler,
    public cluster-connection::AddQueueListener::Handler
{
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_HANDLERS_H*/
