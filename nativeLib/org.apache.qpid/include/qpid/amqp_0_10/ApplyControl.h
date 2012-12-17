#ifndef QPID_AMQP_0_10_APPLYCONTROL_H
#define QPID_AMQP_0_10_APPLYCONTROL_H
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


#include "qpid/amqp_0_10/ControlVisitor.h"
#include "qpid/amqp_0_10/apply.h"

namespace qpid {
namespace amqp_0_10 {

template <class F>
struct ApplyVisitor<ControlVisitor, F>:
    public ApplyVisitorBase<ControlVisitor, F>
{
    virtual void visit(connection::Start& x) { this->invoke(x); }
    virtual void visit(connection::StartOk& x) { this->invoke(x); }
    virtual void visit(connection::Secure& x) { this->invoke(x); }
    virtual void visit(connection::SecureOk& x) { this->invoke(x); }
    virtual void visit(connection::Tune& x) { this->invoke(x); }
    virtual void visit(connection::TuneOk& x) { this->invoke(x); }
    virtual void visit(connection::Open& x) { this->invoke(x); }
    virtual void visit(connection::OpenOk& x) { this->invoke(x); }
    virtual void visit(connection::Redirect& x) { this->invoke(x); }
    virtual void visit(connection::Heartbeat& x) { this->invoke(x); }
    virtual void visit(connection::Close& x) { this->invoke(x); }
    virtual void visit(connection::CloseOk& x) { this->invoke(x); }
    virtual void visit(session::Attach& x) { this->invoke(x); }
    virtual void visit(session::Attached& x) { this->invoke(x); }
    virtual void visit(session::Detach& x) { this->invoke(x); }
    virtual void visit(session::Detached& x) { this->invoke(x); }
    virtual void visit(session::RequestTimeout& x) { this->invoke(x); }
    virtual void visit(session::Timeout& x) { this->invoke(x); }
    virtual void visit(session::CommandPoint& x) { this->invoke(x); }
    virtual void visit(session::Expected& x) { this->invoke(x); }
    virtual void visit(session::Confirmed& x) { this->invoke(x); }
    virtual void visit(session::Completed& x) { this->invoke(x); }
    virtual void visit(session::KnownCompleted& x) { this->invoke(x); }
    virtual void visit(session::Flush& x) { this->invoke(x); }
    virtual void visit(session::Gap& x) { this->invoke(x); }
    virtual void visit(cluster::UpdateRequest& x) { this->invoke(x); }
    virtual void visit(cluster::UpdateOffer& x) { this->invoke(x); }
    virtual void visit(cluster::RetractOffer& x) { this->invoke(x); }
    virtual void visit(cluster::InitialStatus& x) { this->invoke(x); }
    virtual void visit(cluster::Ready& x) { this->invoke(x); }
    virtual void visit(cluster::ConfigChange& x) { this->invoke(x); }
    virtual void visit(cluster::MessageExpired& x) { this->invoke(x); }
    virtual void visit(cluster::ErrorCheck& x) { this->invoke(x); }
    virtual void visit(cluster::Shutdown& x) { this->invoke(x); }
    virtual void visit(cluster-connection::Announce& x) { this->invoke(x); }
    virtual void visit(cluster-connection::DeliverClose& x) { this->invoke(x); }
    virtual void visit(cluster-connection::DeliverDoOutput& x) { this->invoke(x); }
    virtual void visit(cluster-connection::Abort& x) { this->invoke(x); }
    virtual void visit(cluster-connection::ConsumerState& x) { this->invoke(x); }
    virtual void visit(cluster-connection::DeliveryRecord& x) { this->invoke(x); }
    virtual void visit(cluster-connection::TxStart& x) { this->invoke(x); }
    virtual void visit(cluster-connection::TxAccept& x) { this->invoke(x); }
    virtual void visit(cluster-connection::TxDequeue& x) { this->invoke(x); }
    virtual void visit(cluster-connection::TxEnqueue& x) { this->invoke(x); }
    virtual void visit(cluster-connection::TxPublish& x) { this->invoke(x); }
    virtual void visit(cluster-connection::TxEnd& x) { this->invoke(x); }
    virtual void visit(cluster-connection::AccumulatedAck& x) { this->invoke(x); }
    virtual void visit(cluster-connection::OutputTask& x) { this->invoke(x); }
    virtual void visit(cluster-connection::SessionState& x) { this->invoke(x); }
    virtual void visit(cluster-connection::ShadowReady& x) { this->invoke(x); }
    virtual void visit(cluster-connection::Membership& x) { this->invoke(x); }
    virtual void visit(cluster-connection::RetractOffer& x) { this->invoke(x); }
    virtual void visit(cluster-connection::QueuePosition& x) { this->invoke(x); }
    virtual void visit(cluster-connection::Exchange& x) { this->invoke(x); }
    virtual void visit(cluster-connection::Queue& x) { this->invoke(x); }
    virtual void visit(cluster-connection::ExpiryId& x) { this->invoke(x); }
    virtual void visit(cluster-connection::AddQueueListener& x) { this->invoke(x); }
};
template <class F>
struct ApplyVisitor<ConstControlVisitor, F>:
    public ApplyVisitorBase<ConstControlVisitor, F>
{
    virtual void visit(const connection::Start& x) { this->invoke(x); }
    virtual void visit(const connection::StartOk& x) { this->invoke(x); }
    virtual void visit(const connection::Secure& x) { this->invoke(x); }
    virtual void visit(const connection::SecureOk& x) { this->invoke(x); }
    virtual void visit(const connection::Tune& x) { this->invoke(x); }
    virtual void visit(const connection::TuneOk& x) { this->invoke(x); }
    virtual void visit(const connection::Open& x) { this->invoke(x); }
    virtual void visit(const connection::OpenOk& x) { this->invoke(x); }
    virtual void visit(const connection::Redirect& x) { this->invoke(x); }
    virtual void visit(const connection::Heartbeat& x) { this->invoke(x); }
    virtual void visit(const connection::Close& x) { this->invoke(x); }
    virtual void visit(const connection::CloseOk& x) { this->invoke(x); }
    virtual void visit(const session::Attach& x) { this->invoke(x); }
    virtual void visit(const session::Attached& x) { this->invoke(x); }
    virtual void visit(const session::Detach& x) { this->invoke(x); }
    virtual void visit(const session::Detached& x) { this->invoke(x); }
    virtual void visit(const session::RequestTimeout& x) { this->invoke(x); }
    virtual void visit(const session::Timeout& x) { this->invoke(x); }
    virtual void visit(const session::CommandPoint& x) { this->invoke(x); }
    virtual void visit(const session::Expected& x) { this->invoke(x); }
    virtual void visit(const session::Confirmed& x) { this->invoke(x); }
    virtual void visit(const session::Completed& x) { this->invoke(x); }
    virtual void visit(const session::KnownCompleted& x) { this->invoke(x); }
    virtual void visit(const session::Flush& x) { this->invoke(x); }
    virtual void visit(const session::Gap& x) { this->invoke(x); }
    virtual void visit(const cluster::UpdateRequest& x) { this->invoke(x); }
    virtual void visit(const cluster::UpdateOffer& x) { this->invoke(x); }
    virtual void visit(const cluster::RetractOffer& x) { this->invoke(x); }
    virtual void visit(const cluster::InitialStatus& x) { this->invoke(x); }
    virtual void visit(const cluster::Ready& x) { this->invoke(x); }
    virtual void visit(const cluster::ConfigChange& x) { this->invoke(x); }
    virtual void visit(const cluster::MessageExpired& x) { this->invoke(x); }
    virtual void visit(const cluster::ErrorCheck& x) { this->invoke(x); }
    virtual void visit(const cluster::Shutdown& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::Announce& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::DeliverClose& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::DeliverDoOutput& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::Abort& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::ConsumerState& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::DeliveryRecord& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::TxStart& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::TxAccept& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::TxDequeue& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::TxEnqueue& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::TxPublish& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::TxEnd& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::AccumulatedAck& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::OutputTask& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::SessionState& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::ShadowReady& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::Membership& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::RetractOffer& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::QueuePosition& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::Exchange& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::Queue& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::ExpiryId& x) { this->invoke(x); }
    virtual void visit(const cluster-connection::AddQueueListener& x) { this->invoke(x); }
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_APPLYCONTROL_H*/
