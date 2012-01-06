#ifndef QPID_AMQP_0_10_COMMANDHOLDER_H
#define QPID_AMQP_0_10_COMMANDHOLDER_H
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


#include "qpid/amqp_0_10/ApplyCommand.h"
#include "qpid/amqp_0_10/Holder.h"
#include "qpid/amqp_0_10/specification.h"

namespace qpid {
namespace amqp_0_10 {


namespace command_max {

static const size_t MAX000=0;
static const size_t MAX001 = sizeof(execution::Sync) > MAX000 ? sizeof(execution::Sync) : MAX000;
static const size_t MAX002 = sizeof(execution::Result) > MAX001 ? sizeof(execution::Result) : MAX001;
static const size_t MAX003 = sizeof(execution::Exception) > MAX002 ? sizeof(execution::Exception) : MAX002;
static const size_t MAX004 = sizeof(message::Transfer) > MAX003 ? sizeof(message::Transfer) : MAX003;
static const size_t MAX005 = sizeof(message::Accept) > MAX004 ? sizeof(message::Accept) : MAX004;
static const size_t MAX006 = sizeof(message::Reject) > MAX005 ? sizeof(message::Reject) : MAX005;
static const size_t MAX007 = sizeof(message::Release) > MAX006 ? sizeof(message::Release) : MAX006;
static const size_t MAX008 = sizeof(message::Acquire) > MAX007 ? sizeof(message::Acquire) : MAX007;
static const size_t MAX009 = sizeof(message::Resume) > MAX008 ? sizeof(message::Resume) : MAX008;
static const size_t MAX010 = sizeof(message::Subscribe) > MAX009 ? sizeof(message::Subscribe) : MAX009;
static const size_t MAX011 = sizeof(message::Cancel) > MAX010 ? sizeof(message::Cancel) : MAX010;
static const size_t MAX012 = sizeof(message::SetFlowMode) > MAX011 ? sizeof(message::SetFlowMode) : MAX011;
static const size_t MAX013 = sizeof(message::Flow) > MAX012 ? sizeof(message::Flow) : MAX012;
static const size_t MAX014 = sizeof(message::Flush) > MAX013 ? sizeof(message::Flush) : MAX013;
static const size_t MAX015 = sizeof(message::Stop) > MAX014 ? sizeof(message::Stop) : MAX014;
static const size_t MAX016 = sizeof(tx::Select) > MAX015 ? sizeof(tx::Select) : MAX015;
static const size_t MAX017 = sizeof(tx::Commit) > MAX016 ? sizeof(tx::Commit) : MAX016;
static const size_t MAX018 = sizeof(tx::Rollback) > MAX017 ? sizeof(tx::Rollback) : MAX017;
static const size_t MAX019 = sizeof(dtx::Select) > MAX018 ? sizeof(dtx::Select) : MAX018;
static const size_t MAX020 = sizeof(dtx::Start) > MAX019 ? sizeof(dtx::Start) : MAX019;
static const size_t MAX021 = sizeof(dtx::End) > MAX020 ? sizeof(dtx::End) : MAX020;
static const size_t MAX022 = sizeof(dtx::Commit) > MAX021 ? sizeof(dtx::Commit) : MAX021;
static const size_t MAX023 = sizeof(dtx::Forget) > MAX022 ? sizeof(dtx::Forget) : MAX022;
static const size_t MAX024 = sizeof(dtx::GetTimeout) > MAX023 ? sizeof(dtx::GetTimeout) : MAX023;
static const size_t MAX025 = sizeof(dtx::Prepare) > MAX024 ? sizeof(dtx::Prepare) : MAX024;
static const size_t MAX026 = sizeof(dtx::Recover) > MAX025 ? sizeof(dtx::Recover) : MAX025;
static const size_t MAX027 = sizeof(dtx::Rollback) > MAX026 ? sizeof(dtx::Rollback) : MAX026;
static const size_t MAX028 = sizeof(dtx::SetTimeout) > MAX027 ? sizeof(dtx::SetTimeout) : MAX027;
static const size_t MAX029 = sizeof(exchange::Declare) > MAX028 ? sizeof(exchange::Declare) : MAX028;
static const size_t MAX030 = sizeof(exchange::Delete) > MAX029 ? sizeof(exchange::Delete) : MAX029;
static const size_t MAX031 = sizeof(exchange::Query) > MAX030 ? sizeof(exchange::Query) : MAX030;
static const size_t MAX032 = sizeof(exchange::Bind) > MAX031 ? sizeof(exchange::Bind) : MAX031;
static const size_t MAX033 = sizeof(exchange::Unbind) > MAX032 ? sizeof(exchange::Unbind) : MAX032;
static const size_t MAX034 = sizeof(exchange::Bound) > MAX033 ? sizeof(exchange::Bound) : MAX033;
static const size_t MAX035 = sizeof(queue::Declare) > MAX034 ? sizeof(queue::Declare) : MAX034;
static const size_t MAX036 = sizeof(queue::Delete) > MAX035 ? sizeof(queue::Delete) : MAX035;
static const size_t MAX037 = sizeof(queue::Purge) > MAX036 ? sizeof(queue::Purge) : MAX036;
static const size_t MAX038 = sizeof(queue::Query) > MAX037 ? sizeof(queue::Query) : MAX037;
static const size_t MAX039 = sizeof(file::Qos) > MAX038 ? sizeof(file::Qos) : MAX038;
static const size_t MAX040 = sizeof(file::QosOk) > MAX039 ? sizeof(file::QosOk) : MAX039;
static const size_t MAX041 = sizeof(file::Consume) > MAX040 ? sizeof(file::Consume) : MAX040;
static const size_t MAX042 = sizeof(file::ConsumeOk) > MAX041 ? sizeof(file::ConsumeOk) : MAX041;
static const size_t MAX043 = sizeof(file::Cancel) > MAX042 ? sizeof(file::Cancel) : MAX042;
static const size_t MAX044 = sizeof(file::Open) > MAX043 ? sizeof(file::Open) : MAX043;
static const size_t MAX045 = sizeof(file::OpenOk) > MAX044 ? sizeof(file::OpenOk) : MAX044;
static const size_t MAX046 = sizeof(file::Stage) > MAX045 ? sizeof(file::Stage) : MAX045;
static const size_t MAX047 = sizeof(file::Publish) > MAX046 ? sizeof(file::Publish) : MAX046;
static const size_t MAX048 = sizeof(file::Return) > MAX047 ? sizeof(file::Return) : MAX047;
static const size_t MAX049 = sizeof(file::Deliver) > MAX048 ? sizeof(file::Deliver) : MAX048;
static const size_t MAX050 = sizeof(file::Ack) > MAX049 ? sizeof(file::Ack) : MAX049;
static const size_t MAX051 = sizeof(file::Reject) > MAX050 ? sizeof(file::Reject) : MAX050;
static const size_t MAX052 = sizeof(stream::Qos) > MAX051 ? sizeof(stream::Qos) : MAX051;
static const size_t MAX053 = sizeof(stream::QosOk) > MAX052 ? sizeof(stream::QosOk) : MAX052;
static const size_t MAX054 = sizeof(stream::Consume) > MAX053 ? sizeof(stream::Consume) : MAX053;
static const size_t MAX055 = sizeof(stream::ConsumeOk) > MAX054 ? sizeof(stream::ConsumeOk) : MAX054;
static const size_t MAX056 = sizeof(stream::Cancel) > MAX055 ? sizeof(stream::Cancel) : MAX055;
static const size_t MAX057 = sizeof(stream::Publish) > MAX056 ? sizeof(stream::Publish) : MAX056;
static const size_t MAX058 = sizeof(stream::Return) > MAX057 ? sizeof(stream::Return) : MAX057;
static const size_t MAX059 = sizeof(stream::Deliver) > MAX058 ? sizeof(stream::Deliver) : MAX058;
static const int MAX=MAX059;

} // namespace command_max

struct CommandHolder:
    public amqp_0_10::Holder<CommandHolder, Command, command_max::MAX>
{
    CommandHolder() {}
    template <class T> explicit CommandHolder(const T& t) : amqp_0_10::Holder<CommandHolder, Command, command_max::MAX>(t) {}
    using amqp_0_10::Holder<CommandHolder, Command, command_max::MAX>::operator=;
    void set(uint8_t classCode, uint8_t code);
};

std::ostream& operator<<(std::ostream& o, const CommandHolder& h);

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_COMMANDHOLDER_H*/
