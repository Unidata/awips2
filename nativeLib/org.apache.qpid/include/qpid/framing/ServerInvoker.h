#ifndef QPID_FRAMING_SERVERINVOKER_H
#define QPID_FRAMING_SERVERINVOKER_H
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


#include "qpid/framing/AMQP_ServerOperations.h"
#include "qpid/framing/Invoker.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {


class AMQP_ServerOperations::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations& target;
  public:
    Invoker(AMQP_ServerOperations& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const ConnectionStartOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionSecureOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionTuneOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionOpenBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionHeartbeatBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionCloseBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionCloseOkBody& body);
    QPID_COMMON_EXTERN void visit(const SessionAttachBody& body);
    QPID_COMMON_EXTERN void visit(const SessionAttachedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionDetachBody& body);
    QPID_COMMON_EXTERN void visit(const SessionDetachedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionRequestTimeoutBody& body);
    QPID_COMMON_EXTERN void visit(const SessionTimeoutBody& body);
    QPID_COMMON_EXTERN void visit(const SessionCommandPointBody& body);
    QPID_COMMON_EXTERN void visit(const SessionExpectedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionConfirmedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionCompletedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionKnownCompletedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionFlushBody& body);
    QPID_COMMON_EXTERN void visit(const SessionGapBody& body);
    QPID_COMMON_EXTERN void visit(const ExecutionSyncBody& body);
    QPID_COMMON_EXTERN void visit(const ExecutionResultBody& body);
    QPID_COMMON_EXTERN void visit(const ExecutionExceptionBody& body);
    QPID_COMMON_EXTERN void visit(const MessageAcceptBody& body);
    QPID_COMMON_EXTERN void visit(const MessageRejectBody& body);
    QPID_COMMON_EXTERN void visit(const MessageReleaseBody& body);
    QPID_COMMON_EXTERN void visit(const MessageAcquireBody& body);
    QPID_COMMON_EXTERN void visit(const MessageResumeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageSubscribeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageCancelBody& body);
    QPID_COMMON_EXTERN void visit(const MessageSetFlowModeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageFlowBody& body);
    QPID_COMMON_EXTERN void visit(const MessageFlushBody& body);
    QPID_COMMON_EXTERN void visit(const MessageStopBody& body);
    QPID_COMMON_EXTERN void visit(const TxSelectBody& body);
    QPID_COMMON_EXTERN void visit(const TxCommitBody& body);
    QPID_COMMON_EXTERN void visit(const TxRollbackBody& body);
    QPID_COMMON_EXTERN void visit(const DtxSelectBody& body);
    QPID_COMMON_EXTERN void visit(const DtxStartBody& body);
    QPID_COMMON_EXTERN void visit(const DtxEndBody& body);
    QPID_COMMON_EXTERN void visit(const DtxCommitBody& body);
    QPID_COMMON_EXTERN void visit(const DtxForgetBody& body);
    QPID_COMMON_EXTERN void visit(const DtxGetTimeoutBody& body);
    QPID_COMMON_EXTERN void visit(const DtxPrepareBody& body);
    QPID_COMMON_EXTERN void visit(const DtxRecoverBody& body);
    QPID_COMMON_EXTERN void visit(const DtxRollbackBody& body);
    QPID_COMMON_EXTERN void visit(const DtxSetTimeoutBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeDeclareBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeDeleteBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeQueryBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeBindBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeUnbindBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeBoundBody& body);
    QPID_COMMON_EXTERN void visit(const QueueDeclareBody& body);
    QPID_COMMON_EXTERN void visit(const QueueDeleteBody& body);
    QPID_COMMON_EXTERN void visit(const QueuePurgeBody& body);
    QPID_COMMON_EXTERN void visit(const QueueQueryBody& body);
    QPID_COMMON_EXTERN void visit(const FileQosBody& body);
    QPID_COMMON_EXTERN void visit(const FileConsumeBody& body);
    QPID_COMMON_EXTERN void visit(const FileCancelBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenOkBody& body);
    QPID_COMMON_EXTERN void visit(const FilePublishBody& body);
    QPID_COMMON_EXTERN void visit(const FileAckBody& body);
    QPID_COMMON_EXTERN void visit(const FileRejectBody& body);
    QPID_COMMON_EXTERN void visit(const StreamQosBody& body);
    QPID_COMMON_EXTERN void visit(const StreamConsumeBody& body);
    QPID_COMMON_EXTERN void visit(const StreamCancelBody& body);
};

class AMQP_ServerOperations::ConnectionHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::ConnectionHandler& target;
  public:
    Invoker(AMQP_ServerOperations::ConnectionHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const ConnectionStartOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionSecureOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionTuneOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionOpenBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionHeartbeatBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionCloseBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionCloseOkBody& body);
};

class AMQP_ServerOperations::SessionHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::SessionHandler& target;
  public:
    Invoker(AMQP_ServerOperations::SessionHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const SessionAttachBody& body);
    QPID_COMMON_EXTERN void visit(const SessionAttachedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionDetachBody& body);
    QPID_COMMON_EXTERN void visit(const SessionDetachedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionRequestTimeoutBody& body);
    QPID_COMMON_EXTERN void visit(const SessionTimeoutBody& body);
    QPID_COMMON_EXTERN void visit(const SessionCommandPointBody& body);
    QPID_COMMON_EXTERN void visit(const SessionExpectedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionConfirmedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionCompletedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionKnownCompletedBody& body);
    QPID_COMMON_EXTERN void visit(const SessionFlushBody& body);
    QPID_COMMON_EXTERN void visit(const SessionGapBody& body);
};

class AMQP_ServerOperations::ExecutionHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::ExecutionHandler& target;
  public:
    Invoker(AMQP_ServerOperations::ExecutionHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const ExecutionSyncBody& body);
    QPID_COMMON_EXTERN void visit(const ExecutionResultBody& body);
    QPID_COMMON_EXTERN void visit(const ExecutionExceptionBody& body);
};

class AMQP_ServerOperations::MessageHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::MessageHandler& target;
  public:
    Invoker(AMQP_ServerOperations::MessageHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const MessageAcceptBody& body);
    QPID_COMMON_EXTERN void visit(const MessageRejectBody& body);
    QPID_COMMON_EXTERN void visit(const MessageReleaseBody& body);
    QPID_COMMON_EXTERN void visit(const MessageAcquireBody& body);
    QPID_COMMON_EXTERN void visit(const MessageResumeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageSubscribeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageCancelBody& body);
    QPID_COMMON_EXTERN void visit(const MessageSetFlowModeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageFlowBody& body);
    QPID_COMMON_EXTERN void visit(const MessageFlushBody& body);
    QPID_COMMON_EXTERN void visit(const MessageStopBody& body);
};

class AMQP_ServerOperations::TxHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::TxHandler& target;
  public:
    Invoker(AMQP_ServerOperations::TxHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const TxSelectBody& body);
    QPID_COMMON_EXTERN void visit(const TxCommitBody& body);
    QPID_COMMON_EXTERN void visit(const TxRollbackBody& body);
};

class AMQP_ServerOperations::DtxHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::DtxHandler& target;
  public:
    Invoker(AMQP_ServerOperations::DtxHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const DtxSelectBody& body);
    QPID_COMMON_EXTERN void visit(const DtxStartBody& body);
    QPID_COMMON_EXTERN void visit(const DtxEndBody& body);
    QPID_COMMON_EXTERN void visit(const DtxCommitBody& body);
    QPID_COMMON_EXTERN void visit(const DtxForgetBody& body);
    QPID_COMMON_EXTERN void visit(const DtxGetTimeoutBody& body);
    QPID_COMMON_EXTERN void visit(const DtxPrepareBody& body);
    QPID_COMMON_EXTERN void visit(const DtxRecoverBody& body);
    QPID_COMMON_EXTERN void visit(const DtxRollbackBody& body);
    QPID_COMMON_EXTERN void visit(const DtxSetTimeoutBody& body);
};

class AMQP_ServerOperations::ExchangeHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::ExchangeHandler& target;
  public:
    Invoker(AMQP_ServerOperations::ExchangeHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const ExchangeDeclareBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeDeleteBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeQueryBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeBindBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeUnbindBody& body);
    QPID_COMMON_EXTERN void visit(const ExchangeBoundBody& body);
};

class AMQP_ServerOperations::QueueHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::QueueHandler& target;
  public:
    Invoker(AMQP_ServerOperations::QueueHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const QueueDeclareBody& body);
    QPID_COMMON_EXTERN void visit(const QueueDeleteBody& body);
    QPID_COMMON_EXTERN void visit(const QueuePurgeBody& body);
    QPID_COMMON_EXTERN void visit(const QueueQueryBody& body);
};

class AMQP_ServerOperations::FileHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::FileHandler& target;
  public:
    Invoker(AMQP_ServerOperations::FileHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const FileQosBody& body);
    QPID_COMMON_EXTERN void visit(const FileConsumeBody& body);
    QPID_COMMON_EXTERN void visit(const FileCancelBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenOkBody& body);
    QPID_COMMON_EXTERN void visit(const FilePublishBody& body);
    QPID_COMMON_EXTERN void visit(const FileAckBody& body);
    QPID_COMMON_EXTERN void visit(const FileRejectBody& body);
};

class AMQP_ServerOperations::StreamHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ServerOperations::StreamHandler& target;
  public:
    Invoker(AMQP_ServerOperations::StreamHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const StreamQosBody& body);
    QPID_COMMON_EXTERN void visit(const StreamConsumeBody& body);
    QPID_COMMON_EXTERN void visit(const StreamCancelBody& body);
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_SERVERINVOKER_H*/
