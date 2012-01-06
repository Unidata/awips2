#ifndef QPID_FRAMING_CLIENTINVOKER_H
#define QPID_FRAMING_CLIENTINVOKER_H
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


#include "qpid/framing/AMQP_ClientOperations.h"
#include "qpid/framing/Invoker.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {


class AMQP_ClientOperations::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ClientOperations& target;
  public:
    Invoker(AMQP_ClientOperations& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const ConnectionStartBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionSecureBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionTuneBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionOpenOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionRedirectBody& body);
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
    QPID_COMMON_EXTERN void visit(const MessageResumeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageSetFlowModeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageFlowBody& body);
    QPID_COMMON_EXTERN void visit(const MessageStopBody& body);
    QPID_COMMON_EXTERN void visit(const FileQosOkBody& body);
    QPID_COMMON_EXTERN void visit(const FileConsumeOkBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenOkBody& body);
    QPID_COMMON_EXTERN void visit(const FileDeliverBody& body);
    QPID_COMMON_EXTERN void visit(const StreamQosOkBody& body);
    QPID_COMMON_EXTERN void visit(const StreamConsumeOkBody& body);
};

class AMQP_ClientOperations::ConnectionHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ClientOperations::ConnectionHandler& target;
  public:
    Invoker(AMQP_ClientOperations::ConnectionHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const ConnectionStartBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionSecureBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionTuneBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionOpenOkBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionRedirectBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionHeartbeatBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionCloseBody& body);
    QPID_COMMON_EXTERN void visit(const ConnectionCloseOkBody& body);
};

class AMQP_ClientOperations::SessionHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ClientOperations::SessionHandler& target;
  public:
    Invoker(AMQP_ClientOperations::SessionHandler& target_) : target(target_) {}
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

class AMQP_ClientOperations::ExecutionHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ClientOperations::ExecutionHandler& target;
  public:
    Invoker(AMQP_ClientOperations::ExecutionHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const ExecutionSyncBody& body);
    QPID_COMMON_EXTERN void visit(const ExecutionResultBody& body);
    QPID_COMMON_EXTERN void visit(const ExecutionExceptionBody& body);
};

class AMQP_ClientOperations::MessageHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ClientOperations::MessageHandler& target;
  public:
    Invoker(AMQP_ClientOperations::MessageHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const MessageAcceptBody& body);
    QPID_COMMON_EXTERN void visit(const MessageRejectBody& body);
    QPID_COMMON_EXTERN void visit(const MessageReleaseBody& body);
    QPID_COMMON_EXTERN void visit(const MessageResumeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageSetFlowModeBody& body);
    QPID_COMMON_EXTERN void visit(const MessageFlowBody& body);
    QPID_COMMON_EXTERN void visit(const MessageStopBody& body);
};

class AMQP_ClientOperations::FileHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ClientOperations::FileHandler& target;
  public:
    Invoker(AMQP_ClientOperations::FileHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const FileQosOkBody& body);
    QPID_COMMON_EXTERN void visit(const FileConsumeOkBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenBody& body);
    QPID_COMMON_EXTERN void visit(const FileOpenOkBody& body);
    QPID_COMMON_EXTERN void visit(const FileDeliverBody& body);
};

class AMQP_ClientOperations::StreamHandler::Invoker:
    public qpid::framing::Invoker
{
    AMQP_ClientOperations::StreamHandler& target;
  public:
    Invoker(AMQP_ClientOperations::StreamHandler& target_) : target(target_) {}
    using MethodBodyDefaultVisitor::visit;
    QPID_COMMON_EXTERN void visit(const StreamQosOkBody& body);
    QPID_COMMON_EXTERN void visit(const StreamConsumeOkBody& body);
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_CLIENTINVOKER_H*/
