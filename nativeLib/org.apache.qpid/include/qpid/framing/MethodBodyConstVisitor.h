#ifndef QPID_FRAMING_METHODBODYCONSTVISITOR_H
#define QPID_FRAMING_METHODBODYCONSTVISITOR_H
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



namespace qpid {
namespace framing {

class ConnectionStartBody;
class ConnectionStartOkBody;
class ConnectionSecureBody;
class ConnectionSecureOkBody;
class ConnectionTuneBody;
class ConnectionTuneOkBody;
class ConnectionOpenBody;
class ConnectionOpenOkBody;
class ConnectionRedirectBody;
class ConnectionHeartbeatBody;
class ConnectionCloseBody;
class ConnectionCloseOkBody;
class SessionAttachBody;
class SessionAttachedBody;
class SessionDetachBody;
class SessionDetachedBody;
class SessionRequestTimeoutBody;
class SessionTimeoutBody;
class SessionCommandPointBody;
class SessionExpectedBody;
class SessionConfirmedBody;
class SessionCompletedBody;
class SessionKnownCompletedBody;
class SessionFlushBody;
class SessionGapBody;
class ExecutionSyncBody;
class ExecutionResultBody;
class ExecutionExceptionBody;
class MessageTransferBody;
class MessageAcceptBody;
class MessageRejectBody;
class MessageReleaseBody;
class MessageAcquireBody;
class MessageResumeBody;
class MessageSubscribeBody;
class MessageCancelBody;
class MessageSetFlowModeBody;
class MessageFlowBody;
class MessageFlushBody;
class MessageStopBody;
class TxSelectBody;
class TxCommitBody;
class TxRollbackBody;
class DtxSelectBody;
class DtxStartBody;
class DtxEndBody;
class DtxCommitBody;
class DtxForgetBody;
class DtxGetTimeoutBody;
class DtxPrepareBody;
class DtxRecoverBody;
class DtxRollbackBody;
class DtxSetTimeoutBody;
class ExchangeDeclareBody;
class ExchangeDeleteBody;
class ExchangeQueryBody;
class ExchangeBindBody;
class ExchangeUnbindBody;
class ExchangeBoundBody;
class QueueDeclareBody;
class QueueDeleteBody;
class QueuePurgeBody;
class QueueQueryBody;
class FileQosBody;
class FileQosOkBody;
class FileConsumeBody;
class FileConsumeOkBody;
class FileCancelBody;
class FileOpenBody;
class FileOpenOkBody;
class FileStageBody;
class FilePublishBody;
class FileReturnBody;
class FileDeliverBody;
class FileAckBody;
class FileRejectBody;
class StreamQosBody;
class StreamQosOkBody;
class StreamConsumeBody;
class StreamConsumeOkBody;
class StreamCancelBody;
class StreamPublishBody;
class StreamReturnBody;
class StreamDeliverBody;
class ClusterUpdateRequestBody;
class ClusterUpdateOfferBody;
class ClusterRetractOfferBody;
class ClusterInitialStatusBody;
class ClusterReadyBody;
class ClusterConfigChangeBody;
class ClusterMessageExpiredBody;
class ClusterErrorCheckBody;
class ClusterTimerWakeupBody;
class ClusterTimerDropBody;
class ClusterShutdownBody;
class ClusterDeliverToQueueBody;
class ClusterConnectionAnnounceBody;
class ClusterConnectionDeliverCloseBody;
class ClusterConnectionDeliverDoOutputBody;
class ClusterConnectionAbortBody;
class ClusterConnectionShadowSetUserBody;
class ClusterConnectionShadowPrepareBody;
class ClusterConnectionConsumerStateBody;
class ClusterConnectionDeliveryRecordBody;
class ClusterConnectionTxStartBody;
class ClusterConnectionTxAcceptBody;
class ClusterConnectionTxDequeueBody;
class ClusterConnectionTxEnqueueBody;
class ClusterConnectionTxPublishBody;
class ClusterConnectionTxEndBody;
class ClusterConnectionAccumulatedAckBody;
class ClusterConnectionOutputTaskBody;
class ClusterConnectionSessionStateBody;
class ClusterConnectionShadowReadyBody;
class ClusterConnectionMembershipBody;
class ClusterConnectionRetractOfferBody;
class ClusterConnectionQueuePositionBody;
class ClusterConnectionExchangeBody;
class ClusterConnectionQueueBody;
class ClusterConnectionExpiryIdBody;
class ClusterConnectionAddQueueListenerBody;
class ClusterConnectionManagementSetupStateBody;
class ClusterConnectionConfigBody;
class MethodBodyConstVisitor
{
    public:
    virtual ~MethodBodyConstVisitor() {}
    virtual void visit(const ConnectionStartBody&) = 0;
    virtual void visit(const ConnectionStartOkBody&) = 0;
    virtual void visit(const ConnectionSecureBody&) = 0;
    virtual void visit(const ConnectionSecureOkBody&) = 0;
    virtual void visit(const ConnectionTuneBody&) = 0;
    virtual void visit(const ConnectionTuneOkBody&) = 0;
    virtual void visit(const ConnectionOpenBody&) = 0;
    virtual void visit(const ConnectionOpenOkBody&) = 0;
    virtual void visit(const ConnectionRedirectBody&) = 0;
    virtual void visit(const ConnectionHeartbeatBody&) = 0;
    virtual void visit(const ConnectionCloseBody&) = 0;
    virtual void visit(const ConnectionCloseOkBody&) = 0;
    virtual void visit(const SessionAttachBody&) = 0;
    virtual void visit(const SessionAttachedBody&) = 0;
    virtual void visit(const SessionDetachBody&) = 0;
    virtual void visit(const SessionDetachedBody&) = 0;
    virtual void visit(const SessionRequestTimeoutBody&) = 0;
    virtual void visit(const SessionTimeoutBody&) = 0;
    virtual void visit(const SessionCommandPointBody&) = 0;
    virtual void visit(const SessionExpectedBody&) = 0;
    virtual void visit(const SessionConfirmedBody&) = 0;
    virtual void visit(const SessionCompletedBody&) = 0;
    virtual void visit(const SessionKnownCompletedBody&) = 0;
    virtual void visit(const SessionFlushBody&) = 0;
    virtual void visit(const SessionGapBody&) = 0;
    virtual void visit(const ExecutionSyncBody&) = 0;
    virtual void visit(const ExecutionResultBody&) = 0;
    virtual void visit(const ExecutionExceptionBody&) = 0;
    virtual void visit(const MessageTransferBody&) = 0;
    virtual void visit(const MessageAcceptBody&) = 0;
    virtual void visit(const MessageRejectBody&) = 0;
    virtual void visit(const MessageReleaseBody&) = 0;
    virtual void visit(const MessageAcquireBody&) = 0;
    virtual void visit(const MessageResumeBody&) = 0;
    virtual void visit(const MessageSubscribeBody&) = 0;
    virtual void visit(const MessageCancelBody&) = 0;
    virtual void visit(const MessageSetFlowModeBody&) = 0;
    virtual void visit(const MessageFlowBody&) = 0;
    virtual void visit(const MessageFlushBody&) = 0;
    virtual void visit(const MessageStopBody&) = 0;
    virtual void visit(const TxSelectBody&) = 0;
    virtual void visit(const TxCommitBody&) = 0;
    virtual void visit(const TxRollbackBody&) = 0;
    virtual void visit(const DtxSelectBody&) = 0;
    virtual void visit(const DtxStartBody&) = 0;
    virtual void visit(const DtxEndBody&) = 0;
    virtual void visit(const DtxCommitBody&) = 0;
    virtual void visit(const DtxForgetBody&) = 0;
    virtual void visit(const DtxGetTimeoutBody&) = 0;
    virtual void visit(const DtxPrepareBody&) = 0;
    virtual void visit(const DtxRecoverBody&) = 0;
    virtual void visit(const DtxRollbackBody&) = 0;
    virtual void visit(const DtxSetTimeoutBody&) = 0;
    virtual void visit(const ExchangeDeclareBody&) = 0;
    virtual void visit(const ExchangeDeleteBody&) = 0;
    virtual void visit(const ExchangeQueryBody&) = 0;
    virtual void visit(const ExchangeBindBody&) = 0;
    virtual void visit(const ExchangeUnbindBody&) = 0;
    virtual void visit(const ExchangeBoundBody&) = 0;
    virtual void visit(const QueueDeclareBody&) = 0;
    virtual void visit(const QueueDeleteBody&) = 0;
    virtual void visit(const QueuePurgeBody&) = 0;
    virtual void visit(const QueueQueryBody&) = 0;
    virtual void visit(const FileQosBody&) = 0;
    virtual void visit(const FileQosOkBody&) = 0;
    virtual void visit(const FileConsumeBody&) = 0;
    virtual void visit(const FileConsumeOkBody&) = 0;
    virtual void visit(const FileCancelBody&) = 0;
    virtual void visit(const FileOpenBody&) = 0;
    virtual void visit(const FileOpenOkBody&) = 0;
    virtual void visit(const FileStageBody&) = 0;
    virtual void visit(const FilePublishBody&) = 0;
    virtual void visit(const FileReturnBody&) = 0;
    virtual void visit(const FileDeliverBody&) = 0;
    virtual void visit(const FileAckBody&) = 0;
    virtual void visit(const FileRejectBody&) = 0;
    virtual void visit(const StreamQosBody&) = 0;
    virtual void visit(const StreamQosOkBody&) = 0;
    virtual void visit(const StreamConsumeBody&) = 0;
    virtual void visit(const StreamConsumeOkBody&) = 0;
    virtual void visit(const StreamCancelBody&) = 0;
    virtual void visit(const StreamPublishBody&) = 0;
    virtual void visit(const StreamReturnBody&) = 0;
    virtual void visit(const StreamDeliverBody&) = 0;
    virtual void visit(const ClusterUpdateRequestBody&) = 0;
    virtual void visit(const ClusterUpdateOfferBody&) = 0;
    virtual void visit(const ClusterRetractOfferBody&) = 0;
    virtual void visit(const ClusterInitialStatusBody&) = 0;
    virtual void visit(const ClusterReadyBody&) = 0;
    virtual void visit(const ClusterConfigChangeBody&) = 0;
    virtual void visit(const ClusterMessageExpiredBody&) = 0;
    virtual void visit(const ClusterErrorCheckBody&) = 0;
    virtual void visit(const ClusterTimerWakeupBody&) = 0;
    virtual void visit(const ClusterTimerDropBody&) = 0;
    virtual void visit(const ClusterShutdownBody&) = 0;
    virtual void visit(const ClusterDeliverToQueueBody&) = 0;
    virtual void visit(const ClusterConnectionAnnounceBody&) = 0;
    virtual void visit(const ClusterConnectionDeliverCloseBody&) = 0;
    virtual void visit(const ClusterConnectionDeliverDoOutputBody&) = 0;
    virtual void visit(const ClusterConnectionAbortBody&) = 0;
    virtual void visit(const ClusterConnectionShadowSetUserBody&) = 0;
    virtual void visit(const ClusterConnectionShadowPrepareBody&) = 0;
    virtual void visit(const ClusterConnectionConsumerStateBody&) = 0;
    virtual void visit(const ClusterConnectionDeliveryRecordBody&) = 0;
    virtual void visit(const ClusterConnectionTxStartBody&) = 0;
    virtual void visit(const ClusterConnectionTxAcceptBody&) = 0;
    virtual void visit(const ClusterConnectionTxDequeueBody&) = 0;
    virtual void visit(const ClusterConnectionTxEnqueueBody&) = 0;
    virtual void visit(const ClusterConnectionTxPublishBody&) = 0;
    virtual void visit(const ClusterConnectionTxEndBody&) = 0;
    virtual void visit(const ClusterConnectionAccumulatedAckBody&) = 0;
    virtual void visit(const ClusterConnectionOutputTaskBody&) = 0;
    virtual void visit(const ClusterConnectionSessionStateBody&) = 0;
    virtual void visit(const ClusterConnectionShadowReadyBody&) = 0;
    virtual void visit(const ClusterConnectionMembershipBody&) = 0;
    virtual void visit(const ClusterConnectionRetractOfferBody&) = 0;
    virtual void visit(const ClusterConnectionQueuePositionBody&) = 0;
    virtual void visit(const ClusterConnectionExchangeBody&) = 0;
    virtual void visit(const ClusterConnectionQueueBody&) = 0;
    virtual void visit(const ClusterConnectionExpiryIdBody&) = 0;
    virtual void visit(const ClusterConnectionAddQueueListenerBody&) = 0;
    virtual void visit(const ClusterConnectionManagementSetupStateBody&) = 0;
    virtual void visit(const ClusterConnectionConfigBody&) = 0;
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_METHODBODYCONSTVISITOR_H*/
