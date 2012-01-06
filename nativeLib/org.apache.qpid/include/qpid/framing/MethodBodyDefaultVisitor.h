#ifndef QPID_FRAMING_METHODBODYDEFAULTVISITOR_H
#define QPID_FRAMING_METHODBODYDEFAULTVISITOR_H
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


#include "qpid/framing/MethodBodyConstVisitor.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class AMQMethodBody;
class MethodBodyDefaultVisitor:
    public MethodBodyConstVisitor
{
    public:
    virtual void defaultVisit(const AMQMethodBody&) = 0;
    QPID_COMMON_EXTERN virtual void visit(const ConnectionStartBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionStartOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionSecureBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionSecureOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionTuneBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionTuneOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionOpenBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionOpenOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionRedirectBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionHeartbeatBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionCloseBody&);
    QPID_COMMON_EXTERN virtual void visit(const ConnectionCloseOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionAttachBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionAttachedBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionDetachBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionDetachedBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionRequestTimeoutBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionTimeoutBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionCommandPointBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionExpectedBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionConfirmedBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionCompletedBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionKnownCompletedBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionFlushBody&);
    QPID_COMMON_EXTERN virtual void visit(const SessionGapBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExecutionSyncBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExecutionResultBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExecutionExceptionBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageTransferBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageAcceptBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageRejectBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageReleaseBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageAcquireBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageResumeBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageSubscribeBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageCancelBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageSetFlowModeBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageFlowBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageFlushBody&);
    QPID_COMMON_EXTERN virtual void visit(const MessageStopBody&);
    QPID_COMMON_EXTERN virtual void visit(const TxSelectBody&);
    QPID_COMMON_EXTERN virtual void visit(const TxCommitBody&);
    QPID_COMMON_EXTERN virtual void visit(const TxRollbackBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxSelectBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxStartBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxEndBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxCommitBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxForgetBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxGetTimeoutBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxPrepareBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxRecoverBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxRollbackBody&);
    QPID_COMMON_EXTERN virtual void visit(const DtxSetTimeoutBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExchangeDeclareBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExchangeDeleteBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExchangeQueryBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExchangeBindBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExchangeUnbindBody&);
    QPID_COMMON_EXTERN virtual void visit(const ExchangeBoundBody&);
    QPID_COMMON_EXTERN virtual void visit(const QueueDeclareBody&);
    QPID_COMMON_EXTERN virtual void visit(const QueueDeleteBody&);
    QPID_COMMON_EXTERN virtual void visit(const QueuePurgeBody&);
    QPID_COMMON_EXTERN virtual void visit(const QueueQueryBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileQosBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileQosOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileConsumeBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileConsumeOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileCancelBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileOpenBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileOpenOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileStageBody&);
    QPID_COMMON_EXTERN virtual void visit(const FilePublishBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileReturnBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileDeliverBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileAckBody&);
    QPID_COMMON_EXTERN virtual void visit(const FileRejectBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamQosBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamQosOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamConsumeBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamConsumeOkBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamCancelBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamPublishBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamReturnBody&);
    QPID_COMMON_EXTERN virtual void visit(const StreamDeliverBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterUpdateRequestBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterUpdateOfferBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterRetractOfferBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterInitialStatusBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterReadyBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConfigChangeBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterMessageExpiredBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterErrorCheckBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterShutdownBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionAnnounceBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionDeliverCloseBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionDeliverDoOutputBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionAbortBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionConsumerStateBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionDeliveryRecordBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionTxStartBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionTxAcceptBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionTxDequeueBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionTxEnqueueBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionTxPublishBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionTxEndBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionAccumulatedAckBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionOutputTaskBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionSessionStateBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionShadowReadyBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionMembershipBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionRetractOfferBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionQueuePositionBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionExchangeBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionQueueBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionExpiryIdBody&);
    QPID_COMMON_EXTERN virtual void visit(const ClusterConnectionAddQueueListenerBody&);
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_METHODBODYDEFAULTVISITOR_H*/
