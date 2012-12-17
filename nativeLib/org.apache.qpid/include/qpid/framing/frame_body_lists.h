#ifndef QPID_FRAMING_FRAME_BODY_LISTS_H
#define QPID_FRAMING_FRAME_BODY_LISTS_H
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


/**@file
 * Macro lists of frame body classes, used to generate Visitors
 */
#define METHOD_BODIES() \
    (ConnectionStartBody) \
    (ConnectionStartOkBody) \
    (ConnectionSecureBody) \
    (ConnectionSecureOkBody) \
    (ConnectionTuneBody) \
    (ConnectionTuneOkBody) \
    (ConnectionOpenBody) \
    (ConnectionOpenOkBody) \
    (ConnectionRedirectBody) \
    (ConnectionHeartbeatBody) \
    (ConnectionCloseBody) \
    (ConnectionCloseOkBody) \
    (SessionAttachBody) \
    (SessionAttachedBody) \
    (SessionDetachBody) \
    (SessionDetachedBody) \
    (SessionRequestTimeoutBody) \
    (SessionTimeoutBody) \
    (SessionCommandPointBody) \
    (SessionExpectedBody) \
    (SessionConfirmedBody) \
    (SessionCompletedBody) \
    (SessionKnownCompletedBody) \
    (SessionFlushBody) \
    (SessionGapBody) \
    (ExecutionSyncBody) \
    (ExecutionResultBody) \
    (ExecutionExceptionBody) \
    (MessageTransferBody) \
    (MessageAcceptBody) \
    (MessageRejectBody) \
    (MessageReleaseBody) \
    (MessageAcquireBody) \
    (MessageResumeBody) \
    (MessageSubscribeBody) \
    (MessageCancelBody) \
    (MessageSetFlowModeBody) \
    (MessageFlowBody) \
    (MessageFlushBody) \
    (MessageStopBody) \
    (TxSelectBody) \
    (TxCommitBody) \
    (TxRollbackBody) \
    (DtxSelectBody) \
    (DtxStartBody) \
    (DtxEndBody) \
    (DtxCommitBody) \
    (DtxForgetBody) \
    (DtxGetTimeoutBody) \
    (DtxPrepareBody) \
    (DtxRecoverBody) \
    (DtxRollbackBody) \
    (DtxSetTimeoutBody) \
    (ExchangeDeclareBody) \
    (ExchangeDeleteBody) \
    (ExchangeQueryBody) \
    (ExchangeBindBody) \
    (ExchangeUnbindBody) \
    (ExchangeBoundBody) \
    (QueueDeclareBody) \
    (QueueDeleteBody) \
    (QueuePurgeBody) \
    (QueueQueryBody) \
    (FileQosBody) \
    (FileQosOkBody) \
    (FileConsumeBody) \
    (FileConsumeOkBody) \
    (FileCancelBody) \
    (FileOpenBody) \
    (FileOpenOkBody) \
    (FileStageBody) \
    (FilePublishBody) \
    (FileReturnBody) \
    (FileDeliverBody) \
    (FileAckBody) \
    (FileRejectBody) \
    (StreamQosBody) \
    (StreamQosOkBody) \
    (StreamConsumeBody) \
    (StreamConsumeOkBody) \
    (StreamCancelBody) \
    (StreamPublishBody) \
    (StreamReturnBody) \
    (StreamDeliverBody) \
    (ClusterUpdateRequestBody) \
    (ClusterUpdateOfferBody) \
    (ClusterRetractOfferBody) \
    (ClusterInitialStatusBody) \
    (ClusterReadyBody) \
    (ClusterConfigChangeBody) \
    (ClusterMessageExpiredBody) \
    (ClusterErrorCheckBody) \
    (ClusterShutdownBody) \
    (ClusterConnectionAnnounceBody) \
    (ClusterConnectionDeliverCloseBody) \
    (ClusterConnectionDeliverDoOutputBody) \
    (ClusterConnectionAbortBody) \
    (ClusterConnectionConsumerStateBody) \
    (ClusterConnectionDeliveryRecordBody) \
    (ClusterConnectionTxStartBody) \
    (ClusterConnectionTxAcceptBody) \
    (ClusterConnectionTxDequeueBody) \
    (ClusterConnectionTxEnqueueBody) \
    (ClusterConnectionTxPublishBody) \
    (ClusterConnectionTxEndBody) \
    (ClusterConnectionAccumulatedAckBody) \
    (ClusterConnectionOutputTaskBody) \
    (ClusterConnectionSessionStateBody) \
    (ClusterConnectionShadowReadyBody) \
    (ClusterConnectionMembershipBody) \
    (ClusterConnectionRetractOfferBody) \
    (ClusterConnectionQueuePositionBody) \
    (ClusterConnectionExchangeBody) \
    (ClusterConnectionQueueBody) \
    (ClusterConnectionExpiryIdBody) \
    (ClusterConnectionAddQueueListenerBody) 

#define OTHER_BODIES() (AMQContentBody)(AMQHeaderBody)(AMQHeartbeatBody))

#endif  /*!QPID_FRAMING_FRAME_BODY_LISTS_H*/
