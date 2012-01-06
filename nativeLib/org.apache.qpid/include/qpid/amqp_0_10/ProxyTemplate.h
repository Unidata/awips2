#ifndef QPID_AMQP_0_10_PROXYTEMPLATE_H
#define QPID_AMQP_0_10_PROXYTEMPLATE_H
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

template <class F, class R=typename F::result_type>
class ProxyTemplate
{
  public:
    ProxyTemplate(F f=F()) : functor(f) {}
    
    
    R connectionStart(
        const Map& serverProperties_,
        const Str16Array& mechanisms_,
        const Str16Array& locales_
    )
    {
        connection::Start start(serverProperties_, mechanisms_, locales_);
        return functor(start);
    }
    
    
    R connectionStartOk(
        const Map& clientProperties_,
        const Str8& mechanism_,
        const Vbin32& response_,
        const Str8& locale_
    )
    {
        connection::StartOk startOk(clientProperties_, mechanism_, response_, locale_);
        return functor(startOk);
    }
    
    
    R connectionSecure(const Vbin32& challenge_)
    {
        connection::Secure secure(challenge_);
        return functor(secure);
    }
    
    
    R connectionSecureOk(const Vbin32& response_)
    {
        connection::SecureOk secureOk(response_);
        return functor(secureOk);
    }
    
    
    R connectionTune(
        Uint16 channelMax_,
        Uint16 maxFrameSize_,
        Uint16 heartbeatMin_,
        Uint16 heartbeatMax_
    )
    {
        connection::Tune tune(channelMax_, maxFrameSize_, heartbeatMin_, heartbeatMax_);
        return functor(tune);
    }
    
    
    R connectionTuneOk(
        Uint16 channelMax_,
        Uint16 maxFrameSize_,
        Uint16 heartbeat_
    )
    {
        connection::TuneOk tuneOk(channelMax_, maxFrameSize_, heartbeat_);
        return functor(tuneOk);
    }
    
    
    R connectionOpen(
        const Str8& virtualHost_,
        const Str16Array& capabilities_,
        Bit insist_
    )
    {
        connection::Open open(virtualHost_, capabilities_, insist_);
        return functor(open);
    }
    
    
    R connectionOpenOk(const connection::AmqpHostArray& knownHosts_)
    {
        connection::OpenOk openOk(knownHosts_);
        return functor(openOk);
    }
    
    
    R connectionRedirect(
        const connection::AmqpHostUrl& host_,
        const connection::AmqpHostArray& knownHosts_
    )
    {
        connection::Redirect redirect(host_, knownHosts_);
        return functor(redirect);
    }
    
    
    R connectionHeartbeat()
    {
        connection::Heartbeat heartbeat;
        return functor(heartbeat);
    }
    
    
    R connectionClose(
        const connection::CloseCode& replyCode_,
        const Str8& replyText_
    )
    {
        connection::Close close(replyCode_, replyText_);
        return functor(close);
    }
    
    
    R connectionCloseOk()
    {
        connection::CloseOk closeOk;
        return functor(closeOk);
    }
    
    
    R sessionAttach(
        const session::Name& name_,
        Bit force_
    )
    {
        session::Attach attach(name_, force_);
        return functor(attach);
    }
    
    
    R sessionAttached(const session::Name& name_)
    {
        session::Attached attached(name_);
        return functor(attached);
    }
    
    
    R sessionDetach(const session::Name& name_)
    {
        session::Detach detach(name_);
        return functor(detach);
    }
    
    
    R sessionDetached(
        const session::Name& name_,
        const session::DetachCode& code_
    )
    {
        session::Detached detached(name_, code_);
        return functor(detached);
    }
    
    
    R sessionRequestTimeout(Uint32 timeout_)
    {
        session::RequestTimeout requestTimeout(timeout_);
        return functor(requestTimeout);
    }
    
    
    R sessionTimeout(Uint32 timeout_)
    {
        session::Timeout timeout(timeout_);
        return functor(timeout);
    }
    
    
    R sessionCommandPoint(
        const SequenceNo& commandId_,
        Uint64 commandOffset_
    )
    {
        session::CommandPoint commandPoint(commandId_, commandOffset_);
        return functor(commandPoint);
    }
    
    
    R sessionExpected(
        const session::Commands& commands_,
        const session::CommandFragments& fragments_
    )
    {
        session::Expected expected(commands_, fragments_);
        return functor(expected);
    }
    
    
    R sessionConfirmed(
        const session::Commands& commands_,
        const session::CommandFragments& fragments_
    )
    {
        session::Confirmed confirmed(commands_, fragments_);
        return functor(confirmed);
    }
    
    
    R sessionCompleted(
        const session::Commands& commands_,
        Bit timelyReply_
    )
    {
        session::Completed completed(commands_, timelyReply_);
        return functor(completed);
    }
    
    
    R sessionKnownCompleted(const session::Commands& commands_)
    {
        session::KnownCompleted knownCompleted(commands_);
        return functor(knownCompleted);
    }
    
    
    R sessionFlush(
        Bit expected_,
        Bit confirmed_,
        Bit completed_
    )
    {
        session::Flush flush(expected_, confirmed_, completed_);
        return functor(flush);
    }
    
    
    R sessionGap(const session::Commands& commands_)
    {
        session::Gap gap(commands_);
        return functor(gap);
    }
    
    
    R executionSync()
    {
        execution::Sync sync;
        return functor(sync);
    }
    
    
    R executionResult(
        const SequenceNo& commandId_,
        const Struct32& value_
    )
    {
        execution::Result result(commandId_, value_);
        return functor(result);
    }
    
    
    R executionException(
        const execution::ErrorCode& errorCode_,
        const SequenceNo& commandId_,
        Uint8 classCode_,
        Uint8 commandCode_,
        Uint8 fieldIndex_,
        const Str16& description_,
        const Map& errorInfo_
    )
    {
        execution::Exception exception(errorCode_, commandId_, classCode_, commandCode_, fieldIndex_, description_, errorInfo_);
        return functor(exception);
    }
    
    
    R messageTransfer(
        const message::Destination& destination_,
        const message::AcceptMode& acceptMode_,
        const message::AcquireMode& acquireMode_
    )
    {
        message::Transfer transfer(destination_, acceptMode_, acquireMode_);
        return functor(transfer);
    }
    
    
    R messageAccept(const session::Commands& transfers_)
    {
        message::Accept accept(transfers_);
        return functor(accept);
    }
    
    
    R messageReject(
        const session::Commands& transfers_,
        const message::RejectCode& code_,
        const Str8& text_
    )
    {
        message::Reject reject(transfers_, code_, text_);
        return functor(reject);
    }
    
    
    R messageRelease(
        const session::Commands& transfers_,
        Bit setRedelivered_
    )
    {
        message::Release release(transfers_, setRedelivered_);
        return functor(release);
    }
    
    
    R messageAcquire(const session::Commands& transfers_)
    {
        message::Acquire acquire(transfers_);
        return functor(acquire);
    }
    
    
    R messageResume(
        const message::Destination& destination_,
        const message::ResumeId& resumeId_
    )
    {
        message::Resume resume(destination_, resumeId_);
        return functor(resume);
    }
    
    
    R messageSubscribe(
        const queue::Name& queue_,
        const message::Destination& destination_,
        const message::AcceptMode& acceptMode_,
        const message::AcquireMode& acquireMode_,
        Bit exclusive_,
        const message::ResumeId& resumeId_,
        Uint64 resumeTtl_,
        const Map& arguments_
    )
    {
        message::Subscribe subscribe(queue_, destination_, acceptMode_, acquireMode_, exclusive_, resumeId_, resumeTtl_, arguments_);
        return functor(subscribe);
    }
    
    
    R messageCancel(const message::Destination& destination_)
    {
        message::Cancel cancel(destination_);
        return functor(cancel);
    }
    
    
    R messageSetFlowMode(
        const message::Destination& destination_,
        const message::FlowMode& flowMode_
    )
    {
        message::SetFlowMode setFlowMode(destination_, flowMode_);
        return functor(setFlowMode);
    }
    
    
    R messageFlow(
        const message::Destination& destination_,
        const message::CreditUnit& unit_,
        Uint32 value_
    )
    {
        message::Flow flow(destination_, unit_, value_);
        return functor(flow);
    }
    
    
    R messageFlush(const message::Destination& destination_)
    {
        message::Flush flush(destination_);
        return functor(flush);
    }
    
    
    R messageStop(const message::Destination& destination_)
    {
        message::Stop stop(destination_);
        return functor(stop);
    }
    
    
    R txSelect()
    {
        tx::Select select;
        return functor(select);
    }
    
    
    R txCommit()
    {
        tx::Commit commit;
        return functor(commit);
    }
    
    
    R txRollback()
    {
        tx::Rollback rollback;
        return functor(rollback);
    }
    
    
    R dtxSelect()
    {
        dtx::Select select;
        return functor(select);
    }
    
    
    R dtxStart(
        const dtx::Xid& xid_,
        Bit join_,
        Bit resume_
    )
    {
        dtx::Start start(xid_, join_, resume_);
        return functor(start);
    }
    
    
    R dtxEnd(
        const dtx::Xid& xid_,
        Bit fail_,
        Bit suspend_
    )
    {
        dtx::End end(xid_, fail_, suspend_);
        return functor(end);
    }
    
    
    R dtxCommit(
        const dtx::Xid& xid_,
        Bit onePhase_
    )
    {
        dtx::Commit commit(xid_, onePhase_);
        return functor(commit);
    }
    
    
    R dtxForget(const dtx::Xid& xid_)
    {
        dtx::Forget forget(xid_);
        return functor(forget);
    }
    
    
    R dtxGetTimeout(const dtx::Xid& xid_)
    {
        dtx::GetTimeout getTimeout(xid_);
        return functor(getTimeout);
    }
    
    
    R dtxPrepare(const dtx::Xid& xid_)
    {
        dtx::Prepare prepare(xid_);
        return functor(prepare);
    }
    
    
    R dtxRecover()
    {
        dtx::Recover recover;
        return functor(recover);
    }
    
    
    R dtxRollback(const dtx::Xid& xid_)
    {
        dtx::Rollback rollback(xid_);
        return functor(rollback);
    }
    
    
    R dtxSetTimeout(
        const dtx::Xid& xid_,
        Uint32 timeout_
    )
    {
        dtx::SetTimeout setTimeout(xid_, timeout_);
        return functor(setTimeout);
    }
    
    
    R exchangeDeclare(
        const exchange::Name& exchange_,
        const Str8& type_,
        const exchange::Name& alternateExchange_,
        Bit passive_,
        Bit durable_,
        Bit autoDelete_,
        const Map& arguments_
    )
    {
        exchange::Declare declare(exchange_, type_, alternateExchange_, passive_, durable_, autoDelete_, arguments_);
        return functor(declare);
    }
    
    
    R exchangeDelete(
        const exchange::Name& exchange_,
        Bit ifUnused_
    )
    {
        exchange::Delete delete_(exchange_, ifUnused_);
        return functor(delete_);
    }
    
    
    R exchangeQuery(const Str8& name_)
    {
        exchange::Query query(name_);
        return functor(query);
    }
    
    
    R exchangeBind(
        const queue::Name& queue_,
        const exchange::Name& exchange_,
        const Str8& bindingKey_,
        const Map& arguments_
    )
    {
        exchange::Bind bind(queue_, exchange_, bindingKey_, arguments_);
        return functor(bind);
    }
    
    
    R exchangeUnbind(
        const queue::Name& queue_,
        const exchange::Name& exchange_,
        const Str8& bindingKey_
    )
    {
        exchange::Unbind unbind(queue_, exchange_, bindingKey_);
        return functor(unbind);
    }
    
    
    R exchangeBound(
        const Str8& exchange_,
        const Str8& queue_,
        const Str8& bindingKey_,
        const Map& arguments_
    )
    {
        exchange::Bound bound(exchange_, queue_, bindingKey_, arguments_);
        return functor(bound);
    }
    
    
    R queueDeclare(
        const queue::Name& queue_,
        const exchange::Name& alternateExchange_,
        Bit passive_,
        Bit durable_,
        Bit exclusive_,
        Bit autoDelete_,
        const Map& arguments_
    )
    {
        queue::Declare declare(queue_, alternateExchange_, passive_, durable_, exclusive_, autoDelete_, arguments_);
        return functor(declare);
    }
    
    
    R queueDelete(
        const queue::Name& queue_,
        Bit ifUnused_,
        Bit ifEmpty_
    )
    {
        queue::Delete delete_(queue_, ifUnused_, ifEmpty_);
        return functor(delete_);
    }
    
    
    R queuePurge(const queue::Name& queue_)
    {
        queue::Purge purge(queue_);
        return functor(purge);
    }
    
    
    R queueQuery(const queue::Name& queue_)
    {
        queue::Query query(queue_);
        return functor(query);
    }
    
    
    R fileQos(
        Uint32 prefetchSize_,
        Uint16 prefetchCount_,
        Bit global_
    )
    {
        file::Qos qos(prefetchSize_, prefetchCount_, global_);
        return functor(qos);
    }
    
    
    R fileQosOk()
    {
        file::QosOk qosOk;
        return functor(qosOk);
    }
    
    
    R fileConsume(
        const queue::Name& queue_,
        const Str8& consumerTag_,
        Bit noLocal_,
        Bit noAck_,
        Bit exclusive_,
        Bit nowait_,
        const Map& arguments_
    )
    {
        file::Consume consume(queue_, consumerTag_, noLocal_, noAck_, exclusive_, nowait_, arguments_);
        return functor(consume);
    }
    
    
    R fileConsumeOk(const Str8& consumerTag_)
    {
        file::ConsumeOk consumeOk(consumerTag_);
        return functor(consumeOk);
    }
    
    
    R fileCancel(const Str8& consumerTag_)
    {
        file::Cancel cancel(consumerTag_);
        return functor(cancel);
    }
    
    
    R fileOpen(
        const Str8& identifier_,
        Uint64 contentSize_
    )
    {
        file::Open open(identifier_, contentSize_);
        return functor(open);
    }
    
    
    R fileOpenOk(Uint64 stagedSize_)
    {
        file::OpenOk openOk(stagedSize_);
        return functor(openOk);
    }
    
    
    R fileStage()
    {
        file::Stage stage;
        return functor(stage);
    }
    
    
    R filePublish(
        const exchange::Name& exchange_,
        const Str8& routingKey_,
        Bit mandatory_,
        Bit immediate_,
        const Str8& identifier_
    )
    {
        file::Publish publish(exchange_, routingKey_, mandatory_, immediate_, identifier_);
        return functor(publish);
    }
    
    
    R fileReturn(
        const file::ReturnCode& replyCode_,
        const Str8& replyText_,
        const exchange::Name& exchange_,
        const Str8& routingKey_
    )
    {
        file::Return return_(replyCode_, replyText_, exchange_, routingKey_);
        return functor(return_);
    }
    
    
    R fileDeliver(
        const Str8& consumerTag_,
        Uint64 deliveryTag_,
        Bit redelivered_,
        const exchange::Name& exchange_,
        const Str8& routingKey_,
        const Str8& identifier_
    )
    {
        file::Deliver deliver(consumerTag_, deliveryTag_, redelivered_, exchange_, routingKey_, identifier_);
        return functor(deliver);
    }
    
    
    R fileAck(
        Uint64 deliveryTag_,
        Bit multiple_
    )
    {
        file::Ack ack(deliveryTag_, multiple_);
        return functor(ack);
    }
    
    
    R fileReject(
        Uint64 deliveryTag_,
        Bit requeue_
    )
    {
        file::Reject reject(deliveryTag_, requeue_);
        return functor(reject);
    }
    
    
    R streamQos(
        Uint32 prefetchSize_,
        Uint16 prefetchCount_,
        Uint32 consumeRate_,
        Bit global_
    )
    {
        stream::Qos qos(prefetchSize_, prefetchCount_, consumeRate_, global_);
        return functor(qos);
    }
    
    
    R streamQosOk()
    {
        stream::QosOk qosOk;
        return functor(qosOk);
    }
    
    
    R streamConsume(
        const queue::Name& queue_,
        const Str8& consumerTag_,
        Bit noLocal_,
        Bit exclusive_,
        Bit nowait_,
        const Map& arguments_
    )
    {
        stream::Consume consume(queue_, consumerTag_, noLocal_, exclusive_, nowait_, arguments_);
        return functor(consume);
    }
    
    
    R streamConsumeOk(const Str8& consumerTag_)
    {
        stream::ConsumeOk consumeOk(consumerTag_);
        return functor(consumeOk);
    }
    
    
    R streamCancel(const Str8& consumerTag_)
    {
        stream::Cancel cancel(consumerTag_);
        return functor(cancel);
    }
    
    
    R streamPublish(
        const exchange::Name& exchange_,
        const Str8& routingKey_,
        Bit mandatory_,
        Bit immediate_
    )
    {
        stream::Publish publish(exchange_, routingKey_, mandatory_, immediate_);
        return functor(publish);
    }
    
    
    R streamReturn(
        const stream::ReturnCode& replyCode_,
        const Str8& replyText_,
        const exchange::Name& exchange_,
        const Str8& routingKey_
    )
    {
        stream::Return return_(replyCode_, replyText_, exchange_, routingKey_);
        return functor(return_);
    }
    
    
    R streamDeliver(
        const Str8& consumerTag_,
        Uint64 deliveryTag_,
        const exchange::Name& exchange_,
        const queue::Name& queue_
    )
    {
        stream::Deliver deliver(consumerTag_, deliveryTag_, exchange_, queue_);
        return functor(deliver);
    }
    
    
    R clusterUpdateRequest(const Str16& url_)
    {
        cluster::UpdateRequest updateRequest(url_);
        return functor(updateRequest);
    }
    
    
    R clusterUpdateOffer(Uint64 updatee_)
    {
        cluster::UpdateOffer updateOffer(updatee_);
        return functor(updateOffer);
    }
    
    
    R clusterRetractOffer(Uint64 updatee_)
    {
        cluster::RetractOffer retractOffer(updatee_);
        return functor(retractOffer);
    }
    
    
    R clusterInitialStatus(
        Uint32 version_,
        Bit active_,
        const Uuid& clusterId_,
        const cluster::StoreState& storeState_,
        const Uuid& shutdownId_
    )
    {
        cluster::InitialStatus initialStatus(version_, active_, clusterId_, storeState_, shutdownId_);
        return functor(initialStatus);
    }
    
    
    R clusterReady(const Str16& url_)
    {
        cluster::Ready ready(url_);
        return functor(ready);
    }
    
    
    R clusterConfigChange(const Vbin16& current_)
    {
        cluster::ConfigChange configChange(current_);
        return functor(configChange);
    }
    
    
    R clusterMessageExpired(Uint64 id_)
    {
        cluster::MessageExpired messageExpired(id_);
        return functor(messageExpired);
    }
    
    
    R clusterErrorCheck(
        const cluster::ErrorType& type_,
        const SequenceNo& frameSeq_
    )
    {
        cluster::ErrorCheck errorCheck(type_, frameSeq_);
        return functor(errorCheck);
    }
    
    
    R clusterShutdown(const Uuid& shutdownId_)
    {
        cluster::Shutdown shutdown(shutdownId_);
        return functor(shutdown);
    }
    
    
    R clusterConnectionAnnounce(Uint32 ssf_)
    {
        cluster-connection::Announce announce(ssf_);
        return functor(announce);
    }
    
    
    R clusterConnectionDeliverClose()
    {
        cluster-connection::DeliverClose deliverClose;
        return functor(deliverClose);
    }
    
    
    R clusterConnectionDeliverDoOutput(Uint32 limit_)
    {
        cluster-connection::DeliverDoOutput deliverDoOutput(limit_);
        return functor(deliverDoOutput);
    }
    
    
    R clusterConnectionAbort()
    {
        cluster-connection::Abort abort;
        return functor(abort);
    }
    
    
    R clusterConnectionConsumerState(
        const Str8& name_,
        Bit blocked_,
        Bit notifyEnabled_,
        const SequenceNo& position_
    )
    {
        cluster-connection::ConsumerState consumerState(name_, blocked_, notifyEnabled_, position_);
        return functor(consumerState);
    }
    
    
    R clusterConnectionDeliveryRecord(
        const Str8& queue_,
        const SequenceNo& position_,
        const Str8& tag_,
        const SequenceNo& id_,
        Bit acquired_,
        Bit accepted_,
        Bit cancelled_,
        Bit completed_,
        Bit ended_,
        Bit windowing_,
        Bit enqueued_,
        Uint32 credit_
    )
    {
        cluster-connection::DeliveryRecord deliveryRecord(queue_, position_, tag_, id_, acquired_, accepted_, cancelled_, completed_, ended_, windowing_, enqueued_, credit_);
        return functor(deliveryRecord);
    }
    
    
    R clusterConnectionTxStart()
    {
        cluster-connection::TxStart txStart;
        return functor(txStart);
    }
    
    
    R clusterConnectionTxAccept(const SequenceSet& commands_)
    {
        cluster-connection::TxAccept txAccept(commands_);
        return functor(txAccept);
    }
    
    
    R clusterConnectionTxDequeue(const Str8& queue_)
    {
        cluster-connection::TxDequeue txDequeue(queue_);
        return functor(txDequeue);
    }
    
    
    R clusterConnectionTxEnqueue(const Str8& queue_)
    {
        cluster-connection::TxEnqueue txEnqueue(queue_);
        return functor(txEnqueue);
    }
    
    
    R clusterConnectionTxPublish(
        const ArrayDomain<Str8> & queues_,
        Bit delivered_
    )
    {
        cluster-connection::TxPublish txPublish(queues_, delivered_);
        return functor(txPublish);
    }
    
    
    R clusterConnectionTxEnd()
    {
        cluster-connection::TxEnd txEnd;
        return functor(txEnd);
    }
    
    
    R clusterConnectionAccumulatedAck(const SequenceSet& commands_)
    {
        cluster-connection::AccumulatedAck accumulatedAck(commands_);
        return functor(accumulatedAck);
    }
    
    
    R clusterConnectionOutputTask(
        Uint16 channel_,
        const Str8& name_
    )
    {
        cluster-connection::OutputTask outputTask(channel_, name_);
        return functor(outputTask);
    }
    
    
    R clusterConnectionSessionState(
        const SequenceNo& replayStart_,
        const SequenceNo& commandPoint_,
        const SequenceSet& sentIncomplete_,
        const SequenceNo& expected_,
        const SequenceNo& received_,
        const SequenceSet& unknownCompleted_,
        const SequenceSet& receivedIncomplete_
    )
    {
        cluster-connection::SessionState sessionState(replayStart_, commandPoint_, sentIncomplete_, expected_, received_, unknownCompleted_, receivedIncomplete_);
        return functor(sessionState);
    }
    
    
    R clusterConnectionShadowReady(
        Uint64 memberId_,
        Uint64 connectionId_,
        const Str8& userName_,
        const Str32& fragment_,
        Uint32 sendMax_
    )
    {
        cluster-connection::ShadowReady shadowReady(memberId_, connectionId_, userName_, fragment_, sendMax_);
        return functor(shadowReady);
    }
    
    
    R clusterConnectionMembership(
        const Map& joiners_,
        const Map& members_,
        const SequenceNo& frameSeq_
    )
    {
        cluster-connection::Membership membership(joiners_, members_, frameSeq_);
        return functor(membership);
    }
    
    
    R clusterConnectionRetractOffer()
    {
        cluster-connection::RetractOffer retractOffer;
        return functor(retractOffer);
    }
    
    
    R clusterConnectionQueuePosition(
        const Str8& queue_,
        const SequenceNo& position_
    )
    {
        cluster-connection::QueuePosition queuePosition(queue_, position_);
        return functor(queuePosition);
    }
    
    
    R clusterConnectionExchange(const Str32& encoded_)
    {
        cluster-connection::Exchange exchange(encoded_);
        return functor(exchange);
    }
    
    
    R clusterConnectionQueue(const Str32& encoded_)
    {
        cluster-connection::Queue queue(encoded_);
        return functor(queue);
    }
    
    
    R clusterConnectionExpiryId(Uint64 expiryId_)
    {
        cluster-connection::ExpiryId expiryId(expiryId_);
        return functor(expiryId);
    }
    
    
    R clusterConnectionAddQueueListener(
        const Str8& queue_,
        Uint32 consumer_
    )
    {
        cluster-connection::AddQueueListener addQueueListener(queue_, consumer_);
        return functor(addQueueListener);
    }
  private:
    F functor;
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_PROXYTEMPLATE_H*/
