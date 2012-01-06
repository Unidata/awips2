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
#include "qpid/cluster/UpdateClient.h"
#include "qpid/cluster/Cluster.h"
#include "qpid/cluster/ClusterMap.h"
#include "qpid/cluster/Connection.h"
#include "qpid/cluster/Decoder.h"
#include "qpid/cluster/ExpiryPolicy.h"
#include "qpid/client/SessionBase_0_10Access.h" 
#include "qpid/client/ConnectionAccess.h" 
#include "qpid/client/SessionImpl.h" 
#include "qpid/client/ConnectionImpl.h"
#include "qpid/client/Future.h"
#include "qpid/broker/Broker.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/QueueRegistry.h"
#include "qpid/broker/Message.h"
#include "qpid/broker/Exchange.h"
#include "qpid/broker/ExchangeRegistry.h"
#include "qpid/broker/SessionHandler.h"
#include "qpid/broker/SessionState.h"
#include "qpid/broker/TxOpVisitor.h"
#include "qpid/broker/DtxAck.h"
#include "qpid/broker/TxAccept.h"
#include "qpid/broker/TxPublish.h"
#include "qpid/broker/RecoveredDequeue.h"
#include "qpid/broker/RecoveredEnqueue.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/framing/ClusterConnectionMembershipBody.h"
#include "qpid/framing/ClusterConnectionShadowReadyBody.h"
#include "qpid/framing/ClusterConnectionSessionStateBody.h"
#include "qpid/framing/ClusterConnectionConsumerStateBody.h"
#include "qpid/framing/enum.h"
#include "qpid/framing/ProtocolVersion.h"
#include "qpid/framing/TypeCode.h"
#include "qpid/log/Statement.h"
#include "qpid/Url.h"
#include <boost/bind.hpp>
#include <boost/cast.hpp>
#include <algorithm>

namespace qpid {
namespace cluster {

using broker::Broker;
using broker::Exchange;
using broker::Queue;
using broker::QueueBinding;
using broker::Message;
using broker::SemanticState;

using namespace framing;
namespace arg=client::arg;
using client::SessionBase_0_10Access;

struct ClusterConnectionProxy : public AMQP_AllProxy::ClusterConnection {
    ClusterConnectionProxy(client::Connection c) :
        AMQP_AllProxy::ClusterConnection(*client::ConnectionAccess::getImpl(c)) {}
    ClusterConnectionProxy(client::AsyncSession s) :
        AMQP_AllProxy::ClusterConnection(SessionBase_0_10Access(s).get()->out) {}
};

// Create a connection with special version that marks it as a catch-up connection.
client::Connection UpdateClient::catchUpConnection() {
    client::Connection c;
    client::ConnectionAccess::setVersion(c, ProtocolVersion(0x80 , 0x80 + 10));
    return c;
}

// Send a control body directly to the session.
void send(client::AsyncSession& s, const AMQBody& body) {
    client::SessionBase_0_10Access sb(s);
    sb.get()->send(body);
}

// TODO aconway 2008-09-24: optimization: update connections/sessions in parallel.

UpdateClient::UpdateClient(const MemberId& updater, const MemberId& updatee, const Url& url,
                           broker::Broker& broker, const ClusterMap& m, ExpiryPolicy& expiry_, 
                           const Cluster::ConnectionVector& cons, Decoder& decoder_,
                           const boost::function<void()>& ok,
                           const boost::function<void(const std::exception&)>& fail,
                           const client::ConnectionSettings& cs
)
    : updaterId(updater), updateeId(updatee), updateeUrl(url), updaterBroker(broker), map(m),
      expiry(expiry_), connections(cons), decoder(decoder_),
      connection(catchUpConnection()), shadowConnection(catchUpConnection()),
      done(ok), failed(fail), connectionSettings(cs)
{}

UpdateClient::~UpdateClient() {}

// Reserved exchange/queue name for catch-up, avoid clashes with user queues/exchanges.
const std::string UpdateClient::UPDATE("qpid.cluster-update");

void UpdateClient::run() {
    try {
        connection.open(updateeUrl, connectionSettings);
        session = connection.newSession(UPDATE);
        update();
        done();
    } catch (const std::exception& e) {
        failed(e);
    }
    delete this;
}

void UpdateClient::update() {
    QPID_LOG(debug, updaterId << " updating state to " << updateeId
             << " at " << updateeUrl);
    Broker& b = updaterBroker;
    b.getExchanges().eachExchange(boost::bind(&UpdateClient::updateExchange, this, _1));
    b.getQueues().eachQueue(boost::bind(&UpdateClient::updateNonExclusiveQueue, this, _1));

    // Update queue is used to transfer acquired messages that are no
    // longer on their original queue.
    session.queueDeclare(arg::queue=UPDATE, arg::autoDelete=true);
    session.sync();
    std::for_each(connections.begin(), connections.end(), boost::bind(&UpdateClient::updateConnection, this, _1));
    session.queueDelete(arg::queue=UPDATE);
    session.close();

    // Update queue listeners: must come after sessions so consumerNumbering is populated.
    b.getQueues().eachQueue(boost::bind(&UpdateClient::updateQueueListeners, this, _1));

    ClusterConnectionProxy(session).expiryId(expiry.getId());
    ClusterConnectionMembershipBody membership;
    map.toMethodBody(membership);
    AMQFrame frame(membership);
    client::ConnectionAccess::getImpl(connection)->handle(frame);

    connection.close();
    QPID_LOG(debug,  updaterId << " update completed to " << updateeId
             << " at " << updateeUrl << ": " << membership);
}

namespace {
template <class T> std::string encode(const T& t) {
    std::string encoded;
    encoded.resize(t.encodedSize());
    framing::Buffer buf(const_cast<char*>(encoded.data()), encoded.size());
    t.encode(buf);
    return encoded;
}
} // namespace

void UpdateClient::updateExchange(const boost::shared_ptr<Exchange>& ex) {
    QPID_LOG(debug, updaterId << " updating exchange " << ex->getName());
    ClusterConnectionProxy(session).exchange(encode(*ex));
}

/** Bind a queue to the update exchange and update messges to it
 * setting the message possition as needed.
 */
class MessageUpdater {
    std::string queue;
    bool haveLastPos;
    framing::SequenceNumber lastPos;
    client::AsyncSession session;
    ExpiryPolicy& expiry;
    
  public:

    MessageUpdater(const string& q, const client::AsyncSession s, ExpiryPolicy& expiry_) : queue(q), haveLastPos(false), session(s), expiry(expiry_) {
        session.exchangeBind(queue, UpdateClient::UPDATE);
    }

    ~MessageUpdater() {
        try {
            session.exchangeUnbind(queue, UpdateClient::UPDATE);
        }
        catch (const std::exception& e) {
            // Don't throw in a destructor.
            QPID_LOG(error, "Unbinding update queue " << queue << ": " << e.what());
        }
    }


    void updateQueuedMessage(const broker::QueuedMessage& message) {
        // Send the queue position if necessary.
        if (!haveLastPos || message.position - lastPos != 1)  {
            ClusterConnectionProxy(session).queuePosition(queue, message.position.getValue()-1);
            haveLastPos = true;
        }
        lastPos = message.position;

        // Send the expiry ID if necessary.
        if (message.payload->getProperties<DeliveryProperties>()->getTtl()) {
            boost::optional<uint64_t> expiryId = expiry.getId(*message.payload);
            if (!expiryId) return; // Message already expired, don't replicate.
            ClusterConnectionProxy(session).expiryId(*expiryId);
        }

        // We can't send a broker::Message via the normal client API,
        // and it would be expensive to copy it into a client::Message
        // so we go a bit under the client API covers here.
        //
        SessionBase_0_10Access sb(session);
        // Disable client code that clears the delivery-properties.exchange
        sb.get()->setDoClearDeliveryPropertiesExchange(false);
        framing::MessageTransferBody transfer(
            *message.payload->getFrames().as<framing::MessageTransferBody>());
        transfer.setDestination(UpdateClient::UPDATE);
        
        sb.get()->send(transfer, message.payload->getFrames(),
                       !message.payload->isContentReleased());
        if (message.payload->isContentReleased()){
            uint16_t maxFrameSize = sb.get()->getConnection()->getNegotiatedSettings().maxFrameSize;
            uint16_t maxContentSize = maxFrameSize - AMQFrame::frameOverhead();
            bool morecontent = true;
            for (uint64_t offset = 0; morecontent; offset += maxContentSize)
            {            
                AMQFrame frame((AMQContentBody()));
                morecontent = message.payload->getContentFrame(*(message.queue), frame, maxContentSize, offset);
                sb.get()->sendRawFrame(frame);
            }
        }
    }

    void updateMessage(const boost::intrusive_ptr<broker::Message>& message) {
        updateQueuedMessage(broker::QueuedMessage(0, message, haveLastPos? lastPos.getValue()+1 : 1));
    }
};

void UpdateClient::updateQueue(client::AsyncSession& s, const boost::shared_ptr<Queue>& q) {
    broker::Exchange::shared_ptr alternateExchange = q->getAlternateExchange();
    s.queueDeclare(
        arg::queue = q->getName(),
        arg::durable = q->isDurable(),
        arg::autoDelete = q->isAutoDelete(),
        arg::alternateExchange = alternateExchange ? alternateExchange->getName() : "",
        arg::arguments = q->getSettings(),
        arg::exclusive = q->hasExclusiveOwner()
    );
    MessageUpdater updater(q->getName(), s, expiry);
    q->eachMessage(boost::bind(&MessageUpdater::updateQueuedMessage, &updater, _1));
    q->eachBinding(boost::bind(&UpdateClient::updateBinding, this, s, q->getName(), _1));
    ClusterConnectionProxy(s).queuePosition(q->getName(), q->getPosition());
}

void UpdateClient::updateExclusiveQueue(const boost::shared_ptr<broker::Queue>& q) {
    QPID_LOG(debug, updaterId << " updating exclusive queue " << q->getName() << " on " << shadowSession.getId());
    updateQueue(shadowSession, q);
}

void UpdateClient::updateNonExclusiveQueue(const boost::shared_ptr<broker::Queue>& q) {
    if (!q->hasExclusiveOwner()) {
        QPID_LOG(debug, updaterId << " updating queue " << q->getName());
        updateQueue(session, q);
    }//else queue will be updated as part of session state of owning session
}

void UpdateClient::updateBinding(client::AsyncSession& s, const std::string& queue, const QueueBinding& binding) {
    s.exchangeBind(queue, binding.exchange, binding.key, binding.args);
}

void UpdateClient::updateOutputTask(const sys::OutputTask* task) {
    const SemanticState::ConsumerImpl* cci =
        boost::polymorphic_downcast<const SemanticState::ConsumerImpl*> (task);
    SemanticState::ConsumerImpl* ci = const_cast<SemanticState::ConsumerImpl*>(cci);
    uint16_t channel =  ci->getParent().getSession().getChannel();
    ClusterConnectionProxy(shadowConnection).outputTask(channel,  ci->getName());
    QPID_LOG(debug, updaterId << " updating output task " << ci->getName()
             << " channel=" << channel);
}

void UpdateClient::updateConnection(const boost::intrusive_ptr<Connection>& updateConnection) {
    QPID_LOG(debug, updaterId << " updating connection " << *updateConnection);
    shadowConnection = catchUpConnection();

    broker::Connection& bc = updateConnection->getBrokerConnection();
    connectionSettings.maxFrameSize = bc.getFrameMax();
    shadowConnection.open(updateeUrl, connectionSettings);
    bc.eachSessionHandler(boost::bind(&UpdateClient::updateSession, this, _1));
    // Safe to use decoder here because we are stalled for update.
    std::pair<const char*, size_t> fragment = decoder.get(updateConnection->getId()).getFragment();
    bc.getOutputTasks().eachOutput(
        boost::bind(&UpdateClient::updateOutputTask, this, _1));
    ClusterConnectionProxy(shadowConnection).shadowReady(
        updateConnection->getId().getMember(),
        updateConnection->getId().getNumber(),
        bc.getUserId(),
        string(fragment.first, fragment.second),
        updateConnection->getOutput().getSendMax()
    );
    shadowConnection.close();
    QPID_LOG(debug, updaterId << " updated connection " << *updateConnection);
}

void UpdateClient::updateSession(broker::SessionHandler& sh) {
    broker::SessionState* ss = sh.getSession();
    if (!ss) return;            // no session.

    QPID_LOG(debug, updaterId << " updating session " << &sh.getConnection()
             << "[" << sh.getChannel() << "] = " << ss->getId());

    // Create a client session to update session state. 
    boost::shared_ptr<client::ConnectionImpl> cimpl = client::ConnectionAccess::getImpl(shadowConnection);
    boost::shared_ptr<client::SessionImpl> simpl = cimpl->newSession(ss->getId().getName(), ss->getTimeout(), sh.getChannel());
    simpl->disableAutoDetach();
    client::SessionBase_0_10Access(shadowSession).set(simpl);
    AMQP_AllProxy::ClusterConnection proxy(simpl->out);

    // Re-create session state on remote connection.

    QPID_LOG(debug, updaterId << " updating exclusive queues.");
    ss->getSessionAdapter().eachExclusiveQueue(boost::bind(&UpdateClient::updateExclusiveQueue, this, _1));

    QPID_LOG(debug, updaterId << " updating consumers.");
    ss->getSemanticState().eachConsumer(
        boost::bind(&UpdateClient::updateConsumer, this, _1));

    QPID_LOG(debug, updaterId << " updating unacknowledged messages.");
    broker::DeliveryRecords& drs = ss->getSemanticState().getUnacked();
    std::for_each(drs.begin(), drs.end(),  boost::bind(&UpdateClient::updateUnacked, this, _1));

    updateTxState(ss->getSemanticState());           // Tx transaction state.

    // Adjust command counter for message in progress, will be sent after state update.
    boost::intrusive_ptr<Message> inProgress = ss->getMessageInProgress();
    SequenceNumber received = ss->receiverGetReceived().command;
    if (inProgress)  
        --received;
             
    // Reset command-sequence state.
    proxy.sessionState(
        ss->senderGetReplayPoint().command,
        ss->senderGetCommandPoint().command,
        ss->senderGetIncomplete(),
        std::max(received, ss->receiverGetExpected().command),
        received,
        ss->receiverGetUnknownComplete(),
        ss->receiverGetIncomplete()
    );

    // Send frames for partial message in progress.
    if (inProgress) {
        inProgress->getFrames().map(simpl->out);
    }
    QPID_LOG(debug, updaterId << " updated session " << sh.getSession()->getId());
}

void UpdateClient::updateConsumer(
    const broker::SemanticState::ConsumerImpl::shared_ptr& ci)
{
    QPID_LOG(debug, updaterId << " updating consumer " << ci->getName() << " on "
             << shadowSession.getId());

    using namespace message;
    shadowSession.messageSubscribe(
        arg::queue       = ci->getQueue()->getName(),
        arg::destination = ci->getName(),
        arg::acceptMode  = ci->isAckExpected() ? ACCEPT_MODE_EXPLICIT : ACCEPT_MODE_NONE,
        arg::acquireMode = ci->isAcquire() ? ACQUIRE_MODE_PRE_ACQUIRED : ACQUIRE_MODE_NOT_ACQUIRED,
        arg::exclusive   = ci->isExclusive(),
        arg::resumeId    = ci->getResumeId(),
        arg::resumeTtl   = ci->getResumeTtl(),
        arg::arguments   = ci->getArguments()
    );
    shadowSession.messageSetFlowMode(ci->getName(), ci->isWindowing() ? FLOW_MODE_WINDOW : FLOW_MODE_CREDIT);
    shadowSession.messageFlow(ci->getName(), CREDIT_UNIT_MESSAGE, ci->getMsgCredit());
    shadowSession.messageFlow(ci->getName(), CREDIT_UNIT_BYTE, ci->getByteCredit());
    ClusterConnectionProxy(shadowSession).consumerState(
        ci->getName(),
        ci->isBlocked(),
        ci->isNotifyEnabled(),
        ci->position
    );
    consumerNumbering.add(ci);

    QPID_LOG(debug, updaterId << " updated consumer " << ci->getName()
             << " on " << shadowSession.getId());
}
    
void UpdateClient::updateUnacked(const broker::DeliveryRecord& dr) {
    if (!dr.isEnded() && dr.isAcquired() && dr.getMessage().payload) {
        // If the message is acquired then it is no longer on the
        // updatees queue, put it on the update queue for updatee to pick up.
        //
        MessageUpdater(UPDATE, shadowSession, expiry).updateQueuedMessage(dr.getMessage());
    }
    ClusterConnectionProxy(shadowSession).deliveryRecord(
        dr.getQueue()->getName(),
        dr.getMessage().position,
        dr.getTag(),
        dr.getId(),
        dr.isAcquired(),
        dr.isAccepted(),
        dr.isCancelled(),
        dr.isComplete(),
        dr.isEnded(),
        dr.isWindowing(),
        dr.getQueue()->isEnqueued(dr.getMessage()),
        dr.getCredit()
    );
}

class TxOpUpdater : public broker::TxOpConstVisitor, public MessageUpdater {
  public:
    TxOpUpdater(UpdateClient& dc, client::AsyncSession s, ExpiryPolicy& expiry)
        : MessageUpdater(UpdateClient::UPDATE, s, expiry), parent(dc), session(s), proxy(s) {}

    void operator()(const broker::DtxAck& ) {
        throw InternalErrorException("DTX transactions not currently supported by cluster.");
    }
    
    void operator()(const broker::RecoveredDequeue& rdeq) {
        updateMessage(rdeq.getMessage());
        proxy.txEnqueue(rdeq.getQueue()->getName());
    }

    void operator()(const broker::RecoveredEnqueue& renq) {
        updateMessage(renq.getMessage());
        proxy.txEnqueue(renq.getQueue()->getName());
    }

    void operator()(const broker::TxAccept& txAccept) {
        proxy.txAccept(txAccept.getAcked());
    }

    void operator()(const broker::TxPublish& txPub) {
        updateMessage(txPub.getMessage());
        typedef std::list<Queue::shared_ptr> QueueList;
        const QueueList& qlist = txPub.getQueues();
        Array qarray(TYPE_CODE_STR8);
        for (QueueList::const_iterator i = qlist.begin(); i != qlist.end(); ++i) 
            qarray.push_back(Array::ValuePtr(new Str8Value((*i)->getName())));
        proxy.txPublish(qarray, txPub.delivered);
    }

  private:
    UpdateClient& parent;
    client::AsyncSession session;
    ClusterConnectionProxy proxy;
};
    
void UpdateClient::updateTxState(broker::SemanticState& s) {
    QPID_LOG(debug, updaterId << " updating TX transaction state.");
    ClusterConnectionProxy proxy(shadowSession);
    proxy.accumulatedAck(s.getAccumulatedAck());
    broker::TxBuffer::shared_ptr txBuffer = s.getTxBuffer();
    if (txBuffer) {
        proxy.txStart();
        TxOpUpdater updater(*this, shadowSession, expiry);
        txBuffer->accept(updater);
        proxy.txEnd();
    }
}

void UpdateClient::updateQueueListeners(const boost::shared_ptr<broker::Queue>& queue) {
    queue->getListeners().eachListener(
        boost::bind(&UpdateClient::updateQueueListener, this, queue->getName(), _1));
}

void UpdateClient::updateQueueListener(std::string& q,
                                  const boost::shared_ptr<broker::Consumer>& c)
{
    const boost::shared_ptr<SemanticState::ConsumerImpl> ci =
        boost::dynamic_pointer_cast<SemanticState::ConsumerImpl>(c);
    size_t n = consumerNumbering[ci];
    if (n >= consumerNumbering.size())
        throw Exception(QPID_MSG("Unexpected listener on queue " << q));
    ClusterConnectionProxy(session).addQueueListener(q, n);
}

}} // namespace qpid::cluster
