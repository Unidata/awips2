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
#include "Connection.h"
#include "UpdateClient.h"
#include "Cluster.h"
#include "UpdateReceiver.h"

#include "qpid/broker/SessionState.h"
#include "qpid/broker/SemanticState.h"
#include "qpid/broker/TxBuffer.h"
#include "qpid/broker/TxPublish.h"
#include "qpid/broker/TxAccept.h"
#include "qpid/broker/RecoveredEnqueue.h"
#include "qpid/broker/RecoveredDequeue.h"
#include "qpid/broker/Exchange.h"
#include "qpid/broker/Queue.h"
#include "qpid/framing/enum.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/AllInvoker.h"
#include "qpid/framing/DeliveryProperties.h"
#include "qpid/framing/ClusterConnectionDeliverCloseBody.h"
#include "qpid/framing/ConnectionCloseBody.h"
#include "qpid/framing/ConnectionCloseOkBody.h"
#include "qpid/log/Statement.h"

#include <boost/current_function.hpp>

// TODO aconway 2008-11-03:
// 
// Refactor code for receiving an update into a separate UpdateConnection
// class.
//


namespace qpid {
namespace cluster {

using namespace framing;
using namespace framing::cluster;

qpid::sys::AtomicValue<uint64_t> Connection::catchUpId(0x5000000000000000LL);

Connection::NullFrameHandler Connection::nullFrameHandler;

struct NullFrameHandler : public framing::FrameHandler {
    void handle(framing::AMQFrame&) {}
};


namespace {
sys::AtomicValue<uint64_t> idCounter;
const std::string shadowPrefix("[shadow]");
}


// Shadow connection
    Connection::Connection(Cluster& c, sys::ConnectionOutputHandler& out, const std::string& logId,
                           const ConnectionId& id, unsigned int ssf)
    : cluster(c), self(id), catchUp(false), output(*this, out),
      connection(&output, cluster.getBroker(), shadowPrefix+logId, ssf), expectProtocolHeader(false),
      mcastFrameHandler(cluster.getMulticast(), self),
      consumerNumbering(c.getUpdateReceiver().consumerNumbering)
{ init(); }

// Local connection
Connection::Connection(Cluster& c, sys::ConnectionOutputHandler& out,
                       const std::string& logId, MemberId member,
                       bool isCatchUp, bool isLink, unsigned int ssf
) : cluster(c), self(member, ++idCounter), catchUp(isCatchUp), output(*this, out),
    connection(&output, cluster.getBroker(),
               isCatchUp ? shadowPrefix+logId : logId,
               ssf,
               isLink,
               isCatchUp ? ++catchUpId : 0),
    expectProtocolHeader(isLink), mcastFrameHandler(cluster.getMulticast(), self),
    consumerNumbering(c.getUpdateReceiver().consumerNumbering)
{ init(); }

void Connection::init() {
    QPID_LOG(debug, cluster << " new connection: " << *this);
    if (isLocalClient()) {  
        connection.setClusterOrderOutput(mcastFrameHandler); // Actively send cluster-order frames from local node
        cluster.addLocalConnection(this);
        giveReadCredit(cluster.getSettings().readMax);
    }
    else {                                                  // Shadow or catch-up connection
        connection.setClusterOrderOutput(nullFrameHandler); // Passive, discard cluster-order frames
        connection.setClientThrottling(false);              // Disable client throttling, done by active node.
        connection.setShadow(); // Mark the broker connection as a shadow.
    }
    if (!isCatchUp())
        connection.setErrorListener(this);
}

void Connection::giveReadCredit(int credit) {
    if (cluster.getSettings().readMax && credit) 
        output.giveReadCredit(credit);
}

Connection::~Connection() {
    connection.setErrorListener(0);
    QPID_LOG(debug, cluster << " deleted connection: " << *this);
}

bool Connection::doOutput() {
    return output.doOutput();
}

// Received from a directly connected client.
void Connection::received(framing::AMQFrame& f) {
    QPID_LOG(trace, cluster << " RECV " << *this << ": " << f);
    if (isLocal()) {            // Local catch-up connection.
        currentChannel = f.getChannel();
        if (!framing::invoke(*this, *f.getBody()).wasHandled())
            connection.received(f);
    }
    else {             // Shadow or updated catch-up connection.
        if (f.getMethod() && f.getMethod()->isA<ConnectionCloseBody>()) {
            if (isShadow()) 
                cluster.addShadowConnection(this);
            AMQFrame ok((ConnectionCloseOkBody()));
            connection.getOutput().send(ok);
            output.closeOutput();
            catchUp = false;
        }
        else
            QPID_LOG(warning, cluster << " ignoring unexpected frame " << *this << ": " << f);
    }
}

bool Connection::checkUnsupported(const AMQBody& body) {
    std::string message;
    if (body.getMethod()) {
        switch (body.getMethod()->amqpClassId()) {
          case DTX_CLASS_ID: message = "DTX transactions are not currently supported by cluster."; break;
        }
    }
    if (!message.empty())
        connection.close(connection::CLOSE_CODE_FRAMING_ERROR, message);
    return !message.empty();
}

struct GiveReadCreditOnExit {
    Connection& connection;
    int credit;
    GiveReadCreditOnExit(Connection& connection_, int credit_) :
        connection(connection_), credit(credit_) {}
    ~GiveReadCreditOnExit() { connection.giveReadCredit(credit); }
};

// Called in delivery thread, in cluster order.
void Connection::deliveredFrame(const EventFrame& f) {
    GiveReadCreditOnExit gc(*this, f.readCredit);
    assert(!catchUp);
    currentChannel = f.frame.getChannel(); 
    if (f.frame.getBody()       // frame can be emtpy with just readCredit
        && !framing::invoke(*this, *f.frame.getBody()).wasHandled() // Connection contol.
        && !checkUnsupported(*f.frame.getBody())) // Unsupported operation.
    {
        if (f.type == DATA) // incoming data frames to broker::Connection
            connection.received(const_cast<AMQFrame&>(f.frame)); 
        else {           // frame control, send frame via SessionState
            broker::SessionState* ss = connection.getChannel(currentChannel).getSession();
            if (ss) ss->out(const_cast<AMQFrame&>(f.frame));
        }
    }
}

// A local connection is closed by the network layer.
void Connection::closed() {
    try {
        if (catchUp) {
            QPID_LOG(critical, cluster << " catch-up connection closed prematurely " << *this);
            cluster.leave();
        }
        else if (isUpdated()) {
            QPID_LOG(debug, cluster << " closed update connection " << *this);
            connection.closed();
        }
        else if (isLocal()) {
            QPID_LOG(debug, cluster << " local close of replicated connection " << *this);
            // This was a local replicated connection. Multicast a deliver
            // closed and process any outstanding frames from the cluster
            // until self-delivery of deliver-close.
            output.closeOutput();
            cluster.getMulticast().mcastControl(ClusterConnectionDeliverCloseBody(), self);
        }
    }
    catch (const std::exception& e) {
        QPID_LOG(error, cluster << " error closing connection " << *this << ": " << e.what());
    }
}

// Self-delivery of close message, close the connection.
void Connection::deliverClose () {
    assert(!catchUp);
    connection.closed();
    cluster.erase(self);
}

// The connection has been killed for misbehaving
void Connection::abort() {
    connection.abort();
    cluster.erase(self);
}

// Member of a shadow connection left the cluster.
void Connection::left() {
    assert(isShadow());
    connection.closed();
}

// ConnectoinCodec::decode receives read buffers from  directly-connected clients.
size_t Connection::decode(const char* buffer, size_t size) {
    if (catchUp) {  // Handle catch-up locally.
        Buffer buf(const_cast<char*>(buffer), size);
        while (localDecoder.decode(buf))
            received(localDecoder.getFrame());
    }
    else {                      // Multicast local connections.
        assert(isLocal());
        const char* remainingData = buffer;
        size_t remainingSize = size;
        if (expectProtocolHeader) {
            //If this is an outgoing link, we will receive a protocol
            //header which needs to be decoded first
            framing::ProtocolInitiation pi;
            Buffer buf(const_cast<char*>(buffer), size);
            if (pi.decode(buf)) {
                //TODO: check the version is correct
                QPID_LOG(debug, "Outgoing clustered link connection received INIT(" << pi << ")");
                expectProtocolHeader = false;
                remainingData = buffer + pi.encodedSize();
                remainingSize = size - pi.encodedSize();
            } else {
                QPID_LOG(debug, "Not enough data for protocol header on outgoing clustered link");
                giveReadCredit(1); // We're not going to mcast so give read credit now.
                return 0;
            }
        }
        cluster.getMulticast().mcastBuffer(remainingData, remainingSize, self);
    }
    return size;
}

broker::SessionState& Connection::sessionState() {
    return *connection.getChannel(currentChannel).getSession();
}

broker::SemanticState& Connection::semanticState() {
    return sessionState().getSemanticState();
}

void Connection::consumerState(const string& name, bool blocked, bool notifyEnabled, const SequenceNumber& position)
{
    broker::SemanticState::ConsumerImpl& c = semanticState().find(name);
    c.position = position;
    c.setBlocked(blocked);
    if (notifyEnabled) c.enableNotify(); else c.disableNotify();
    consumerNumbering.add(c.shared_from_this());
}


void Connection::sessionState(
    const SequenceNumber& replayStart,
    const SequenceNumber& sendCommandPoint,
    const SequenceSet& sentIncomplete,
    const SequenceNumber& expected,
    const SequenceNumber& received,
    const SequenceSet& unknownCompleted,
    const SequenceSet& receivedIncomplete)
{
    
    sessionState().setState(
        replayStart,
        sendCommandPoint,
        sentIncomplete,
        expected,
        received,
        unknownCompleted,
        receivedIncomplete);
    QPID_LOG(debug, cluster << " received session state update for " << sessionState().getId());
    // The output tasks will be added later in the update process. 
    connection.getOutputTasks().removeAll();
}

void Connection::outputTask(uint16_t channel, const std::string& name) {
    broker::SessionState* session = connection.getChannel(channel).getSession();
    if (!session)
        throw Exception(QPID_MSG(cluster << " channel not attached " << *this
                                 << "[" <<  channel << "] "));
    OutputTask* task = &session->getSemanticState().find(name);
    connection.getOutputTasks().addOutputTask(task);
}
    
void Connection::shadowReady(uint64_t memberId, uint64_t connectionId, const string& username, const string& fragment, uint32_t sendMax) {
    ConnectionId shadowId = ConnectionId(memberId, connectionId);
    QPID_LOG(debug, cluster << " catch-up connection " << *this << " becomes shadow " << shadowId);
    self = shadowId;
    connection.setUserId(username);
    // OK to use decoder here because cluster is stalled for update.
    cluster.getDecoder().get(self).setFragment(fragment.data(), fragment.size());
    connection.setErrorListener(this);
    output.setSendMax(sendMax);
}

void Connection::membership(const FieldTable& joiners, const FieldTable& members, const framing::SequenceNumber& frameSeq) {
    QPID_LOG(debug, cluster << " incoming update complete on connection " << *this);
    cluster.updateInDone(ClusterMap(joiners, members, frameSeq));
    consumerNumbering.clear();
    self.second = 0;        // Mark this as completed update connection.
}

void Connection::retractOffer() {
    QPID_LOG(debug, cluster << " incoming update retracted on connection " << *this);
    cluster.updateInRetracted();
    self.second = 0;        // Mark this as completed update connection.
}

bool Connection::isLocal() const {
    return self.first == cluster.getId() && self.second;
}

bool Connection::isShadow() const {
    return self.first != cluster.getId();
}

bool Connection::isUpdated() const {
    return self.first == cluster.getId() && self.second == 0;
}


boost::shared_ptr<broker::Queue> Connection::findQueue(const std::string& qname) {
    boost::shared_ptr<broker::Queue> queue = cluster.getBroker().getQueues().find(qname);
    if (!queue) throw Exception(QPID_MSG(cluster << " can't find queue " << qname));
    return queue;
}

broker::QueuedMessage Connection::getUpdateMessage() {
    boost::shared_ptr<broker::Queue> updateq = findQueue(UpdateClient::UPDATE);
    assert(!updateq->isDurable());
    broker::QueuedMessage m = updateq->get();
    if (!m.payload) throw Exception(QPID_MSG(cluster << " empty update queue"));
    return m;
}

void Connection::deliveryRecord(const string& qname,
                                const SequenceNumber& position,
                                const string& tag,
                                const SequenceNumber& id,
                                bool acquired,
                                bool accepted,
                                bool cancelled,
                                bool completed,
                                bool ended,
                                bool windowing,
                                bool enqueued,
                                uint32_t credit)
{
    broker::QueuedMessage m;
    broker::Queue::shared_ptr queue = findQueue(qname);
    if (!ended) {               // Has a message
        if (acquired) {         // Message is on the update queue
            m = getUpdateMessage();
            m.queue = queue.get();
            m.position = position;
            if (enqueued) queue->enqueued(m); //inform queue of the message 
        } else {                // Message at original position in original queue
            m = queue->find(position);
        }
        if (!m.payload)
            throw Exception(QPID_MSG("deliveryRecord no update message"));
    }

    broker::DeliveryRecord dr(m, queue, tag, acquired, accepted, windowing, credit);
    dr.setId(id);
    if (cancelled) dr.cancel(dr.getTag());
    if (completed) dr.complete();
    if (ended) dr.setEnded();   // Exsitance of message
    semanticState().record(dr); // Part of the session's unacked list.
}

void Connection::queuePosition(const string& qname, const SequenceNumber& position) {
    findQueue(qname)->setPosition(position);
}

void Connection::expiryId(uint64_t id) {
    cluster.getExpiryPolicy().setId(id);
}

std::ostream& operator<<(std::ostream& o, const Connection& c) {
    const char* type="unknown";
    if (c.isLocal()) type = "local";
    else if (c.isShadow()) type = "shadow";
    else if (c.isUpdated()) type = "updated";
    return o << c.getId() << "(" << type << (c.isCatchUp() ? ",catchup" : "") << ")";
}

void Connection::txStart() {
    txBuffer.reset(new broker::TxBuffer());
}
void Connection::txAccept(const framing::SequenceSet& acked) {
    txBuffer->enlist(boost::shared_ptr<broker::TxAccept>(
                         new broker::TxAccept(acked, semanticState().getUnacked())));
}

void Connection::txDequeue(const std::string& queue) {
    txBuffer->enlist(boost::shared_ptr<broker::RecoveredDequeue>(
                         new broker::RecoveredDequeue(findQueue(queue), getUpdateMessage().payload)));
}

void Connection::txEnqueue(const std::string& queue) {
    txBuffer->enlist(boost::shared_ptr<broker::RecoveredEnqueue>(
                         new broker::RecoveredEnqueue(findQueue(queue), getUpdateMessage().payload)));
}

void Connection::txPublish(const framing::Array& queues, bool delivered) {
    boost::shared_ptr<broker::TxPublish> txPub(new broker::TxPublish(getUpdateMessage().payload));
    for (framing::Array::const_iterator i = queues.begin(); i != queues.end(); ++i) 
        txPub->deliverTo(findQueue((*i)->get<std::string>()));
    txPub->delivered = delivered;
    txBuffer->enlist(txPub);
}

void Connection::txEnd() {
    semanticState().setTxBuffer(txBuffer);
}

void Connection::accumulatedAck(const qpid::framing::SequenceSet& s) {
    semanticState().setAccumulatedAck(s);
}

void Connection::exchange(const std::string& encoded) {
    Buffer buf(const_cast<char*>(encoded.data()), encoded.size());
    broker::Exchange::shared_ptr ex = broker::Exchange::decode(cluster.getBroker().getExchanges(), buf);
    QPID_LOG(debug, cluster << " updated exchange " << ex->getName());
}

void Connection::queue(const std::string& encoded) {
    Buffer buf(const_cast<char*>(encoded.data()), encoded.size());
    broker::Queue::shared_ptr q = broker::Queue::decode(cluster.getBroker().getQueues(), buf);
    QPID_LOG(debug, cluster << " updated queue " << q->getName());
}

void Connection::sessionError(uint16_t , const std::string& msg) {
    cluster.flagError(*this, ERROR_TYPE_SESSION, msg);
    
}

void Connection::connectionError(const std::string& msg) {
    cluster.flagError(*this, ERROR_TYPE_CONNECTION, msg);
}

void Connection::addQueueListener(const std::string& q, uint32_t listener) {
    if (listener >= consumerNumbering.size())
        throw Exception(QPID_MSG("Invalid listener ID: " << listener));
    findQueue(q)->getListeners().addListener(consumerNumbering[listener]);
}

}} // Namespace qpid::cluster

