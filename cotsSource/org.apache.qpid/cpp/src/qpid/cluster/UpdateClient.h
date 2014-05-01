#ifndef QPID_CLUSTER_UPDATECLIENT_H
#define QPID_CLUSTER_UPDATECLIENT_H

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

#include "qpid/cluster/ClusterMap.h"
#include "qpid/cluster/Numbering.h"
#include "qpid/client/Connection.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/broker/SemanticState.h"
#include "qpid/sys/Runnable.h"
#include <boost/shared_ptr.hpp>


namespace qpid {

class Url;

namespace broker {

class Broker;
class Queue;
class Exchange;
class QueueBindings;
class QueueBinding;
class QueuedMessage;
class SessionHandler;
class DeliveryRecord;
class SessionState;
class SemanticState;
class Decoder;

} // namespace broker

namespace cluster {

class Cluster;
class Connection;
class ClusterMap;
class Decoder;
class ExpiryPolicy;

/**
 * A client that updates the contents of a local broker to a remote one using AMQP.
 */
class UpdateClient : public sys::Runnable {
  public:
    static const std::string UPDATE; // Name for special update queue and exchange.
    static client::Connection catchUpConnection();
    
    UpdateClient(const MemberId& updater, const MemberId& updatee, const Url&,
                 broker::Broker& donor, const ClusterMap& map, ExpiryPolicy& expiry,
                 const std::vector<boost::intrusive_ptr<Connection> >&, Decoder&,
                 const boost::function<void()>& done,
                 const boost::function<void(const std::exception&)>& fail,
                 const client::ConnectionSettings& 
    );

    ~UpdateClient();
    void update();
    void run();                 // Will delete this when finished.

    void updateUnacked(const broker::DeliveryRecord&);

  private:
    void updateQueue(client::AsyncSession&, const boost::shared_ptr<broker::Queue>&);
    void updateNonExclusiveQueue(const boost::shared_ptr<broker::Queue>&);
    void updateExclusiveQueue(const boost::shared_ptr<broker::Queue>&);
    void updateExchange(const boost::shared_ptr<broker::Exchange>&);
    void updateMessage(const broker::QueuedMessage&);
    void updateMessageTo(const broker::QueuedMessage&, const std::string& queue, client::Session s);
    void updateBinding(client::AsyncSession&, const std::string& queue, const broker::QueueBinding& binding);
    void updateConnection(const boost::intrusive_ptr<Connection>& connection);
    void updateSession(broker::SessionHandler& s);
    void updateTxState(broker::SemanticState& s);
    void updateOutputTask(const sys::OutputTask* task);
    void updateConsumer(const broker::SemanticState::ConsumerImpl::shared_ptr&);
    void updateQueueListeners(const boost::shared_ptr<broker::Queue>&);
    void updateQueueListener(std::string& q, const boost::shared_ptr<broker::Consumer>& c);

    Numbering<broker::SemanticState::ConsumerImpl::shared_ptr> consumerNumbering;
    MemberId updaterId;
    MemberId updateeId;
    Url updateeUrl;
    broker::Broker& updaterBroker;
    ClusterMap map;
    ExpiryPolicy& expiry;
    std::vector<boost::intrusive_ptr<Connection> > connections;
    Decoder& decoder;
    client::Connection connection, shadowConnection;
    client::AsyncSession session, shadowSession;
    boost::function<void()> done;
    boost::function<void(const std::exception& e)> failed;
    client::ConnectionSettings connectionSettings;
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_UPDATECLIENT_H*/
