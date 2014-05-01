#ifndef QPID_CLIENT_FAILOVERLISTENER_H
#define QPID_CLIENT_FAILOVERLISTENER_H

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

#include "qpid/client/ClientImportExport.h"
#include "qpid/client/MessageListener.h"
#include "qpid/client/Connection.h"
#include "qpid/client/Session.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/Url.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/Thread.h"
#include <vector>

namespace qpid {
namespace client {


/**
 * Listen for updates from the amq.failover exchange.
 *
 * In a cluster, the amq.failover exchange provides updates whenever
 * the cluster membership changes. This class subscribes to the
 * failover exchange and providees the latest list of known brokers.
 *
 * You can also subscribe to amq.failover yourself and use
 * FailoverListener::decode to extract a list of broker URLs from a
 * failover exchange message.
 */
class FailoverListener : private MessageListener, private qpid::sys::Runnable 
{
  public:
    /** The name of the standard failover exchange amq.failover */
    static QPID_CLIENT_EXTERN const std::string AMQ_FAILOVER;

    /** Extract the broker list from a failover exchange message */
    static QPID_CLIENT_EXTERN std::vector<Url> getKnownBrokers(const Message& m);
    
    /** Subscribe to amq.failover exchange. */
    QPID_CLIENT_EXTERN FailoverListener(Connection);

    QPID_CLIENT_EXTERN ~FailoverListener();

    /** Returns the latest list of known broker URLs. */
    QPID_CLIENT_EXTERN std::vector<Url> getKnownBrokers() const;
    
  private:
    void received(Message& msg);
    void run();

    mutable sys::Mutex lock;
    Connection connection;
    Session session;
    SubscriptionManager subscriptions;
    sys::Thread thread;
    std::vector<Url> knownBrokers;
};
}} // namespace qpid::client

#endif  /*!QPID_CLIENT_FAILOVERLISTENER_H*/
