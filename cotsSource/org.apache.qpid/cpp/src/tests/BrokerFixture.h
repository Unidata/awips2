#ifndef TESTS_BROKERFIXTURE_H
#define TESTS_BROKERFIXTURE_H

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

#include "SocketProxy.h"

#include "qpid/broker/Broker.h"
#include "qpid/client/Connection.h"
#include "qpid/client/ConnectionImpl.h"
#include "qpid/client/Session.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/LocalQueue.h"
#include "qpid/log/Logger.h"
#include "qpid/log/Options.h"
#include "qpid/sys/Thread.h"
#include <boost/noncopyable.hpp>

namespace qpid {
namespace tests {

/**
 * A fixture with an in-process broker.
 */
struct  BrokerFixture : private boost::noncopyable {
    typedef qpid::broker::Broker Broker;
    typedef boost::intrusive_ptr<Broker> BrokerPtr;

    BrokerPtr broker;
    qpid::sys::Thread brokerThread;

    BrokerFixture(Broker::Options opts=Broker::Options()) {
        // Keep the tests quiet unless logging env. vars have been set by user.
        if (!::getenv("QPID_LOG_ENABLE") && !::getenv("QPID_TRACE")) {
            qpid::log::Options logOpts;
            logOpts.selectors.clear();
            logOpts.selectors.push_back("error+");
            qpid::log::Logger::instance().configure(logOpts);
        }
        opts.port=0;
        // Management doesn't play well with multiple in-process brokers.
        opts.enableMgmt=false;
        opts.workerThreads=1;
        opts.dataDir="";
        opts.auth=false;
        broker = Broker::create(opts);
        // TODO aconway 2007-12-05: At one point BrokerFixture
        // tests could hang in Connection ctor if the following
        // line is removed. This may not be an issue anymore.
        broker->accept();
        broker->getPort(qpid::broker::Broker::TCP_TRANSPORT);
        brokerThread = qpid::sys::Thread(*broker);
    };

    void shutdownBroker()
    {
        broker->shutdown();
        broker = BrokerPtr();
    }

    ~BrokerFixture() {
        if (broker) broker->shutdown();
        brokerThread.join();
    }

    /** Open a connection to the broker. */
    void open(qpid::client::Connection& c) {
        c.open("localhost", broker->getPort(qpid::broker::Broker::TCP_TRANSPORT));
    }

    uint16_t getPort() { return broker->getPort(qpid::broker::Broker::TCP_TRANSPORT); }
};

/** Connection that opens in its constructor */
struct LocalConnection : public qpid::client::Connection {
    LocalConnection(uint16_t port) { open("localhost", port); }
    LocalConnection(const qpid::client::ConnectionSettings& s) { open(s); }
    ~LocalConnection() { close(); }
};

/** A local client connection via a socket proxy. */
struct ProxyConnection : public qpid::client::Connection {
    SocketProxy proxy;
    ProxyConnection(int brokerPort) : proxy(brokerPort) {
        open("localhost", proxy.getPort());
    }
    ProxyConnection(const qpid::client::ConnectionSettings& s) : proxy(s.port) {
        qpid::client::ConnectionSettings proxySettings(s);
        proxySettings.port = proxy.getPort();
        open(proxySettings);
    }
    ~ProxyConnection() { close(); }
};

/** Convenience class to create and open a connection and session
 * and some related useful objects.
 */
template <class ConnectionType=LocalConnection, class SessionType=qpid::client::Session>
struct ClientT {
    ConnectionType connection;
    SessionType session;
    qpid::client::SubscriptionManager subs;
    qpid::client::LocalQueue lq;
    std::string name;

    ClientT(uint16_t port, const std::string& name_=std::string())
        : connection(port), session(connection.newSession(name_)), subs(session), name(name_) {}
    ClientT(const qpid::client::ConnectionSettings& settings, const std::string& name_=std::string())
        : connection(settings), session(connection.newSession(name_)), subs(session), name(name_) {}

    ~ClientT() { close(); }
    void close() { session.close(); connection.close(); }
};

typedef ClientT<> Client;

/**
 * A BrokerFixture and ready-connected BrokerFixture::Client all in one.
 */
template <class ConnectionType, class SessionType=qpid::client::Session>
struct  SessionFixtureT : BrokerFixture, ClientT<ConnectionType,SessionType> {

    SessionFixtureT(Broker::Options opts=Broker::Options()) :
        BrokerFixture(opts),
        ClientT<ConnectionType,SessionType>(broker->getPort(qpid::broker::Broker::TCP_TRANSPORT))
    {}

};

typedef SessionFixtureT<LocalConnection> SessionFixture;
typedef SessionFixtureT<ProxyConnection> ProxySessionFixture;

}} // namespace qpid::tests

#endif  /*!TESTS_BROKERFIXTURE_H*/
