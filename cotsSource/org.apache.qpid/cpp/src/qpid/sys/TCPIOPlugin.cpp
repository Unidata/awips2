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

#include "qpid/sys/ProtocolFactory.h"
#include "qpid/sys/AsynchIOHandler.h"
#include "qpid/sys/AsynchIO.h"

#include "qpid/Plugin.h"
#include "qpid/sys/Socket.h"
#include "qpid/sys/Poller.h"
#include "qpid/broker/Broker.h"
#include "qpid/log/Statement.h"

#include <boost/bind.hpp>
#include <memory>

namespace qpid {
namespace sys {

class AsynchIOProtocolFactory : public ProtocolFactory {
    const bool tcpNoDelay;
    Socket listener;
    const uint16_t listeningPort;
    std::auto_ptr<AsynchAcceptor> acceptor;

  public:
    AsynchIOProtocolFactory(int16_t port, int backlog, bool nodelay);
    void accept(Poller::shared_ptr, ConnectionCodec::Factory*);
    void connect(Poller::shared_ptr, const std::string& host, int16_t port,
                 ConnectionCodec::Factory*,
                 ConnectFailedCallback);

    uint16_t getPort() const;
    std::string getHost() const;

  private:
    void established(Poller::shared_ptr, const Socket&, ConnectionCodec::Factory*,
                     bool isClient);
    void connectFailed(const Socket&, int, const std::string&, ConnectFailedCallback);
};

// Static instance to initialise plugin
static class TCPIOPlugin : public Plugin {
    void earlyInitialize(Target&) {
    }
    
    void initialize(Target& target) {
        broker::Broker* broker = dynamic_cast<broker::Broker*>(&target);
        // Only provide to a Broker
        if (broker) {
            const broker::Broker::Options& opts = broker->getOptions();
            ProtocolFactory::shared_ptr protocol(new AsynchIOProtocolFactory(opts.port, opts.connectionBacklog, 
                                                                             opts.tcpNoDelay));
            QPID_LOG(notice, "Listening on TCP port " << protocol->getPort());
            broker->registerProtocolFactory("tcp", protocol);
        }
    }
} tcpPlugin;

AsynchIOProtocolFactory::AsynchIOProtocolFactory(int16_t port, int backlog, bool nodelay) :
    tcpNoDelay(nodelay), listeningPort(listener.listen(port, backlog))
{}

void AsynchIOProtocolFactory::established(Poller::shared_ptr poller, const Socket& s,
                                          ConnectionCodec::Factory* f, bool isClient) {
    AsynchIOHandler* async = new AsynchIOHandler(s.getPeerAddress(), f);

    if (tcpNoDelay) {
        s.setTcpNoDelay();
        QPID_LOG(info, "Set TCP_NODELAY on connection to " << s.getPeerAddress());
    }

    if (isClient)
        async->setClient();
    AsynchIO* aio = AsynchIO::create
      (s,
       boost::bind(&AsynchIOHandler::readbuff, async, _1, _2),
       boost::bind(&AsynchIOHandler::eof, async, _1),
       boost::bind(&AsynchIOHandler::disconnect, async, _1),
       boost::bind(&AsynchIOHandler::closedSocket, async, _1, _2),
       boost::bind(&AsynchIOHandler::nobuffs, async, _1),
       boost::bind(&AsynchIOHandler::idle, async, _1));

    async->init(aio, 4);
    aio->start(poller);
}

uint16_t AsynchIOProtocolFactory::getPort() const {
    return listeningPort; // Immutable no need for lock.
}

std::string AsynchIOProtocolFactory::getHost() const {
    return listener.getSockname();
}

void AsynchIOProtocolFactory::accept(Poller::shared_ptr poller,
                                     ConnectionCodec::Factory* fact) {
    acceptor.reset(
        AsynchAcceptor::create(listener,
                           boost::bind(&AsynchIOProtocolFactory::established, this, poller, _1, fact, false)));
    acceptor->start(poller);
}

void AsynchIOProtocolFactory::connectFailed(
    const Socket& s, int ec, const std::string& emsg,
    ConnectFailedCallback failedCb)
{
    failedCb(ec, emsg);
    s.close();
    delete &s;
}

void AsynchIOProtocolFactory::connect(
    Poller::shared_ptr poller,
    const std::string& host, int16_t port,
    ConnectionCodec::Factory* fact,
    ConnectFailedCallback failed)
{
    // Note that the following logic does not cause a memory leak.
    // The allocated Socket is freed either by the AsynchConnector
    // upon connection failure or by the AsynchIO upon connection
    // shutdown.  The allocated AsynchConnector frees itself when it
    // is no longer needed.

    Socket* socket = new Socket();
    AsynchConnector::create(*socket,
                            poller,
                            host,
                            port,
                            boost::bind(&AsynchIOProtocolFactory::established,
                                        this, poller, _1, fact, true),
                            boost::bind(&AsynchIOProtocolFactory::connectFailed,
                                        this, _1, _2, _3, failed));
}

}} // namespace qpid::sys
