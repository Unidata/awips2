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

#include "qpid/Plugin.h"
#include "qpid/sys/ssl/check.h"
#include "qpid/sys/ssl/util.h"
#include "qpid/sys/ssl/SslHandler.h"
#include "qpid/sys/ssl/SslIo.h"
#include "qpid/sys/ssl/SslSocket.h"
#include "qpid/broker/Broker.h"
#include "qpid/log/Statement.h"

#include <boost/bind.hpp>
#include <memory>


namespace qpid {
namespace sys {

struct SslServerOptions : ssl::SslOptions
{
    uint16_t port;
    bool clientAuth;

    SslServerOptions() : port(5671),
                         clientAuth(false)
    {
        addOptions()
            ("ssl-port", optValue(port, "PORT"), "Port on which to listen for SSL connections")
            ("ssl-require-client-authentication", optValue(clientAuth), 
             "Forces clients to authenticate in order to establish an SSL connection");
    }
};

class SslProtocolFactory : public ProtocolFactory {
    const bool tcpNoDelay;
    qpid::sys::ssl::SslSocket listener;
    const uint16_t listeningPort;
    std::auto_ptr<qpid::sys::ssl::SslAcceptor> acceptor;

  public:
    SslProtocolFactory(const SslServerOptions&, int backlog, bool nodelay);
    void accept(Poller::shared_ptr, ConnectionCodec::Factory*);
    void connect(Poller::shared_ptr, const std::string& host, int16_t port,
                 ConnectionCodec::Factory*,
                 boost::function2<void, int, std::string> failed);

    uint16_t getPort() const;
    std::string getHost() const;
    bool supports(const std::string& capability);

  private:
    void established(Poller::shared_ptr, const qpid::sys::ssl::SslSocket&, ConnectionCodec::Factory*,
                     bool isClient);
};

// Static instance to initialise plugin
static struct SslPlugin : public Plugin {
    SslServerOptions options;

    Options* getOptions() { return &options; }

    ~SslPlugin() { ssl::shutdownNSS(); }

    void earlyInitialize(Target&) {
    }
    
    void initialize(Target& target) {
        broker::Broker* broker = dynamic_cast<broker::Broker*>(&target);
        // Only provide to a Broker
        if (broker) {
            if (options.certDbPath.empty()) {
                QPID_LOG(info, "SSL plugin not enabled, you must set --ssl-cert-db to enable it.");                    
            } else {
                try {
                    ssl::initNSS(options, true);
                    
                    const broker::Broker::Options& opts = broker->getOptions();
                    ProtocolFactory::shared_ptr protocol(new SslProtocolFactory(options,
                                                                                opts.connectionBacklog, opts.tcpNoDelay));
                    QPID_LOG(notice, "Listening for SSL connections on TCP port " << protocol->getPort());
                    broker->registerProtocolFactory("ssl", protocol);
                } catch (const std::exception& e) {
                    QPID_LOG(error, "Failed to initialise SSL plugin: " << e.what());
                }
            }
        }
    }
} sslPlugin;

SslProtocolFactory::SslProtocolFactory(const SslServerOptions& options, int backlog, bool nodelay) :
    tcpNoDelay(nodelay), listeningPort(listener.listen(options.port, backlog, options.certName, options.clientAuth))
{}

void SslProtocolFactory::established(Poller::shared_ptr poller, const qpid::sys::ssl::SslSocket& s,
                                          ConnectionCodec::Factory* f, bool isClient) {
    qpid::sys::ssl::SslHandler* async = new qpid::sys::ssl::SslHandler(s.getPeerAddress(), f);

    if (tcpNoDelay) {
        s.setTcpNoDelay(tcpNoDelay);
        QPID_LOG(info, "Set TCP_NODELAY on connection to " << s.getPeerAddress());
    }

    if (isClient)
        async->setClient();
    qpid::sys::ssl::SslIO* aio = new qpid::sys::ssl::SslIO(s,
                                 boost::bind(&qpid::sys::ssl::SslHandler::readbuff, async, _1, _2),
                                 boost::bind(&qpid::sys::ssl::SslHandler::eof, async, _1),
                                 boost::bind(&qpid::sys::ssl::SslHandler::disconnect, async, _1),
                                 boost::bind(&qpid::sys::ssl::SslHandler::closedSocket, async, _1, _2),
                                 boost::bind(&qpid::sys::ssl::SslHandler::nobuffs, async, _1),
                                 boost::bind(&qpid::sys::ssl::SslHandler::idle, async, _1));

    async->init(aio, 4);
    aio->start(poller);
}

uint16_t SslProtocolFactory::getPort() const {
    return listeningPort; // Immutable no need for lock.
}

std::string SslProtocolFactory::getHost() const {
    return listener.getSockname();
}

void SslProtocolFactory::accept(Poller::shared_ptr poller,
                                     ConnectionCodec::Factory* fact) {
    acceptor.reset(
        new qpid::sys::ssl::SslAcceptor(listener,
                           boost::bind(&SslProtocolFactory::established, this, poller, _1, fact, false)));
    acceptor->start(poller);
}

void SslProtocolFactory::connect(
    Poller::shared_ptr poller,
    const std::string& host, int16_t port,
    ConnectionCodec::Factory* fact,
    ConnectFailedCallback failed)
{
    // Note that the following logic does not cause a memory leak.
    // The allocated Socket is freed either by the SslConnector
    // upon connection failure or by the SslIoHandle upon connection
    // shutdown.  The allocated SslConnector frees itself when it
    // is no longer needed.

    qpid::sys::ssl::SslSocket* socket = new qpid::sys::ssl::SslSocket();
    new qpid::sys::ssl::SslConnector (*socket, poller, host, port,
                         boost::bind(&SslProtocolFactory::established, this, poller, _1, fact, true),
                         failed);
}

namespace
{
const std::string SSL = "ssl";
}

bool SslProtocolFactory::supports(const std::string& capability)
{
    std::string s = capability;
    transform(s.begin(), s.end(), s.begin(), tolower);
    return s == SSL;
}

}} // namespace qpid::sys
