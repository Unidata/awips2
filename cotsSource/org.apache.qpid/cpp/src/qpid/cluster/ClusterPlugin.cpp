/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "config.h"
#include "qpid/cluster/Connection.h"
#include "qpid/cluster/ConnectionCodec.h"
#include "qpid/cluster/ClusterSettings.h"

#include "qpid/cluster/Cluster.h"
#include "qpid/cluster/ConnectionCodec.h"
#include "qpid/cluster/UpdateClient.h"

#include "qpid/broker/Broker.h"
#include "qpid/Plugin.h"
#include "qpid/Options.h"
#include "qpid/sys/AtomicValue.h"
#include "qpid/log/Statement.h"

#include "qpid/management/ManagementAgent.h"
#include "qpid/management/IdAllocator.h"
#include "qpid/broker/Exchange.h"
#include "qpid/broker/Message.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/SessionState.h"
#include "qpid/client/ConnectionSettings.h"

#include <boost/shared_ptr.hpp>
#include <boost/utility/in_place_factory.hpp>
#include <boost/scoped_ptr.hpp>

namespace qpid {
namespace cluster {

using namespace std;
using broker::Broker;
using management::IdAllocator;
using management::ManagementAgent;


/** Note separating options from settings to work around boost version differences.
 *  Old boost takes a reference to options objects, but new boost makes a copy.
 *  New boost allows a shared_ptr but that's not compatible with old boost.
 */
struct ClusterOptions : public Options {
    ClusterSettings& settings; 

    ClusterOptions(ClusterSettings& v) : Options("Cluster Options"), settings(v) {
        addOptions()
            ("cluster-name", optValue(settings.name, "NAME"), "Name of cluster to join")
            ("cluster-url", optValue(settings.url,"URL"),
             "URL of this broker, advertized to the cluster.\n"
             "Defaults to a URL listing all the local IP addresses\n")
            ("cluster-username", optValue(settings.username, "USER"), "Username for connections between brokers")
            ("cluster-password", optValue(settings.password, "PASS"), "Password for connections between brokers")
            ("cluster-mechanism", optValue(settings.mechanism, "MECH"), "Authentication mechanism for connections between brokers")
#if HAVE_LIBCMAN_H
            ("cluster-cman", optValue(settings.quorum), "Integrate with Cluster Manager (CMAN) cluster.")
#endif
            ("cluster-size", optValue(settings.size, "N"), "Wait for N cluster members before allowing clients to connect.")
            ("cluster-read-max", optValue(settings.readMax,"N"), "Experimental: flow-control limit  reads per connection. 0=no limit.")
            ;
    }
};

struct UpdateClientIdAllocator : management::IdAllocator
{
    qpid::sys::AtomicValue<uint64_t> sequence;

    UpdateClientIdAllocator() : sequence(0x4000000000000000LL) {}

    uint64_t getIdFor(management::Manageable* m)
    {
        if (isUpdateQueue(m) || isUpdateExchange(m) || isUpdateSession(m) || isUpdateBinding(m)) {
            return ++sequence;
        } else {
            return 0;
        }
    }

    bool isUpdateQueue(management::Manageable* manageable)
    {
        qpid::broker::Queue* queue = dynamic_cast<qpid::broker::Queue*>(manageable);
        return queue && queue->getName() == UpdateClient::UPDATE;
    }

    bool isUpdateExchange(management::Manageable* manageable)
    {
        qpid::broker::Exchange* exchange = dynamic_cast<qpid::broker::Exchange*>(manageable);
        return exchange && exchange->getName() == UpdateClient::UPDATE;
    }

    bool isUpdateSession(management::Manageable* manageable)
    {
        broker::SessionState* session = dynamic_cast<broker::SessionState*>(manageable);
        return session && session->getId().getName() == UpdateClient::UPDATE;
    }

    bool isUpdateBinding(management::Manageable* manageable)
    {
        broker::Exchange::Binding* binding = dynamic_cast<broker::Exchange::Binding*>(manageable);
        return binding && binding->queue->getName() == UpdateClient::UPDATE;
    }
};

struct ClusterPlugin : public Plugin {

    ClusterSettings settings;
    ClusterOptions options;
    Cluster* cluster;
    boost::scoped_ptr<ConnectionCodec::Factory> factory;

    ClusterPlugin() : options(settings), cluster(0) {}

    // Cluster needs to be initialized after the store 
    int initOrder() const { return Plugin::DEFAULT_INIT_ORDER+500; }
    
    Options* getOptions() { return &options; }

    void earlyInitialize(Plugin::Target& target) {
        if (settings.name.empty()) return; // Only if --cluster-name option was specified.
        Broker* broker = dynamic_cast<Broker*>(&target);
        if (!broker) return;
        cluster = new Cluster(settings, *broker);
        broker->setConnectionFactory(
            boost::shared_ptr<sys::ConnectionCodec::Factory>(
                new ConnectionCodec::Factory(broker->getConnectionFactory(), *cluster)));
        ManagementAgent* mgmt = broker->getManagementAgent();
        if (mgmt) {
            std::auto_ptr<IdAllocator> allocator(new UpdateClientIdAllocator());
            mgmt->setAllocator(allocator);
        }
    }

    void disallow(ManagementAgent* agent, const string& className, const string& methodName) {
        string message = "Management method " + className + ":" + methodName + " is not allowed on a clustered broker.";
        agent->disallow(className, methodName, message);
    }
    void disallowManagementMethods(ManagementAgent* agent) {
        if (!agent) return;
        disallow(agent, "queue", "purge");
        disallow(agent, "session", "detach");
        disallow(agent, "session", "close");
        disallow(agent, "connection", "close");
    }

    void initialize(Plugin::Target& target) {
        Broker* broker = dynamic_cast<Broker*>(&target);
        if (broker && cluster) {
            disallowManagementMethods(broker->getManagementAgent());
            cluster->initialize();
        }
    }
};

static ClusterPlugin instance; // Static initialization.

}} // namespace qpid::cluster
