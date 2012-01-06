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
#ifndef _QPID_CONSOLE_BROKER_H_
#define _QPID_CONSOLE_BROKER_H_

#include "qpid/console/ConsoleImportExport.h"
#include "qpid/client/Connection.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/Session.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/client/Message.h"
#include "qpid/client/MessageListener.h"
#include "qpid/sys/Thread.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Condition.h"
#include "qpid/Url.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/Uuid.h"
#include <string>
#include <iostream>

namespace qpid {
namespace console {
    class SessionManager;
    class Agent;
    class Object;

    /**
     *
     * \ingroup qpidconsoleapi
     */
    class Broker : public client::MessageListener {
    public:
        QPID_CONSOLE_EXTERN Broker(SessionManager& sm,
                                   client::ConnectionSettings& settings);
        QPID_CONSOLE_EXTERN ~Broker();

        bool isConnected() const { return connected; }
        const std::string& getError() const { return error; }
        const std::string& getSessionId() const { return amqpSessionId; }
        const framing::Uuid& getBrokerId() const { return brokerId; }
        uint32_t getBrokerBank() const { return 1; }
        void addBinding(const std::string& key) {
            connThreadBody.bindExchange("qpid.management", key);
        }
        QPID_CONSOLE_EXTERN std::string getUrl() const;

    private:
        friend class SessionManager;
        friend class Object;
        typedef std::map<uint64_t,Agent*> AgentMap;
        static const int SYNC_TIME = 60;

        SessionManager& sessionManager;
        AgentMap agents;
        bool connected;
        std::string error;
        std::string amqpSessionId;
        client::ConnectionSettings connectionSettings;
        sys::Mutex lock;
        sys::Condition cond;
        framing::Uuid brokerId;
        uint32_t reqsOutstanding;
        bool syncInFlight;
        bool topicBound;
        Object* methodObject;

        friend class ConnectionThread;
        class ConnectionThread : public sys::Runnable {
            bool operational;
            bool shuttingDown;
            Broker&              broker;
            framing::Uuid        sessionId;
            client::Connection   connection;
            client::Session      session;
            client::SubscriptionManager* subscriptions;
            std::stringstream queueName;
            sys::Mutex        connLock;
            void run();
        public:
            ConnectionThread(Broker& _broker) :
                operational(false), shuttingDown(false), broker(_broker), subscriptions(0) {}
            ~ConnectionThread();
            void sendBuffer(qpid::framing::Buffer& buf,
                            uint32_t               length,
                            const std::string&     exchange = "qpid.management",
                            const std::string&     routingKey = "broker");
            void bindExchange(const std::string& exchange, const std::string& key);
            void shutdown();
        };

        ConnectionThread connThreadBody;
        sys::Thread      connThread;

        void encodeHeader(framing::Buffer& buf, uint8_t opcode, uint32_t seq = 0) const;
        bool checkHeader(framing::Buffer& buf, uint8_t *opcode, uint32_t *seq) const;
        void received(client::Message& msg);
        void resetAgents();
        void updateAgent(const Object& object);
        void waitForStable();
        void incOutstanding();
        void decOutstanding();
        void setBrokerId(const framing::Uuid& id) { brokerId = id; }
        void appendAgents(std::vector<Agent*>& agents) const;

        friend QPID_CONSOLE_EXTERN std::ostream& operator<<(std::ostream& o, const Broker& k);
    };

    QPID_CONSOLE_EXTERN std::ostream& operator<<(std::ostream& o, const Broker& k);
}
}

#endif
