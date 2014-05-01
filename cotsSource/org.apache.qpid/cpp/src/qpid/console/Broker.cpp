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

#include "qpid/console/Broker.h"
#include "qpid/console/Object.h"
#include "qpid/console/Value.h"
#include "qpid/console/SessionManager.h"
#include "qpid/console/ConsoleListener.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/SystemInfo.h"

using namespace qpid::client;
using namespace qpid::console;
using namespace qpid::framing;
using namespace qpid::sys;
using namespace std;

Broker::Broker(SessionManager& sm, ConnectionSettings& settings) : 
    sessionManager(sm), connected(false), connectionSettings(settings),
    reqsOutstanding(1), syncInFlight(false), topicBound(false), methodObject(0),
    connThreadBody(*this), connThread(connThreadBody)
{
    string osName;
    string nodeName;
    string release;
    string version;
    string machine;

    sys::SystemInfo::getSystemId(osName, nodeName, release, version, machine);
    uint32_t pid = sys::SystemInfo::getParentProcessId();

    stringstream text;

    text << "qmfc-cpp-" << nodeName << "-" << pid;
    amqpSessionId = string(text.str());

    QPID_LOG(debug, "Broker::Broker: constructed, amqpSessionId=" << amqpSessionId);
}

Broker::~Broker()
{
    connThreadBody.shutdown();
    connThread.join();
}

string Broker::getUrl() const
{
    stringstream url;
    url << connectionSettings.host << ":" << connectionSettings.port;
    return url.str();
}

void Broker::encodeHeader(Buffer& buf, uint8_t opcode, uint32_t seq) const
{
    buf.putOctet('A');
    buf.putOctet('M');
    buf.putOctet('2');
    buf.putOctet(opcode);
    buf.putLong (seq);
}

bool Broker::checkHeader(Buffer& buf, uint8_t *opcode, uint32_t *seq) const
{
    if (buf.getSize() < 8)
        return false;

    uint8_t h1 = buf.getOctet();
    uint8_t h2 = buf.getOctet();
    uint8_t h3 = buf.getOctet();

    *opcode = buf.getOctet();
    *seq    = buf.getLong();

    return h1 == 'A' && h2 == 'M' && h3 == '2';
}

void Broker::received(qpid::client::Message& msg)
{
#define QMF_HEADER_SIZE 8
    string   data = msg.getData();
    Buffer   inBuffer(const_cast<char*>(data.c_str()), data.size());
    uint8_t  opcode;
    uint32_t sequence;

    while (inBuffer.available() >= QMF_HEADER_SIZE) {
        if (checkHeader(inBuffer, &opcode, &sequence)) {
            QPID_LOG(trace, "Broker::received: opcode=" << opcode << " seq=" << sequence);

            if      (opcode == 'b') sessionManager.handleBrokerResp(this, inBuffer, sequence);
            else if (opcode == 'p') sessionManager.handlePackageInd(this, inBuffer, sequence);
            else if (opcode == 'z') sessionManager.handleCommandComplete(this, inBuffer, sequence);
            else if (opcode == 'q') sessionManager.handleClassInd(this, inBuffer, sequence);
            else if (opcode == 'm') sessionManager.handleMethodResp(this, inBuffer, sequence);
            else if (opcode == 'h') sessionManager.handleHeartbeatInd(this, inBuffer, sequence);
            else if (opcode == 'e') sessionManager.handleEventInd(this, inBuffer, sequence);
            else if (opcode == 's') sessionManager.handleSchemaResp(this, inBuffer, sequence);
            else if (opcode == 'c') sessionManager.handleContentInd(this, inBuffer, sequence, true, false);
            else if (opcode == 'i') sessionManager.handleContentInd(this, inBuffer, sequence, false, true);
            else if (opcode == 'g') sessionManager.handleContentInd(this, inBuffer, sequence, true, true);
        } else
            return;
    }
}

void Broker::resetAgents()
{
    for (AgentMap::iterator iter = agents.begin(); iter != agents.end(); iter++) {
        if (sessionManager.listener != 0)
            sessionManager.listener->delAgent(*(iter->second));
        delete iter->second;
    }

    agents.clear();
    agents[0x0000000100000000LL] = new Agent(this, 0, "BrokerAgent");
}

void Broker::updateAgent(const Object& object)
{
    uint32_t brokerBank = object.attrUint("brokerBank");
    uint32_t agentBank = object.attrUint("agentBank");
    uint64_t agentKey = ((uint64_t) brokerBank << 32) | (uint64_t) agentBank;
    AgentMap::iterator iter = agents.find(agentKey);

    if (object.isDeleted()) {
        if (iter != agents.end()) {
            if (sessionManager.listener != 0)
                sessionManager.listener->delAgent(*(iter->second));
            delete iter->second;
            agents.erase(iter);
        }
    } else {
        if (iter == agents.end()) {
            Agent* agent = new Agent(this, agentBank, object.attrString("label"));
            agents[agentKey] = agent;
            if (sessionManager.listener != 0)
                sessionManager.listener->newAgent(*agent);
        }
    }
}

void Broker::ConnectionThread::run()
{
    static const int delayMin(1);
    static const int delayMax(128);
    static const int delayFactor(2);
    int delay(delayMin);
    string dest("qmfc");

    sessionId.generate();
    queueName << "qmfc-" << sessionId;

    while (true) {
        try {
            broker.topicBound = false;
            broker.reqsOutstanding = 1;
            connection.open(broker.connectionSettings);
            session = connection.newSession(queueName.str());
            subscriptions = new client::SubscriptionManager(session);

            session.queueDeclare(arg::queue=queueName.str(), arg::autoDelete=true,
                                 arg::exclusive=true);
            session.exchangeBind(arg::exchange="amq.direct", arg::queue=queueName.str(),
                                 arg::bindingKey=queueName.str());

            subscriptions->setAcceptMode(ACCEPT_MODE_NONE);
            subscriptions->setAcquireMode(ACQUIRE_MODE_PRE_ACQUIRED);
            subscriptions->subscribe(broker, queueName.str(), dest);
            subscriptions->setFlowControl(dest, FlowControl::unlimited());
            {
                Mutex::ScopedLock _lock(connLock);
                if (shuttingDown)
                    return;
                operational = true;
                broker.resetAgents();
                broker.connected = true;
                broker.sessionManager.handleBrokerConnect(&broker);
                broker.sessionManager.startProtocol(&broker);
                try {
                    Mutex::ScopedUnlock _unlock(connLock);
                    subscriptions->run();
                } catch (std::exception) {}
                
                operational = false;
                broker.connected = false;
                broker.sessionManager.handleBrokerDisconnect(&broker);
            }
            delay = delayMin;
            connection.close();
            delete subscriptions;
            subscriptions = 0;
        } catch (std::exception &e) {
            QPID_LOG(debug, "  outer exception: " << e.what());
            if (delay < delayMax)
                delay *= delayFactor;
        }

            {
                Mutex::ScopedLock _lock(connLock);
                if (shuttingDown)
                    return;
                {
                    Mutex::ScopedUnlock _unlock(connLock);
                    ::sleep(delay);
                }
                if (shuttingDown)
                    return;
            }
    }
}

Broker::ConnectionThread::~ConnectionThread()
{
    if (subscriptions != 0) {
        delete subscriptions;
    }
}

void Broker::ConnectionThread::sendBuffer(Buffer& buf, uint32_t length,
                                          const string& exchange, const string& routingKey)
{
    {
        Mutex::ScopedLock _lock(connLock);
        if (!operational)
            return;
    }

    client::Message msg;
    string  data;

    buf.getRawData(data, length);
    msg.getDeliveryProperties().setRoutingKey(routingKey);
    msg.getMessageProperties().setReplyTo(ReplyTo("amq.direct", queueName.str()));
    msg.setData(data);
    try {
        session.messageTransfer(arg::content=msg, arg::destination=exchange);
    } catch(std::exception&) {}
}

void Broker::ConnectionThread::bindExchange(const std::string& exchange, const std::string& key)
{
    {
        Mutex::ScopedLock _lock(connLock);
        if (!operational)
            return;
    }

    QPID_LOG(debug, "Broker::ConnectionThread::bindExchange: exchange=" << exchange << " key=" << key);
    session.exchangeBind(arg::exchange=exchange, arg::queue=queueName.str(),
                          arg::bindingKey=key);
}

void Broker::ConnectionThread::shutdown()
{
    {
        Mutex::ScopedLock _lock(connLock);
        shuttingDown = true;
    }
    if (subscriptions)
        subscriptions->stop();
}

void Broker::waitForStable()
{
    Mutex::ScopedLock l(lock);
    if (reqsOutstanding == 0)
        return;
    syncInFlight = true;
    while (reqsOutstanding != 0) {
        bool result = cond.wait(lock, AbsTime(now(), TIME_SEC * sessionManager.settings.getTimeout));
        if (!result)
            throw(Exception("Timed out waiting for broker to synchronize"));
    }
}

void Broker::incOutstanding()
{
    Mutex::ScopedLock l(lock);
    reqsOutstanding++;
}

void Broker::decOutstanding()
{
    Mutex::ScopedLock l(lock);
    reqsOutstanding--;
    if (reqsOutstanding == 0) {
        if (!topicBound) {
            topicBound = true;
            for (vector<string>::const_iterator iter = sessionManager.bindingKeyList.begin();
                 iter != sessionManager.bindingKeyList.end(); iter++)
                connThreadBody.bindExchange("qpid.management", *iter);
        }
        if (syncInFlight) {
            syncInFlight = false;
            cond.notify();
        }
    }
}

void Broker::appendAgents(Agent::Vector& agentlist) const
{
    for (AgentMap::const_iterator iter = agents.begin(); iter != agents.end(); iter++) {
        agentlist.push_back(iter->second);
    }
}

ostream& qpid::console::operator<<(ostream& o, const Broker& k)
{
    o << "Broker: " << k.connectionSettings.host << ":" << k.connectionSettings.port;
    return o;
}
