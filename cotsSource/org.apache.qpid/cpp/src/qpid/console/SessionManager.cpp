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
#include "qpid/console/SessionManager.h"
#include "qpid/console/Schema.h"
#include "qpid/console/Agent.h"
#include "qpid/console/ConsoleListener.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/Time.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/Uuid.h"
#include "qpid/framing/FieldTable.h"

using namespace qpid::console;
using namespace qpid::sys;
using namespace qpid;
using namespace std;
using qpid::framing::Buffer;
using qpid::framing::FieldTable;

SessionManager::SessionManager(ConsoleListener* _listener, Settings _settings) :
    listener(_listener), settings(_settings)
{
    bindingKeys();
}

SessionManager::~SessionManager()
{
    for (vector<Broker*>::iterator iter = brokers.begin();
         iter != brokers.end(); iter++)
        delete *iter;
}

Broker* SessionManager::addBroker(client::ConnectionSettings& settings)
{
    Broker* broker(new Broker(*this, settings));
    {
        Mutex::ScopedLock l(brokerListLock);
        brokers.push_back(broker);
    }
    return broker;
}

void SessionManager::delBroker(Broker* broker)
{
    Mutex::ScopedLock l(brokerListLock);
    for (vector<Broker*>::iterator iter = brokers.begin();
         iter != brokers.end(); iter++)
        if (*iter == broker) {
            brokers.erase(iter);
            delete broker;
            return;
        }
}

void SessionManager::getPackages(NameVector& packageNames)
{
    allBrokersStable();
    packageNames.clear();
    {
        Mutex::ScopedLock l(lock);
        for (map<string, Package*>::iterator iter = packages.begin();
             iter != packages.end(); iter++)
            packageNames.push_back(iter->first);
    }
}

void SessionManager::getClasses(KeyVector& classKeys, const std::string& packageName)
{
    allBrokersStable();
    classKeys.clear();
    map<string, Package*>::iterator iter = packages.find(packageName);
    if (iter == packages.end())
        return;

    Package& package = *(iter->second);
    for (Package::ClassMap::const_iterator piter = package.classes.begin();
         piter != package.classes.end(); piter++) {
        ClassKey key(piter->second->getClassKey());
        classKeys.push_back(key);
    }
}

SchemaClass& SessionManager::getSchema(const ClassKey& classKey)
{
    allBrokersStable();
    map<string, Package*>::iterator iter = packages.find(classKey.getPackageName());
    if (iter == packages.end())
        throw Exception("Unknown package");

    Package& package = *(iter->second);
    Package::NameHash key(classKey.getClassName(), classKey.getHash());
    Package::ClassMap::iterator cIter = package.classes.find(key);
    if (cIter == package.classes.end())
        throw Exception("Unknown class");

    return *(cIter->second);
}

void SessionManager::bindPackage(const std::string& packageName)
{
    stringstream key;
    key << "console.obj.*.*." << packageName << ".#";
    bindingKeyList.push_back(key.str());
    for (vector<Broker*>::iterator iter = brokers.begin(); iter != brokers.end(); iter++)
        (*iter)->addBinding(key.str());
}

void SessionManager::bindClass(const ClassKey& classKey)
{
    bindClass(classKey.getPackageName(), classKey.getClassName());
}

void SessionManager::bindClass(const std::string& packageName, const std::string& className)
{
    stringstream key;
    key << "console.obj.*.*." << packageName << "." << className << ".#";
    bindingKeyList.push_back(key.str());
    for (vector<Broker*>::iterator iter = brokers.begin();
         iter != brokers.end(); iter++)
        (*iter)->addBinding(key.str());
}

void SessionManager::getAgents(Agent::Vector& agents, Broker* broker)
{
    agents.clear();
    if (broker != 0) {
        broker->appendAgents(agents);
    } else {
        for (vector<Broker*>::iterator iter = brokers.begin(); iter != brokers.end(); iter++) {
            (*iter)->appendAgents(agents);
        }
    }
}

void SessionManager::getObjects(Object::Vector& objects, const std::string& className,
                                Broker* _broker, Agent* _agent)
{
    Agent::Vector agentList;

    if (_agent != 0) {
        agentList.push_back(_agent);
        _agent->getBroker()->waitForStable();
    } else {
        if (_broker != 0) {
            _broker->appendAgents(agentList);
            _broker->waitForStable();
        } else {
            allBrokersStable();
            Mutex::ScopedLock _lock(brokerListLock);
            for (vector<Broker*>::iterator iter = brokers.begin(); iter != brokers.end(); iter++) {
                (*iter)->appendAgents(agentList);
            }
        }
    }

    FieldTable ft;
    uint32_t sequence;
    ft.setString("_class", className);

    getResult.clear();
    syncSequenceList.clear();
    error = string();

    if (agentList.empty()) {
        objects = getResult;
        return;
    }

    for (Agent::Vector::iterator iter = agentList.begin(); iter != agentList.end(); iter++) {
        Agent* agent = *iter;
        char rawbuffer[512];
        Buffer buffer(rawbuffer, 512);
        stringstream routingKey;
        routingKey << "agent." << agent->getBrokerBank() << "." << agent->getAgentBank();
        {
            Mutex::ScopedLock _lock(lock);
            sequence = sequenceManager.reserve("multiget");
            syncSequenceList.insert(sequence);
        }
        agent->getBroker()->encodeHeader(buffer, 'G', sequence);
        ft.encode(buffer);
        uint32_t length = buffer.getPosition();
        buffer.reset();
        agent->getBroker()->connThreadBody.sendBuffer(buffer, length, "qpid.management", routingKey.str());
    }

    {
        Mutex::ScopedLock _lock(lock);
        sys::AbsTime startTime = sys::now();
        while (!syncSequenceList.empty() && error.empty()) {
            cv.wait(lock, AbsTime(now(), settings.getTimeout * TIME_SEC));
            sys::AbsTime currTime = sys::now();
            if (sys::Duration(startTime, currTime) > settings.getTimeout * TIME_SEC)
                break;
        }
    }

    objects = getResult;
}

void SessionManager::bindingKeys()
{
    bindingKeyList.push_back("schema.#");
    if (settings.rcvObjects && settings.rcvEvents && settings.rcvHeartbeats && !settings.userBindings) {
        bindingKeyList.push_back("console.#");
    } else {
        if (settings.rcvObjects && !settings.userBindings)
            bindingKeyList.push_back("console.obj.#");
        else
            bindingKeyList.push_back("console.obj.*.*.org.apache.qpid.broker.agent");
        if (settings.rcvEvents)
            bindingKeyList.push_back("console.event.#");
        if (settings.rcvHeartbeats)
            bindingKeyList.push_back("console.heartbeat");
    }
}

void SessionManager::allBrokersStable()
{
    Mutex::ScopedLock l(brokerListLock);
    for (vector<Broker*>::iterator iter = brokers.begin();
         iter != brokers.end(); iter++)
        if ((*iter)->isConnected())
            (*iter)->waitForStable();
}

void SessionManager::startProtocol(Broker* broker)
{
    char    rawbuffer[512];
    Buffer  buffer(rawbuffer, 512);

    broker->encodeHeader(buffer, 'B');
    uint32_t length = 512 - buffer.available();
    buffer.reset();
    broker->connThreadBody.sendBuffer(buffer, length);
}


void SessionManager::handleBrokerResp(Broker* broker, Buffer& inBuffer, uint32_t)
{
    framing::Uuid brokerId;

    brokerId.decode(inBuffer);
    broker->setBrokerId(brokerId);

    char    rawbuffer[512];
    Buffer  buffer(rawbuffer, 512);

    uint32_t sequence = sequenceManager.reserve("startup");
    broker->encodeHeader(buffer, 'P', sequence);
    uint32_t length = 512 - buffer.available();
    buffer.reset();
    broker->connThreadBody.sendBuffer(buffer, length);

    if (listener != 0) {
        listener->brokerInfo(*broker);
    }
}

void SessionManager::handlePackageInd(Broker* broker, Buffer& inBuffer, uint32_t)
{
    string packageName;
    inBuffer.getShortString(packageName);

    {
        Mutex::ScopedLock l(lock);
        map<string, Package*>::iterator iter = packages.find(packageName);
        if (iter == packages.end()) {
            packages[packageName] = new Package(packageName);
            if (listener != 0)
                listener->newPackage(packageName);
        }
    }

    broker->incOutstanding();
    char    rawbuffer[512];
    Buffer  buffer(rawbuffer, 512);

    uint32_t sequence = sequenceManager.reserve("startup");
    broker->encodeHeader(buffer, 'Q', sequence);
    buffer.putShortString(packageName);
    uint32_t length = 512 - buffer.available();
    buffer.reset();
    broker->connThreadBody.sendBuffer(buffer, length);
}

void SessionManager::handleCommandComplete(Broker* broker, Buffer& inBuffer, uint32_t sequence)
{
    Mutex::ScopedLock l(lock);
    uint32_t resultCode = inBuffer.getLong();
    string resultText;
    inBuffer.getShortString(resultText);
    string context = sequenceManager.release(sequence);
    if (resultCode != 0)
        QPID_LOG(debug, "Received error in completion: " << resultCode << " " << resultText);
    if (context == "startup") {
        broker->decOutstanding();
    } else if (context == "multiget") {
        if (syncSequenceList.count(sequence) == 1) {
            syncSequenceList.erase(sequence);
            if (syncSequenceList.empty()) {
                cv.notify();
            }
        }
    }
    // TODO: Other context cases
}

void SessionManager::handleClassInd(Broker* broker, Buffer& inBuffer, uint32_t)
{
    uint8_t kind;
    string packageName;
    string className;
    uint8_t hash[16];

    kind = inBuffer.getOctet();
    inBuffer.getShortString(packageName);
    inBuffer.getShortString(className);
    inBuffer.getBin128(hash);

    {
        Mutex::ScopedLock l(lock);
        map<string, Package*>::iterator pIter = packages.find(packageName);
        if (pIter == packages.end() || pIter->second->getClass(className, hash))
            return;
    }

    broker->incOutstanding();
    char    rawbuffer[512];
    Buffer  buffer(rawbuffer, 512);

    uint32_t sequence = sequenceManager.reserve("startup");
    broker->encodeHeader(buffer, 'S', sequence);
    buffer.putShortString(packageName);
    buffer.putShortString(className);
    buffer.putBin128(hash);
    uint32_t length = 512 - buffer.available();
    buffer.reset();
    broker->connThreadBody.sendBuffer(buffer, length);
}

void SessionManager::handleMethodResp(Broker* broker, Buffer& buffer, uint32_t sequence)
{
    if (broker->methodObject) {
        broker->methodObject->handleMethodResp(buffer, sequence);
    }
}

void SessionManager::handleHeartbeatInd(Broker* /*broker*/, Buffer& /*inBuffer*/, uint32_t /*sequence*/)
{
}

void SessionManager::handleEventInd(Broker* broker, Buffer& buffer, uint32_t /*sequence*/)
{
    string packageName;
    string className;
    uint8_t hash[16];
    SchemaClass* schemaClass;

    buffer.getShortString(packageName);
    buffer.getShortString(className);
    buffer.getBin128(hash);

    {
        Mutex::ScopedLock l(lock);
        map<string, Package*>::iterator pIter = packages.find(packageName);
        if (pIter == packages.end())
            return;
        schemaClass = pIter->second->getClass(className, hash);
        if (schemaClass == 0)
            return;
    }

    Event event(broker, schemaClass, buffer);    

    if (listener)
        listener->event(event);
}

void SessionManager::handleSchemaResp(Broker* broker, Buffer& inBuffer, uint32_t sequence)
{
    uint8_t kind;
    string packageName;
    string className;
    uint8_t hash[16];

    kind = inBuffer.getOctet();
    inBuffer.getShortString(packageName);
    inBuffer.getShortString(className);
    inBuffer.getBin128(hash);

    {
        Mutex::ScopedLock l(lock);
        map<string, Package*>::iterator pIter = packages.find(packageName);
        if (pIter != packages.end() && !pIter->second->getClass(className, hash)) {
            ClassKey key(packageName, className, hash);
            SchemaClass* schemaClass(new SchemaClass(kind, key, inBuffer));
            pIter->second->addClass(className, hash, schemaClass);
            if (listener != 0) {
                listener->newClass(schemaClass->getClassKey());
            }
        }
    }

    sequenceManager.release(sequence);
    broker->decOutstanding();
}

void SessionManager::handleContentInd(Broker* broker, Buffer& buffer, uint32_t sequence, bool prop, bool stat)
{
    string packageName;
    string className;
    uint8_t hash[16];
    SchemaClass* schemaClass;

    buffer.getShortString(packageName);
    buffer.getShortString(className);
    buffer.getBin128(hash);

    {
        Mutex::ScopedLock l(lock);
        map<string, Package*>::iterator pIter = packages.find(packageName);
        if (pIter == packages.end())
            return;
        schemaClass = pIter->second->getClass(className, hash);
        if (schemaClass == 0)
            return;
    }

    Object object(broker, schemaClass, buffer, prop, stat);

    if (prop && className == "agent" && packageName == "org.apache.qpid.broker")
        broker->updateAgent(object);

    {
        Mutex::ScopedLock l(lock);
        if (syncSequenceList.count(sequence) == 1) {
            if (!object.isDeleted())
                getResult.push_back(object);
            return;
        }
    }

    if (listener) {
        if (prop)
            listener->objectProps(*broker, object);
        if (stat)
            listener->objectStats(*broker, object);
    }
}

void SessionManager::handleBrokerConnect(Broker* broker)
{
    if (listener != 0)
        listener->brokerConnected(*broker);
}

void SessionManager::handleBrokerDisconnect(Broker* broker)
{
    if (listener != 0)
        listener->brokerDisconnected(*broker);
}

