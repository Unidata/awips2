
//
// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
// 
//   http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//

#include "qpid/management/Manageable.h"
#include "qpid/management/ManagementObject.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/PipeHandle.h"
#include "qpid/agent/ManagementAgentImpl.h"
#include <list>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <iostream>
#include <fstream>


using namespace qpid::client;
using namespace qpid::framing;
using namespace qpid::management;
using namespace qpid::sys;
using namespace std;
using std::stringstream;
using std::ofstream;
using std::ifstream;
using std::string;
using std::cout;
using std::endl;

Mutex            ManagementAgent::Singleton::lock;
bool             ManagementAgent::Singleton::disabled = false;
ManagementAgent* ManagementAgent::Singleton::agent    = 0;
int              ManagementAgent::Singleton::refCount = 0;

ManagementAgent::Singleton::Singleton(bool disableManagement)
{
    Mutex::ScopedLock _lock(lock);
    if (disableManagement && !disabled) {
        disabled = true;
        assert(refCount == 0); // can't disable after agent has been allocated
    }
    if (refCount == 0 && !disabled)
        agent = new ManagementAgentImpl();
    refCount++;
}

ManagementAgent::Singleton::~Singleton()
{
    Mutex::ScopedLock _lock(lock);
    refCount--;
    if (refCount == 0 && !disabled) {
        delete agent;
        agent = 0;
    }
}

ManagementAgent* ManagementAgent::Singleton::getInstance()
{
    return agent;
}

const string ManagementAgentImpl::storeMagicNumber("MA02");

ManagementAgentImpl::ManagementAgentImpl() :
    interval(10), extThread(false), pipeHandle(0),
    initialized(false), connected(false), lastFailure("never connected"),
    clientWasAdded(true), requestedBrokerBank(0), requestedAgentBank(0),
    assignedBrokerBank(0), assignedAgentBank(0), bootSequence(0),
    connThreadBody(*this), connThread(connThreadBody),
    pubThreadBody(*this), pubThread(pubThreadBody)
{
}

ManagementAgentImpl::~ManagementAgentImpl()
{
    // shutdown & cleanup all threads
    connThreadBody.close();
    pubThreadBody.close();

    connThread.join();
    pubThread.join();

    // Release the memory associated with stored management objects.
    {
        Mutex::ScopedLock lock(agentLock);

        moveNewObjectsLH();
        for (ManagementObjectMap::iterator iter = managementObjects.begin ();
             iter != managementObjects.end ();
             iter++) {
            ManagementObject* object = iter->second;
            delete object;
        }
        managementObjects.clear();
    }
    if (pipeHandle) {
        delete pipeHandle;
        pipeHandle = 0;
    }
}

void ManagementAgentImpl::init(const string& brokerHost,
                               uint16_t brokerPort,
                               uint16_t intervalSeconds,
                               bool useExternalThread,
                               const string& _storeFile,
                               const string& uid,
                               const string& pwd,
                               const string& mech,
                               const string& proto)
{
    client::ConnectionSettings settings;
    settings.protocol = proto;
    settings.host = brokerHost;
    settings.port = brokerPort;
    settings.username = uid;
    settings.password = pwd;
    settings.mechanism = mech;
    init(settings, intervalSeconds, useExternalThread, _storeFile);
}

void ManagementAgentImpl::init(const qpid::client::ConnectionSettings& settings,
                               uint16_t intervalSeconds,
                               bool useExternalThread,
                               const std::string& _storeFile)
{
    interval     = intervalSeconds;
    extThread    = useExternalThread;
    storeFile    = _storeFile;
    nextObjectId = 1;

    QPID_LOG(info, "QMF Agent Initialized: broker=" << settings.host << ":" << settings.port <<
             " interval=" << intervalSeconds << " storeFile=" << _storeFile);
    connectionSettings = settings;

    // TODO: Abstract the socket calls for portability
    // qpid::sys::PipeHandle to create a pipe
    if (extThread) {
        pipeHandle = new PipeHandle(true);
    }

    retrieveData();
    bootSequence++;
    if ((bootSequence & 0xF000) != 0)
        bootSequence = 1;
    storeData(true);

    initialized = true;
}

void ManagementAgentImpl::registerClass(const string& packageName,
                                        const string& className,
                                        uint8_t*     md5Sum,
                                        qpid::management::ManagementObject::writeSchemaCall_t schemaCall)
{ 
    Mutex::ScopedLock lock(agentLock);
    PackageMap::iterator pIter = findOrAddPackage(packageName);
    addClassLocal(ManagementItem::CLASS_KIND_TABLE, pIter, className, md5Sum, schemaCall);
}

void ManagementAgentImpl::registerEvent(const string& packageName,
                                        const string& eventName,
                                        uint8_t*     md5Sum,
                                        qpid::management::ManagementObject::writeSchemaCall_t schemaCall)
{ 
    Mutex::ScopedLock lock(agentLock);
    PackageMap::iterator pIter = findOrAddPackage(packageName);
    addClassLocal(ManagementItem::CLASS_KIND_EVENT, pIter, eventName, md5Sum, schemaCall);
}

ObjectId ManagementAgentImpl::addObject(ManagementObject* object,
                                        uint64_t          persistId)
{
    Mutex::ScopedLock lock(addLock);
    uint16_t sequence  = persistId ? 0 : bootSequence;
    uint64_t objectNum = persistId ? persistId : nextObjectId++;

    ObjectId objectId(&attachment, 0, sequence, objectNum);

    // TODO: fix object-id handling
    object->setObjectId(objectId);
    newManagementObjects[objectId] = object;
    return objectId;
}

void ManagementAgentImpl::raiseEvent(const ManagementEvent& event, severity_t severity)
{
    Mutex::ScopedLock lock(agentLock);
    Buffer outBuffer(eventBuffer, MA_BUFFER_SIZE);
    uint32_t outLen;
    uint8_t sev = (severity == SEV_DEFAULT) ? event.getSeverity() : (uint8_t) severity;
    stringstream key;

    key << "console.event." << assignedBrokerBank << "." << assignedAgentBank << "." <<
        event.getPackageName() << "." << event.getEventName();

    encodeHeader(outBuffer, 'e');
    outBuffer.putShortString(event.getPackageName());
    outBuffer.putShortString(event.getEventName());
    outBuffer.putBin128(event.getMd5Sum());
    outBuffer.putLongLong(uint64_t(Duration(now())));
    outBuffer.putOctet(sev);
    event.encode(outBuffer);
    outLen = MA_BUFFER_SIZE - outBuffer.available();
    outBuffer.reset();
    connThreadBody.sendBuffer(outBuffer, outLen, "qpid.management", key.str());
}

uint32_t ManagementAgentImpl::pollCallbacks(uint32_t callLimit)
{
    Mutex::ScopedLock lock(agentLock);

    for (uint32_t idx = 0; callLimit == 0 || idx < callLimit; idx++) {
        if (methodQueue.empty())
            break;

        QueuedMethod* item = methodQueue.front();
        methodQueue.pop_front();
        {
            Mutex::ScopedUnlock unlock(agentLock);
            Buffer inBuffer(const_cast<char*>(item->body.c_str()), item->body.size());
            invokeMethodRequest(inBuffer, item->sequence, item->replyTo);
            delete item;
        }
    }
    
    char rbuf[100];
    while (pipeHandle->read(rbuf, 100) > 0) ; // Consume all signaling bytes
    return methodQueue.size();
}

int ManagementAgentImpl::getSignalFd(void)
{
    return pipeHandle->getReadHandle();
}

void ManagementAgentImpl::startProtocol()
{
    char    rawbuffer[512];
    Buffer  buffer(rawbuffer, 512);

    connected = true;
    encodeHeader(buffer, 'A');
    buffer.putShortString("RemoteAgent [C++]");
    systemId.encode (buffer);
    buffer.putLong(requestedBrokerBank);
    buffer.putLong(requestedAgentBank);
    uint32_t length = buffer.getPosition();
    buffer.reset();
    connThreadBody.sendBuffer(buffer, length, "qpid.management", "broker");
    QPID_LOG(trace, "SENT AttachRequest: reqBroker=" << requestedBrokerBank <<
             " reqAgent=" << requestedAgentBank);
}

void ManagementAgentImpl::storeData(bool requested)
{
    if (!storeFile.empty()) {
        ofstream outFile(storeFile.c_str());
        uint32_t brokerBankToWrite = requested ? requestedBrokerBank : assignedBrokerBank;
        uint32_t agentBankToWrite = requested ? requestedAgentBank : assignedAgentBank;

        if (outFile.good()) {
            outFile << storeMagicNumber << " " << brokerBankToWrite << " " <<
                agentBankToWrite << " " << bootSequence << endl;
            outFile.close();
        }
    }
}

void ManagementAgentImpl::retrieveData()
{
    if (!storeFile.empty()) {
        ifstream inFile(storeFile.c_str());
        string   mn;

        if (inFile.good()) {
            inFile >> mn;
            if (mn == storeMagicNumber) {
                inFile >> requestedBrokerBank;
                inFile >> requestedAgentBank;
                inFile >> bootSequence;
            }
            inFile.close();
        }
    }
}

void ManagementAgentImpl::sendCommandComplete(string replyToKey, uint32_t sequence,
                                              uint32_t code, string text)
{
    Buffer   outBuffer(outputBuffer, MA_BUFFER_SIZE);
    uint32_t outLen;

    encodeHeader(outBuffer, 'z', sequence);
    outBuffer.putLong(code);
    outBuffer.putShortString(text);
    outLen = MA_BUFFER_SIZE - outBuffer.available();
    outBuffer.reset();
    connThreadBody.sendBuffer(outBuffer, outLen, "amq.direct", replyToKey);
    QPID_LOG(trace, "SENT CommandComplete: seq=" << sequence << " code=" << code << " text=" << text);
}

void ManagementAgentImpl::handleAttachResponse(Buffer& inBuffer)
{
    Mutex::ScopedLock lock(agentLock);

    assignedBrokerBank = inBuffer.getLong();
    assignedAgentBank  = inBuffer.getLong();

    QPID_LOG(trace, "RCVD AttachResponse: broker=" << assignedBrokerBank << " agent=" << assignedAgentBank);

    if ((assignedBrokerBank != requestedBrokerBank) ||
        (assignedAgentBank  != requestedAgentBank)) {
        if (requestedAgentBank == 0) {
            QPID_LOG(notice, "Initial object-id bank assigned: " << assignedBrokerBank << "." <<
                     assignedAgentBank);
        } else {
            QPID_LOG(warning, "Collision in object-id! New bank assigned: " << assignedBrokerBank <<
                     "." << assignedAgentBank);
        }
        storeData();
        requestedBrokerBank = assignedBrokerBank;
        requestedAgentBank = assignedAgentBank;
    }

    attachment.setBanks(assignedBrokerBank, assignedAgentBank);

    // Bind to qpid.management to receive commands
    connThreadBody.bindToBank(assignedBrokerBank, assignedAgentBank);

    // Send package indications for all local packages
    for (PackageMap::iterator pIter = packages.begin();
         pIter != packages.end();
         pIter++) {
        Buffer   outBuffer(outputBuffer, MA_BUFFER_SIZE);
        uint32_t outLen;

        encodeHeader(outBuffer, 'p');
        encodePackageIndication(outBuffer, pIter);
        outLen = MA_BUFFER_SIZE - outBuffer.available();
        outBuffer.reset();
        connThreadBody.sendBuffer(outBuffer, outLen, "qpid.management", "broker");

        // Send class indications for all local classes
        ClassMap cMap = pIter->second;
        for (ClassMap::iterator cIter = cMap.begin(); cIter != cMap.end(); cIter++) {
            outBuffer.reset();
            encodeHeader(outBuffer, 'q');
            encodeClassIndication(outBuffer, pIter, cIter);
            outLen = MA_BUFFER_SIZE - outBuffer.available();
            outBuffer.reset();
            connThreadBody.sendBuffer(outBuffer, outLen, "qpid.management", "broker");
        }
    }
}

void ManagementAgentImpl::handleSchemaRequest(Buffer& inBuffer, uint32_t sequence)
{
    Mutex::ScopedLock lock(agentLock);
    string packageName;
    SchemaClassKey key;

    inBuffer.getShortString(packageName);
    inBuffer.getShortString(key.name);
    inBuffer.getBin128(key.hash);

    QPID_LOG(trace, "RCVD SchemaRequest: package=" << packageName << " class=" << key.name);

    PackageMap::iterator pIter = packages.find(packageName);
    if (pIter != packages.end()) {
        ClassMap& cMap = pIter->second;
        ClassMap::iterator cIter = cMap.find(key);
        if (cIter != cMap.end()) {
            SchemaClass& schema = cIter->second;
            Buffer   outBuffer(outputBuffer, MA_BUFFER_SIZE);
            uint32_t outLen;

            encodeHeader(outBuffer, 's', sequence);
            schema.writeSchemaCall(outBuffer);
            outLen = MA_BUFFER_SIZE - outBuffer.available();
            outBuffer.reset();
            connThreadBody.sendBuffer(outBuffer, outLen, "qpid.management", "broker");

            QPID_LOG(trace, "SENT SchemaInd: package=" << packageName << " class=" << key.name);
        }
    }
}

void ManagementAgentImpl::handleConsoleAddedIndication()
{
    Mutex::ScopedLock lock(agentLock);
    clientWasAdded = true;

    QPID_LOG(trace, "RCVD ConsoleAddedInd");
}

void ManagementAgentImpl::invokeMethodRequest(Buffer& inBuffer, uint32_t sequence, string replyTo)
{
    string   methodName;
    string   packageName;
    string   className;
    uint8_t  hash[16];
    Buffer   outBuffer(outputBuffer, MA_BUFFER_SIZE);
    uint32_t outLen;

    ObjectId objId(inBuffer);
    inBuffer.getShortString(packageName);
    inBuffer.getShortString(className);
    inBuffer.getBin128(hash);
    inBuffer.getShortString(methodName);

    encodeHeader(outBuffer, 'm', sequence);

    ManagementObjectMap::iterator iter = managementObjects.find(objId);
    if (iter == managementObjects.end() || iter->second->isDeleted()) {
        outBuffer.putLong        (Manageable::STATUS_UNKNOWN_OBJECT);
        outBuffer.putMediumString(Manageable::StatusText(Manageable::STATUS_UNKNOWN_OBJECT));
    } else {
        if ((iter->second->getPackageName() != packageName) ||
            (iter->second->getClassName()   != className)) {
            outBuffer.putLong        (Manageable::STATUS_PARAMETER_INVALID);
            outBuffer.putMediumString(Manageable::StatusText (Manageable::STATUS_PARAMETER_INVALID));
        }
        else
            try {
                outBuffer.record();
                iter->second->doMethod(methodName, inBuffer, outBuffer);
            } catch(exception& e) {
                outBuffer.restore();
                outBuffer.putLong(Manageable::STATUS_EXCEPTION);
                outBuffer.putMediumString(e.what());
            }
    }

    outLen = MA_BUFFER_SIZE - outBuffer.available();
    outBuffer.reset();
    connThreadBody.sendBuffer(outBuffer, outLen, "amq.direct", replyTo);
}

void ManagementAgentImpl::handleGetQuery(Buffer& inBuffer, uint32_t sequence, string replyTo)
{
    FieldTable           ft;
    FieldTable::ValuePtr value;

    moveNewObjectsLH();

    ft.decode(inBuffer);

    QPID_LOG(trace, "RCVD GetQuery: map=" << ft);

    value = ft.get("_class");
    if (value.get() == 0 || !value->convertsTo<string>()) {
        value = ft.get("_objectid");
        if (value.get() == 0 || !value->convertsTo<string>())
            return;

        ObjectId selector(value->get<string>());
        ManagementObjectMap::iterator iter = managementObjects.find(selector);
        if (iter != managementObjects.end()) {
            ManagementObject* object = iter->second;
            Buffer   outBuffer (outputBuffer, MA_BUFFER_SIZE);
            uint32_t outLen;

            if (object->getConfigChanged() || object->getInstChanged())
                object->setUpdateTime();

            encodeHeader(outBuffer, 'g', sequence);
            object->writeProperties(outBuffer);
            object->writeStatistics(outBuffer, true);
            outLen = MA_BUFFER_SIZE - outBuffer.available ();
            outBuffer.reset ();
            connThreadBody.sendBuffer(outBuffer, outLen, "amq.direct", replyTo);

            QPID_LOG(trace, "SENT ObjectInd");
        }
        sendCommandComplete(replyTo, sequence);
        return;
    }

    string className(value->get<string>());

    for (ManagementObjectMap::iterator iter = managementObjects.begin();
         iter != managementObjects.end();
         iter++) {
        ManagementObject* object = iter->second;
        if (object->getClassName() == className) {
            Buffer   outBuffer(outputBuffer, MA_BUFFER_SIZE);
            uint32_t outLen;

            if (object->getConfigChanged() || object->getInstChanged())
                object->setUpdateTime();

            encodeHeader(outBuffer, 'g', sequence);
            object->writeProperties(outBuffer);
            object->writeStatistics(outBuffer, true);
            outLen = MA_BUFFER_SIZE - outBuffer.available();
            outBuffer.reset();
            connThreadBody.sendBuffer(outBuffer, outLen, "amq.direct", replyTo);

            QPID_LOG(trace, "SENT ObjectInd");
        }
    }

    sendCommandComplete(replyTo, sequence);
}

void ManagementAgentImpl::handleMethodRequest(Buffer& inBuffer, uint32_t sequence, string replyTo)
{
    if (extThread) {
        Mutex::ScopedLock lock(agentLock);
        string body;

        inBuffer.getRawData(body, inBuffer.available());
        methodQueue.push_back(new QueuedMethod(sequence, replyTo, body));
        pipeHandle->write("X", 1);
    } else {
        invokeMethodRequest(inBuffer, sequence, replyTo);
    }

    QPID_LOG(trace, "RCVD MethodRequest");
}

void ManagementAgentImpl::received(Message& msg)
{
    string   data = msg.getData();
    Buffer   inBuffer(const_cast<char*>(data.c_str()), data.size());
    uint8_t  opcode;
    uint32_t sequence;
    string   replyToKey;

    framing::MessageProperties p = msg.getMessageProperties();
    if (p.hasReplyTo()) {
        const framing::ReplyTo& rt = p.getReplyTo();
        replyToKey = rt.getRoutingKey();
    }

    if (checkHeader(inBuffer, &opcode, &sequence))
    {
        if      (opcode == 'a') handleAttachResponse(inBuffer);
        else if (opcode == 'S') handleSchemaRequest(inBuffer, sequence);
        else if (opcode == 'x') handleConsoleAddedIndication();
        else if (opcode == 'G') handleGetQuery(inBuffer, sequence, replyToKey);
        else if (opcode == 'M') handleMethodRequest(inBuffer, sequence, replyToKey);
    }
}

void ManagementAgentImpl::encodeHeader(Buffer& buf, uint8_t opcode, uint32_t seq)
{
    buf.putOctet('A');
    buf.putOctet('M');
    buf.putOctet('2');
    buf.putOctet(opcode);
    buf.putLong (seq);
}

bool ManagementAgentImpl::checkHeader(Buffer& buf, uint8_t *opcode, uint32_t *seq)
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

ManagementAgentImpl::PackageMap::iterator ManagementAgentImpl::findOrAddPackage(const string& name)
{
    PackageMap::iterator pIter = packages.find(name);
    if (pIter != packages.end())
        return pIter;

    // No such package found, create a new map entry.
    pair<PackageMap::iterator, bool> result =
        packages.insert(pair<string, ClassMap>(name, ClassMap()));

    if (connected) {
        // Publish a package-indication message
        Buffer   outBuffer(outputBuffer, MA_BUFFER_SIZE);
        uint32_t outLen;

        encodeHeader(outBuffer, 'p');
        encodePackageIndication(outBuffer, result.first);
        outLen = MA_BUFFER_SIZE - outBuffer.available();
        outBuffer.reset();
        connThreadBody.sendBuffer(outBuffer, outLen, "qpid.management", "schema.package");
    }

    return result.first;
}

void ManagementAgentImpl::moveNewObjectsLH()
{
    Mutex::ScopedLock lock(addLock);
    for (ManagementObjectMap::iterator iter = newManagementObjects.begin();
         iter != newManagementObjects.end();
         iter++)
        managementObjects[iter->first] = iter->second;
    newManagementObjects.clear();
}

void ManagementAgentImpl::addClassLocal(uint8_t               classKind,
                                        PackageMap::iterator  pIter,
                                        const string&         className,
                                        uint8_t*              md5Sum,
                                        qpid::management::ManagementObject::writeSchemaCall_t schemaCall)
{
    SchemaClassKey key;
    ClassMap&      cMap = pIter->second;

    key.name = className;
    memcpy(&key.hash, md5Sum, 16);

    ClassMap::iterator cIter = cMap.find(key);
    if (cIter != cMap.end())
        return;

    // No such class found, create a new class with local information.
    cMap.insert(pair<SchemaClassKey, SchemaClass>(key, SchemaClass(schemaCall, classKind)));
}

void ManagementAgentImpl::encodePackageIndication(Buffer&              buf,
                                                  PackageMap::iterator pIter)
{
    buf.putShortString((*pIter).first);

    QPID_LOG(trace, "SENT PackageInd: package=" << (*pIter).first);
}

void ManagementAgentImpl::encodeClassIndication(Buffer&              buf,
                                                PackageMap::iterator pIter,
                                                ClassMap::iterator   cIter)
{
    SchemaClassKey key = (*cIter).first;

    buf.putOctet((*cIter).second.kind);
    buf.putShortString((*pIter).first);
    buf.putShortString(key.name);
    buf.putBin128(key.hash);

    QPID_LOG(trace, "SENT ClassInd: package=" << (*pIter).first << " class=" << key.name);
}

void ManagementAgentImpl::periodicProcessing()
{
#define BUFSIZE   65536
    Mutex::ScopedLock lock(agentLock);
    char                msgChars[BUFSIZE];
    uint32_t            contentSize;
    list<pair<ObjectId, ManagementObject*> > deleteList;

    if (!connected)
        return;

    moveNewObjectsLH();

    //
    //  Clear the been-here flag on all objects in the map.
    //
    for (ManagementObjectMap::iterator iter = managementObjects.begin();
         iter != managementObjects.end();
         iter++) {
        ManagementObject* object = iter->second;
        object->setFlags(0);
        if (clientWasAdded) {
            object->setForcePublish(true);
        }
    }

    clientWasAdded = false;

    //
    //  Process the entire object map.
    //
    for (ManagementObjectMap::iterator baseIter = managementObjects.begin();
         baseIter != managementObjects.end();
         baseIter++) {
        ManagementObject* baseObject = baseIter->second;

        //
        //  Skip until we find a base object requiring a sent message.
        //
        if (baseObject->getFlags() == 1 ||
            (!baseObject->getConfigChanged() &&
             !baseObject->getInstChanged() &&
             !baseObject->getForcePublish() &&
             !baseObject->isDeleted()))
            continue;

        Buffer msgBuffer(msgChars, BUFSIZE);
        for (ManagementObjectMap::iterator iter = baseIter;
             iter != managementObjects.end();
             iter++) {
            ManagementObject* object = iter->second;
            if (baseObject->isSameClass(*object) && object->getFlags() == 0) {
                object->setFlags(1);
                if (object->getConfigChanged() || object->getInstChanged())
                    object->setUpdateTime();

                if (object->getConfigChanged() || object->getForcePublish() || object->isDeleted()) {
                    encodeHeader(msgBuffer, 'c');
                    object->writeProperties(msgBuffer);
                }
        
                if (object->hasInst() && (object->getInstChanged() || object->getForcePublish())) {
                    encodeHeader(msgBuffer, 'i');
                    object->writeStatistics(msgBuffer);
                }

                if (object->isDeleted())
                    deleteList.push_back(pair<ObjectId, ManagementObject*>(iter->first, object));
                object->setForcePublish(false);

                if (msgBuffer.available() < (BUFSIZE / 2))
                    break;
            }
        }

        contentSize = BUFSIZE - msgBuffer.available();
        if (contentSize > 0) {
            msgBuffer.reset();
            stringstream key;
            key << "console.obj." << assignedBrokerBank << "." << assignedAgentBank << "." <<
                baseObject->getPackageName() << "." << baseObject->getClassName();
            connThreadBody.sendBuffer(msgBuffer, contentSize, "qpid.management", key.str());
        }
    }

    // Delete flagged objects
    for (list<pair<ObjectId, ManagementObject*> >::reverse_iterator iter = deleteList.rbegin();
         iter != deleteList.rend();
         iter++) {
        delete iter->second;
        managementObjects.erase(iter->first);
    }

    deleteList.clear();

    {
        Buffer msgBuffer(msgChars, BUFSIZE);
        encodeHeader(msgBuffer, 'h');
        msgBuffer.putLongLong(uint64_t(Duration(now())));
        stringstream key;
        key << "console.heartbeat." << assignedBrokerBank << "." << assignedAgentBank;

        contentSize = BUFSIZE - msgBuffer.available();
        msgBuffer.reset();
        connThreadBody.sendBuffer(msgBuffer, contentSize, "qpid.management", key.str());
    }
}

void ManagementAgentImpl::ConnectionThread::run()
{
    static const int delayMin(1);
    static const int delayMax(128);
    static const int delayFactor(2);
    int delay(delayMin);
    string dest("qmfagent");
    ConnectionThread::shared_ptr tmp;

    sessionId.generate();
    queueName << "qmfagent-" << sessionId;

    while (true) {
        try {
            if (agent.initialized) {
                QPID_LOG(debug, "QMF Agent attempting to connect to the broker...");
                connection.open(agent.connectionSettings);
                session = connection.newSession(queueName.str());
                subscriptions.reset(new client::SubscriptionManager(session));

                session.queueDeclare(arg::queue=queueName.str(), arg::autoDelete=true,
                                     arg::exclusive=true);
                session.exchangeBind(arg::exchange="amq.direct", arg::queue=queueName.str(),
                                     arg::bindingKey=queueName.str());

                subscriptions->subscribe(agent, queueName.str(), dest);
                QPID_LOG(info, "Connection established with broker");
                {
                    Mutex::ScopedLock _lock(connLock);
                    if (shutdown)
                        return;
                    operational = true;
                    agent.startProtocol();
                    try {
                        Mutex::ScopedUnlock _unlock(connLock);
                        subscriptions->run();
                    } catch (exception) {}

                    QPID_LOG(warning, "Connection to the broker has been lost");

                    operational = false;
                    agent.connected = false;
                    tmp = subscriptions;
                    subscriptions.reset();
                }
                tmp.reset();    // frees the subscription outside the lock
                delay = delayMin;
                connection.close();
            }
        } catch (exception &e) {
            if (delay < delayMax)
                delay *= delayFactor;
            QPID_LOG(debug, "Connection failed: exception=" << e.what());
        }

        {
            // sleep for "delay" seconds, but peridically check if the
            // agent is shutting down so we don't hang for up to delayMax 
            // seconds during agent shutdown
             Mutex::ScopedLock _lock(connLock);
             if (shutdown)
                 return;
             sleeping = true;
             int totalSleep = 0;
             do {
                 Mutex::ScopedUnlock _unlock(connLock);
                 ::sleep(delayMin);
                 totalSleep += delayMin;
             } while (totalSleep < delay && !shutdown);
             sleeping = false;
             if (shutdown)
                 return;
        }
    }
}

ManagementAgentImpl::ConnectionThread::~ConnectionThread()
{
}

void ManagementAgentImpl::ConnectionThread::sendBuffer(Buffer&  buf,
                                                       uint32_t length,
                                                       const string& exchange,
                                                       const string& routingKey)
{
    ConnectionThread::shared_ptr s;
    {
        Mutex::ScopedLock _lock(connLock);
        if (!operational)
            return;
        s = subscriptions;
    }

    Message msg;
    string  data;

    buf.getRawData(data, length);
    msg.getDeliveryProperties().setRoutingKey(routingKey);
    msg.getMessageProperties().setReplyTo(ReplyTo("amq.direct", queueName.str()));
    msg.setData(data);
    try {
        session.messageTransfer(arg::content=msg, arg::destination=exchange);
    } catch(exception& e) {
        QPID_LOG(error, "Exception caught in sendBuffer: " << e.what());
        // Bounce the connection
        if (s)
            s->stop();
    }
}

void ManagementAgentImpl::ConnectionThread::bindToBank(uint32_t brokerBank, uint32_t agentBank)
{
    stringstream key;
    key << "agent." << brokerBank << "." << agentBank;
    session.exchangeBind(arg::exchange="qpid.management", arg::queue=queueName.str(),
                         arg::bindingKey=key.str());
}

void ManagementAgentImpl::ConnectionThread::close()
{
    ConnectionThread::shared_ptr s;
    {
        Mutex::ScopedLock _lock(connLock);
        shutdown = true;
        s = subscriptions;
    }
    if (s)
        s->stop();
}

bool ManagementAgentImpl::ConnectionThread::isSleeping() const
{
    Mutex::ScopedLock _lock(connLock);
    return sleeping;
}


void ManagementAgentImpl::PublishThread::run()
{
    uint16_t    totalSleep;

    while (!shutdown) {
        agent.periodicProcessing();
        totalSleep = 0;
        while (totalSleep++ < agent.getInterval() && !shutdown) {
            ::sleep(1);
        }
    }
}
