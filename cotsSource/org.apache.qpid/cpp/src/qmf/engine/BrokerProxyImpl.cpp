/*
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
 */

#include "qmf/engine/BrokerProxyImpl.h"
#include "qmf/engine/ConsoleImpl.h"
#include "qmf/engine/Protocol.h"
#include "qpid/Address.h"
#include "qpid/sys/SystemInfo.h"
#include <qpid/log/Statement.h>
#include <qpid/StringUtils.h>
#include <string.h>
#include <iostream>
#include <fstream>

using namespace std;
using namespace qmf::engine;
using namespace qpid::framing;
using namespace qpid::sys;

namespace {
    const char* QMF_EXCHANGE     = "qpid.management";
    const char* DIR_EXCHANGE     = "amq.direct";
    const char* BROKER_KEY       = "broker";
    const char* BROKER_PACKAGE   = "org.apache.qpid.broker";
    const char* AGENT_CLASS      = "agent";
    const char* BROKER_AGENT_KEY = "agent.1.0";
}

const Object* QueryResponseImpl::getObject(uint32_t idx) const
{
    vector<ObjectPtr>::const_iterator iter = results.begin();

    while (idx > 0) {
        if (iter == results.end())
            return 0;
        iter++;
        idx--;
    }

    return iter->get();
}

#define STRING_REF(s) {if (!s.empty()) item.s = const_cast<char*>(s.c_str());}

BrokerEvent BrokerEventImpl::copy()
{
    BrokerEvent item;

    ::memset(&item, 0, sizeof(BrokerEvent));
    item.kind = kind;

    STRING_REF(name);
    STRING_REF(exchange);
    STRING_REF(bindingKey);
    item.context = context;
    item.queryResponse = queryResponse.get();
    item.methodResponse = methodResponse.get();

    return item;
}

BrokerProxyImpl::BrokerProxyImpl(BrokerProxy& pub, Console& _console) : publicObject(pub), console(_console)
{
    stringstream qn;
    qpid::TcpAddress addr;

    SystemInfo::getLocalHostname(addr);
    qn << "qmfc-" << SystemInfo::getProcessName() << "-" << addr << "-" << SystemInfo::getProcessId();
    queueName = qn.str();

    seqMgr.setUnsolicitedContext(SequenceContext::Ptr(new StaticContext(*this)));
}

void BrokerProxyImpl::sessionOpened(SessionHandle& /*sh*/)
{
    Mutex::ScopedLock _lock(lock);
    agentList.clear();
    eventQueue.clear();
    xmtQueue.clear();
    eventQueue.push_back(eventDeclareQueue(queueName));
    eventQueue.push_back(eventBind(DIR_EXCHANGE, queueName, queueName));
    eventQueue.push_back(eventSetupComplete());

    // TODO: Store session handle
}

void BrokerProxyImpl::sessionClosed()
{
    Mutex::ScopedLock _lock(lock);
    agentList.clear();
    eventQueue.clear();
    xmtQueue.clear();
}

void BrokerProxyImpl::startProtocol()
{
    AgentProxyPtr agent(AgentProxyImpl::factory(console, publicObject, 0, "Agent embedded in broker"));
    {
        Mutex::ScopedLock _lock(lock);
        char rawbuffer[512];
        Buffer buffer(rawbuffer, 512);

        agentList[0] = agent;

        requestsOutstanding = 1;
        topicBound = false;
        uint32_t sequence(seqMgr.reserve());
        Protocol::encodeHeader(buffer, Protocol::OP_BROKER_REQUEST, sequence);
        sendBufferLH(buffer, QMF_EXCHANGE, BROKER_KEY);
        QPID_LOG(trace, "SENT BrokerRequest seq=" << sequence);
    }

    console.impl->eventAgentAdded(agent);
}

void BrokerProxyImpl::sendBufferLH(Buffer& buf, const string& destination, const string& routingKey)
{
    uint32_t length = buf.getPosition();
    MessageImpl::Ptr message(new MessageImpl);

    buf.reset();
    buf.getRawData(message->body, length);
    message->destination   = destination;
    message->routingKey    = routingKey;
    message->replyExchange = DIR_EXCHANGE;
    message->replyKey      = queueName;

    xmtQueue.push_back(message);
}

void BrokerProxyImpl::handleRcvMessage(Message& message)
{
    Buffer inBuffer(message.body, message.length);
    uint8_t opcode;
    uint32_t sequence;

    while (Protocol::checkHeader(inBuffer, &opcode, &sequence))
        seqMgr.dispatch(opcode, sequence, message.routingKey ? string(message.routingKey) : string(), inBuffer);
}

bool BrokerProxyImpl::getXmtMessage(Message& item) const
{
    Mutex::ScopedLock _lock(lock);
    if (xmtQueue.empty())
        return false;
    item =  xmtQueue.front()->copy();
    return true;
}

void BrokerProxyImpl::popXmt()
{
    Mutex::ScopedLock _lock(lock);
    if (!xmtQueue.empty())
        xmtQueue.pop_front();
}

bool BrokerProxyImpl::getEvent(BrokerEvent& event) const
{
    Mutex::ScopedLock _lock(lock);
    if (eventQueue.empty())
        return false;
    event = eventQueue.front()->copy();
    return true;
}

void BrokerProxyImpl::popEvent()
{
    Mutex::ScopedLock _lock(lock);
    if (!eventQueue.empty())
        eventQueue.pop_front();
}

uint32_t BrokerProxyImpl::agentCount() const
{
    Mutex::ScopedLock _lock(lock);
    return agentList.size();
}

const AgentProxy* BrokerProxyImpl::getAgent(uint32_t idx) const
{
    Mutex::ScopedLock _lock(lock);
    for (map<uint32_t, AgentProxyPtr>::const_iterator iter = agentList.begin();
         iter != agentList.end(); iter++)
        if (idx-- == 0)
            return iter->second.get();
    return 0;
}

void BrokerProxyImpl::sendQuery(const Query& query, void* context, const AgentProxy* agent)
{
    SequenceContext::Ptr queryContext(new QueryContext(*this, context));
    Mutex::ScopedLock _lock(lock);
    bool sent = false;
    if (agent != 0) {
        if (sendGetRequestLH(queryContext, query, agent))
            sent = true;
    } else {
        // TODO (optimization) only send queries to agents that have the requested class+package
        for (map<uint32_t, AgentProxyPtr>::const_iterator iter = agentList.begin();
             iter != agentList.end(); iter++) {
            if (sendGetRequestLH(queryContext, query, iter->second.get()))
                sent = true;
        }
    }

    if (!sent) {
        queryContext->reserve();
        queryContext->release();
    }
}

bool BrokerProxyImpl::sendGetRequestLH(SequenceContext::Ptr queryContext, const Query& query, const AgentProxy* agent)
{
    if (query.impl->singleAgent()) {
        if (query.impl->agentBank() != agent->getAgentBank())
            return false;
    }
    stringstream key;
    Buffer outBuffer(outputBuffer, MA_BUFFER_SIZE);
    uint32_t sequence(seqMgr.reserve(queryContext));
    agent->impl->addSequence(sequence);

    Protocol::encodeHeader(outBuffer, Protocol::OP_GET_QUERY, sequence);
    query.impl->encode(outBuffer);
    key << "agent.1." << agent->impl->agentBank;
    sendBufferLH(outBuffer, QMF_EXCHANGE, key.str());
    QPID_LOG(trace, "SENT GetQuery seq=" << sequence << " key=" << key.str());
    return true;
}

string BrokerProxyImpl::encodeMethodArguments(const SchemaMethod* schema, const Value* argmap, Buffer& buffer)
{
    int argCount = schema->getArgumentCount();

    if (argmap == 0 || !argmap->isMap())
        return string("Arguments must be in a map value");

    for (int aIdx = 0; aIdx < argCount; aIdx++) {
        const SchemaArgument* arg(schema->getArgument(aIdx));
        if (arg->getDirection() == DIR_IN || arg->getDirection() == DIR_IN_OUT) {
            if (argmap->keyInMap(arg->getName())) {
                const Value* argVal(argmap->byKey(arg->getName()));
                if (argVal->getType() != arg->getType())
                    return string("Argument is the wrong type: ") + arg->getName();
                argVal->impl->encode(buffer);
            } else {
                Value defaultValue(arg->getType());
                defaultValue.impl->encode(buffer);
            }
        }
    }

    return string();
}

void BrokerProxyImpl::sendMethodRequest(ObjectId* oid, const SchemaObjectClass* cls,
                                        const string& methodName, const Value* args, void* userContext)
{
    int methodCount = cls->getMethodCount();
    int idx;
    for (idx = 0; idx < methodCount; idx++) {
        const SchemaMethod* method = cls->getMethod(idx);
        if (string(method->getName()) == methodName) {
            Mutex::ScopedLock _lock(lock);
            SequenceContext::Ptr methodContext(new MethodContext(*this, userContext, method));
            stringstream key;
            Buffer outBuffer(outputBuffer, MA_BUFFER_SIZE);
            uint32_t sequence(seqMgr.reserve(methodContext));

            Protocol::encodeHeader(outBuffer, Protocol::OP_METHOD_REQUEST, sequence);
            oid->impl->encode(outBuffer);
            cls->getClassKey()->impl->encode(outBuffer);
            outBuffer.putShortString(methodName);

            string argErrorString = encodeMethodArguments(method, args, outBuffer);
            if (argErrorString.empty()) {
                key << "agent.1." << oid->impl->getAgentBank();
                sendBufferLH(outBuffer, QMF_EXCHANGE, key.str());
                QPID_LOG(trace, "SENT MethodRequest seq=" << sequence << " method=" << methodName << " key=" << key.str());
            } else {
                MethodResponsePtr argError(MethodResponseImpl::factory(1, argErrorString));
                eventQueue.push_back(eventMethodResponse(userContext, argError));
            }
            return;
        }
    }

    MethodResponsePtr error(MethodResponseImpl::factory(1, string("Unknown method: ") + methodName));
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(eventMethodResponse(userContext, error));
}

void BrokerProxyImpl::addBinding(const string& exchange, const string& key)
{
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(eventBind(exchange, queueName, key));
}

BrokerEventImpl::Ptr BrokerProxyImpl::eventDeclareQueue(const string& queueName)
{
    BrokerEventImpl::Ptr event(new BrokerEventImpl(BrokerEvent::DECLARE_QUEUE));
    event->name = queueName;
    return event;
}

BrokerEventImpl::Ptr BrokerProxyImpl::eventBind(const string& exchange, const string& queue, const string& key)
{
    BrokerEventImpl::Ptr event(new BrokerEventImpl(BrokerEvent::BIND));
    event->name       = queue;
    event->exchange   = exchange;
    event->bindingKey = key;

    return event;
}

BrokerEventImpl::Ptr BrokerProxyImpl::eventSetupComplete()
{
    BrokerEventImpl::Ptr event(new BrokerEventImpl(BrokerEvent::SETUP_COMPLETE));
    return event;
}

BrokerEventImpl::Ptr BrokerProxyImpl::eventStable()
{
    QPID_LOG(trace, "Console Link to Broker Stable");
    BrokerEventImpl::Ptr event(new BrokerEventImpl(BrokerEvent::STABLE));
    return event;
}

BrokerEventImpl::Ptr BrokerProxyImpl::eventQueryComplete(void* context, QueryResponsePtr response)
{
    BrokerEventImpl::Ptr event(new BrokerEventImpl(BrokerEvent::QUERY_COMPLETE));
    event->context = context;
    event->queryResponse = response;
    return event;
}

BrokerEventImpl::Ptr BrokerProxyImpl::eventMethodResponse(void* context, MethodResponsePtr response)
{
    BrokerEventImpl::Ptr event(new BrokerEventImpl(BrokerEvent::METHOD_RESPONSE));
    event->context = context;
    event->methodResponse = response;
    return event;
}

void BrokerProxyImpl::handleBrokerResponse(Buffer& inBuffer, uint32_t seq)
{
    brokerId.decode(inBuffer);
    QPID_LOG(trace, "RCVD BrokerResponse seq=" << seq << " brokerId=" << brokerId);
    Mutex::ScopedLock _lock(lock);
    Buffer outBuffer(outputBuffer, MA_BUFFER_SIZE);
    uint32_t sequence(seqMgr.reserve());
    incOutstandingLH();
    Protocol::encodeHeader(outBuffer, Protocol::OP_PACKAGE_REQUEST, sequence);
    sendBufferLH(outBuffer, QMF_EXCHANGE, BROKER_KEY);
    QPID_LOG(trace, "SENT PackageRequest seq=" << sequence);
}

void BrokerProxyImpl::handlePackageIndication(Buffer& inBuffer, uint32_t seq)
{
    string package;

    inBuffer.getShortString(package);
    QPID_LOG(trace, "RCVD PackageIndication seq=" << seq << " package=" << package);
    console.impl->learnPackage(package);

    Mutex::ScopedLock _lock(lock);
    Buffer outBuffer(outputBuffer, MA_BUFFER_SIZE);
    uint32_t sequence(seqMgr.reserve());
    incOutstandingLH();
    Protocol::encodeHeader(outBuffer, Protocol::OP_CLASS_QUERY, sequence);
    outBuffer.putShortString(package);
    sendBufferLH(outBuffer, QMF_EXCHANGE, BROKER_KEY);
    QPID_LOG(trace, "SENT ClassQuery seq=" << sequence << " package=" << package);
}

void BrokerProxyImpl::handleCommandComplete(Buffer& inBuffer, uint32_t seq)
{
    string text;
    uint32_t code = inBuffer.getLong();
    inBuffer.getShortString(text);
    QPID_LOG(trace, "RCVD CommandComplete seq=" << seq << " code=" << code << " text=" << text);
}

void BrokerProxyImpl::handleClassIndication(Buffer& inBuffer, uint32_t seq)
{
    uint8_t kind = inBuffer.getOctet();
    auto_ptr<SchemaClassKey> classKey(SchemaClassKeyImpl::factory(inBuffer));

    QPID_LOG(trace, "RCVD ClassIndication seq=" << seq << " kind=" << (int) kind << " key=" << classKey->impl->str());

    if (!console.impl->haveClass(classKey.get())) {
        Mutex::ScopedLock _lock(lock);
        incOutstandingLH();
        Buffer outBuffer(outputBuffer, MA_BUFFER_SIZE);
        uint32_t sequence(seqMgr.reserve());
        Protocol::encodeHeader(outBuffer, Protocol::OP_SCHEMA_REQUEST, sequence);
        classKey->impl->encode(outBuffer);
        sendBufferLH(outBuffer, QMF_EXCHANGE, BROKER_KEY);
        QPID_LOG(trace, "SENT SchemaRequest seq=" << sequence <<" key=" << classKey->impl->str());
    }
}

MethodResponsePtr BrokerProxyImpl::handleMethodResponse(Buffer& inBuffer, uint32_t seq, const SchemaMethod* schema)
{
    MethodResponsePtr response(MethodResponseImpl::factory(inBuffer, schema));

    QPID_LOG(trace, "RCVD MethodResponse seq=" << seq << " status=" << response->getStatus() << " text=" <<
             response->getException()->asString());

    return response;
}

void BrokerProxyImpl::handleHeartbeatIndication(Buffer& inBuffer, uint32_t seq, const string& routingKey)
{
    vector<string> tokens = qpid::split(routingKey, ".");
    uint32_t agentBank;
    uint64_t timestamp;

    if (routingKey.empty() || tokens.size() != 4)
        agentBank = 0;
    else
        agentBank = ::atoi(tokens[3].c_str());

    timestamp = inBuffer.getLongLong();
    map<uint32_t, AgentProxyPtr>::const_iterator iter = agentList.find(agentBank);
    if (iter != agentList.end()) {
        console.impl->eventAgentHeartbeat(iter->second, timestamp);
    }
    QPID_LOG(trace, "RCVD HeartbeatIndication seq=" << seq << " agentBank=" << agentBank);
}

void BrokerProxyImpl::handleEventIndication(Buffer& /*inBuffer*/, uint32_t /*seq*/)
{
    // TODO
}

void BrokerProxyImpl::handleSchemaResponse(Buffer& inBuffer, uint32_t seq)
{
    SchemaObjectClass* oClassPtr;
    SchemaEventClass* eClassPtr;
    uint8_t kind = inBuffer.getOctet();
    const SchemaClassKey* key;
    if (kind == CLASS_OBJECT) {
        oClassPtr = SchemaObjectClassImpl::factory(inBuffer);
        console.impl->learnClass(oClassPtr);
        key = oClassPtr->getClassKey();
        QPID_LOG(trace, "RCVD SchemaResponse seq=" << seq << " kind=object key=" << key->impl->str());

        //
        // If we have just learned about the org.apache.qpid.broker:agent class, send a get
        // request for the current list of agents so we can have it on-hand before we declare
        // this session "stable".
        //
        if (key->impl->getClassName() == AGENT_CLASS && key->impl->getPackageName() == BROKER_PACKAGE) {
            Mutex::ScopedLock _lock(lock);
            incOutstandingLH();
            Buffer outBuffer(outputBuffer, MA_BUFFER_SIZE);
            uint32_t sequence(seqMgr.reserve());
            Protocol::encodeHeader(outBuffer, Protocol::OP_GET_QUERY, sequence);
            FieldTable ft;
            ft.setString("_class", AGENT_CLASS);
            ft.setString("_package", BROKER_PACKAGE);
            ft.encode(outBuffer);
            sendBufferLH(outBuffer, QMF_EXCHANGE, BROKER_AGENT_KEY);
            QPID_LOG(trace, "SENT GetQuery seq=" << sequence << " key=" << BROKER_AGENT_KEY);
        }
    } else if (kind == CLASS_EVENT) {
        eClassPtr = SchemaEventClassImpl::factory(inBuffer);
        console.impl->learnClass(eClassPtr);
        key = eClassPtr->getClassKey();
        QPID_LOG(trace, "RCVD SchemaResponse seq=" << seq << " kind=event key=" << key->impl->str());
    }
    else {
        QPID_LOG(error, "BrokerProxyImpl::handleSchemaResponse received unknown class kind: " << (int) kind);
    }
}

ObjectPtr BrokerProxyImpl::handleObjectIndication(Buffer& inBuffer, uint32_t seq, bool prop, bool stat)
{
    auto_ptr<SchemaClassKey> classKey(SchemaClassKeyImpl::factory(inBuffer));
    QPID_LOG(trace, "RCVD ObjectIndication seq=" << seq << " key=" << classKey->impl->str());

    SchemaObjectClass* schema = console.impl->getSchema(classKey.get());
    if (schema == 0) {
        QPID_LOG(trace, "No Schema Found for ObjectIndication. seq=" << seq << " key=" << classKey->impl->str());
        return ObjectPtr();
    }

    ObjectPtr optr(ObjectImpl::factory(schema, this, inBuffer, prop, stat, true));
    if (prop && classKey->impl->getPackageName() == BROKER_PACKAGE && classKey->impl->getClassName() == AGENT_CLASS) {
        //
        // We've intercepted information about a remote agent...  update the agent list accordingly
        //
        updateAgentList(optr);
    }
    return optr;
}

void BrokerProxyImpl::updateAgentList(ObjectPtr obj)
{
    Value* value = obj->getValue("agentBank");
    Mutex::ScopedLock _lock(lock);
    if (value != 0 && value->isUint()) {
        uint32_t agentBank = value->asUint();
        if (obj->isDeleted()) {
            map<uint32_t, AgentProxyPtr>::iterator iter = agentList.find(agentBank);
            if (iter != agentList.end()) {
                AgentProxyPtr agent(iter->second);
                console.impl->eventAgentDeleted(agent);
                agentList.erase(agentBank);
                QPID_LOG(trace, "Agent at bank " << agentBank << " removed from agent list");

                //
                //  Release all sequence numbers for requests in-flight to this agent.
                //  Since the agent is no longer connected, these requests would not
                //  otherwise complete.
                //
                agent->impl->releaseInFlight(seqMgr);
            }
        } else {
            Value* str = obj->getValue("label");
            string label;
            if (str != 0 && str->isString())
                label = str->asString();
            map<uint32_t, AgentProxyPtr>::const_iterator iter = agentList.find(agentBank);
            if (iter == agentList.end()) {
                AgentProxyPtr agent(AgentProxyImpl::factory(console, publicObject, agentBank, label));
                agentList[agentBank] = agent;
                console.impl->eventAgentAdded(agent);
                QPID_LOG(trace, "Agent '" << label << "' found at bank " << agentBank);
            }
        }
    }
}

void BrokerProxyImpl::incOutstandingLH()
{
    requestsOutstanding++;
}

void BrokerProxyImpl::decOutstanding()
{
    Mutex::ScopedLock _lock(lock);
    requestsOutstanding--;
    if (requestsOutstanding == 0 && !topicBound) {
        topicBound = true;
        for (vector<pair<string, string> >::const_iterator iter = console.impl->bindingList.begin();
             iter != console.impl->bindingList.end(); iter++) {
            string exchange(iter->first.empty() ? QMF_EXCHANGE : iter->first);
            string key(iter->second);
            eventQueue.push_back(eventBind(exchange, queueName, key));
        }
        eventQueue.push_back(eventStable());
    }
}

MethodResponseImpl::MethodResponseImpl(const MethodResponseImpl& from) :
    status(from.status), schema(from.schema)
{
    if (from.exception.get())
        exception.reset(new Value(*(from.exception)));
    if (from.arguments.get())
        arguments.reset(new Value(*(from.arguments)));
}

MethodResponseImpl::MethodResponseImpl(Buffer& buf, const SchemaMethod* s) : schema(s)
{
    string text;

    status = buf.getLong();
    buf.getMediumString(text);
    exception.reset(new Value(TYPE_LSTR));
    exception->setString(text.c_str());

    if (status != 0)
        return;

    arguments.reset(new Value(TYPE_MAP));
    int argCount(schema->getArgumentCount());
    for (int idx = 0; idx < argCount; idx++) {
        const SchemaArgument* arg = schema->getArgument(idx);
        if (arg->getDirection() == DIR_OUT || arg->getDirection() == DIR_IN_OUT) {
            Value* value(ValueImpl::factory(arg->getType(), buf));
            arguments->insert(arg->getName(), value);
        }
    }
}

MethodResponseImpl::MethodResponseImpl(uint32_t s, const string& text) : schema(0)
{
    status = s;
    exception.reset(new Value(TYPE_LSTR));
    exception->setString(text.c_str());
}

MethodResponse* MethodResponseImpl::factory(Buffer& buf, const SchemaMethod* schema)
{
    MethodResponseImpl* impl(new MethodResponseImpl(buf, schema));
    return new MethodResponse(impl);
}

MethodResponse* MethodResponseImpl::factory(uint32_t status, const std::string& text)
{
    MethodResponseImpl* impl(new MethodResponseImpl(status, text));
    return new MethodResponse(impl);
}

bool StaticContext::handleMessage(uint8_t opcode, uint32_t sequence, const string& routingKey, Buffer& buffer)
{
    ObjectPtr object;
    bool completeContext = false;

    if      (opcode == Protocol::OP_BROKER_RESPONSE) {
        broker.handleBrokerResponse(buffer, sequence);
        completeContext = true;
    }
    else if (opcode == Protocol::OP_COMMAND_COMPLETE) {
        broker.handleCommandComplete(buffer, sequence);
        completeContext = true;
    }
    else if (opcode == Protocol::OP_SCHEMA_RESPONSE) {
        broker.handleSchemaResponse(buffer, sequence);
        completeContext = true;
    }
    else if (opcode == Protocol::OP_PACKAGE_INDICATION)
        broker.handlePackageIndication(buffer, sequence);
    else if (opcode == Protocol::OP_CLASS_INDICATION)
        broker.handleClassIndication(buffer, sequence);
    else if (opcode == Protocol::OP_HEARTBEAT_INDICATION)
        broker.handleHeartbeatIndication(buffer, sequence, routingKey);
    else if (opcode == Protocol::OP_EVENT_INDICATION)
        broker.handleEventIndication(buffer, sequence);
    else if (opcode == Protocol::OP_PROPERTY_INDICATION) {
        object = broker.handleObjectIndication(buffer, sequence, true,  false);
        broker.console.impl->eventObjectUpdate(object, true, false);
    }
    else if (opcode == Protocol::OP_STATISTIC_INDICATION) {
        object = broker.handleObjectIndication(buffer, sequence, false, true);
        broker.console.impl->eventObjectUpdate(object, false, true);
    }
    else if (opcode == Protocol::OP_OBJECT_INDICATION) {
        object = broker.handleObjectIndication(buffer, sequence, true,  true);
        broker.console.impl->eventObjectUpdate(object, true, true);
    }
    else {
        QPID_LOG(trace, "StaticContext::handleMessage invalid opcode: " << opcode);
        completeContext = true;
    }

    return completeContext;
}

void QueryContext::reserve()
{
    Mutex::ScopedLock _lock(lock);
    requestsOutstanding++;
}

void QueryContext::release()
{
    {
        Mutex::ScopedLock _lock(lock);
        if (--requestsOutstanding > 0)
            return;
    }

    Mutex::ScopedLock _block(broker.lock);
    broker.eventQueue.push_back(broker.eventQueryComplete(userContext, queryResponse));
}

bool QueryContext::handleMessage(uint8_t opcode, uint32_t sequence, const string& /*routingKey*/, Buffer& buffer)
{
    bool completeContext = false;
    ObjectPtr object;

    if      (opcode == Protocol::OP_COMMAND_COMPLETE) {
        broker.handleCommandComplete(buffer, sequence);
        completeContext = true;

        //
        //  Visit each agent and remove the sequence from that agent's in-flight list.
        //  This could be made more efficient because only one agent will have this sequence
        //  in its list.
        //
        map<uint32_t, AgentProxyPtr> copy;
        {
            Mutex::ScopedLock _block(broker.lock);
            copy = broker.agentList;
        }
        for (map<uint32_t, AgentProxyPtr>::iterator iter = copy.begin(); iter != copy.end(); iter++)
            iter->second->impl->delSequence(sequence);
    }
    else if (opcode == Protocol::OP_OBJECT_INDICATION) {
        object = broker.handleObjectIndication(buffer, sequence, true,  true);
        if (object.get() != 0)
            queryResponse->impl->results.push_back(object);
    }
    else {
        QPID_LOG(trace, "QueryContext::handleMessage invalid opcode: " << opcode);
        completeContext = true;
    }

    return completeContext;
}

void MethodContext::release()
{
    Mutex::ScopedLock _block(broker.lock);
    broker.eventQueue.push_back(broker.eventMethodResponse(userContext, methodResponse));
}

bool MethodContext::handleMessage(uint8_t opcode, uint32_t sequence, const string& /*routingKey*/, Buffer& buffer)
{
    if (opcode == Protocol::OP_METHOD_RESPONSE)
        methodResponse = broker.handleMethodResponse(buffer, sequence, schema);
    else
        QPID_LOG(trace, "QueryContext::handleMessage invalid opcode: " << opcode);

    return true;
}


//==================================================================
// Wrappers
//==================================================================

AgentProxy::AgentProxy(AgentProxyImpl* i) : impl(i) {}
AgentProxy::AgentProxy(const AgentProxy& from) : impl(new AgentProxyImpl(*(from.impl))) {}
AgentProxy::~AgentProxy() { delete impl; }
const char* AgentProxy::getLabel() const { return impl->getLabel().c_str(); }
uint32_t AgentProxy::getBrokerBank() const { return impl->getBrokerBank(); }
uint32_t AgentProxy::getAgentBank() const { return impl->getAgentBank(); }

BrokerProxy::BrokerProxy(Console& console) : impl(new BrokerProxyImpl(*this, console)) {}
BrokerProxy::~BrokerProxy() { delete impl; }
void BrokerProxy::sessionOpened(SessionHandle& sh) { impl->sessionOpened(sh); }
void BrokerProxy::sessionClosed() { impl->sessionClosed(); }
void BrokerProxy::startProtocol() { impl->startProtocol(); }
void BrokerProxy::handleRcvMessage(Message& message) { impl->handleRcvMessage(message); }
bool BrokerProxy::getXmtMessage(Message& item) const { return impl->getXmtMessage(item); }
void BrokerProxy::popXmt() { impl->popXmt(); }
bool BrokerProxy::getEvent(BrokerEvent& event) const { return impl->getEvent(event); }
void BrokerProxy::popEvent() { impl->popEvent(); }
uint32_t BrokerProxy::agentCount() const { return impl->agentCount(); }
const AgentProxy* BrokerProxy::getAgent(uint32_t idx) const { return impl->getAgent(idx); }
void BrokerProxy::sendQuery(const Query& query, void* context, const AgentProxy* agent) { impl->sendQuery(query, context, agent); }

MethodResponse::MethodResponse(const MethodResponse& from) : impl(new MethodResponseImpl(*(from.impl))) {}
MethodResponse::MethodResponse(MethodResponseImpl* i) : impl(i) {}
MethodResponse::~MethodResponse() {}
uint32_t MethodResponse::getStatus() const { return impl->getStatus(); }
const Value* MethodResponse::getException() const { return impl->getException(); }
const Value* MethodResponse::getArgs() const { return impl->getArgs(); }

QueryResponse::QueryResponse(QueryResponseImpl* i) : impl(i) {}
QueryResponse::~QueryResponse() {}
uint32_t QueryResponse::getStatus() const { return impl->getStatus(); }
const Value* QueryResponse::getException() const { return impl->getException(); }
uint32_t QueryResponse::getObjectCount() const { return impl->getObjectCount(); }
const Object* QueryResponse::getObject(uint32_t idx) const { return impl->getObject(idx); }

