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

#include "qmf/engine/ConsoleImpl.h"
#include "qmf/engine/MessageImpl.h"
#include "qmf/engine/SchemaImpl.h"
#include "qmf/engine/Typecode.h"
#include "qmf/engine/ObjectImpl.h"
#include "qmf/engine/ObjectIdImpl.h"
#include "qmf/engine/QueryImpl.h"
#include "qmf/engine/ValueImpl.h"
#include "qmf/engine/Protocol.h"
#include "qmf/engine/SequenceManager.h"
#include "qmf/engine/BrokerProxyImpl.h"
#include <qpid/framing/Buffer.h>
#include <qpid/framing/Uuid.h>
#include <qpid/framing/FieldTable.h>
#include <qpid/framing/FieldValue.h>
#include <qpid/log/Statement.h>
#include <qpid/sys/Time.h>
#include <qpid/sys/SystemInfo.h>
#include <string.h>
#include <iostream>
#include <fstream>

using namespace std;
using namespace qmf::engine;
using namespace qpid::framing;
using namespace qpid::sys;

namespace {
    const char* QMF_EXCHANGE = "qpid.management";
}

#define STRING_REF(s) {if (!s.empty()) item.s = const_cast<char*>(s.c_str());}

ConsoleEvent ConsoleEventImpl::copy()
{
    ConsoleEvent item;

    ::memset(&item, 0, sizeof(ConsoleEvent));
    item.kind      = kind;
    item.agent     = agent.get();
    item.classKey  = classKey;
    item.object    = object.get();
    item.context   = context;
    item.event     = event;
    item.timestamp = timestamp;
    item.hasProps  = hasProps;
    item.hasStats  = hasStats;

    STRING_REF(name);

    return item;
}

ConsoleImpl::ConsoleImpl(const ConsoleSettings& s) : settings(s)
{
    bindingList.push_back(pair<string, string>(string(), "schema.#"));
    if (settings.rcvObjects && settings.rcvEvents && settings.rcvHeartbeats && !settings.userBindings) {
        bindingList.push_back(pair<string, string>(string(), "console.#"));
    } else {
        if (settings.rcvObjects && !settings.userBindings)
            bindingList.push_back(pair<string, string>(string(), "console.obj.#"));
        else
            bindingList.push_back(pair<string, string>(string(), "console.obj.*.*.org.apache.qpid.broker.agent"));
        if (settings.rcvEvents)
            bindingList.push_back(pair<string, string>(string(), "console.event.#"));
        if (settings.rcvHeartbeats)
            bindingList.push_back(pair<string, string>(string(), "console.heartbeat.#"));
    }
}

ConsoleImpl::~ConsoleImpl()
{
    // This function intentionally left blank.
}

bool ConsoleImpl::getEvent(ConsoleEvent& event) const
{
    Mutex::ScopedLock _lock(lock);
    if (eventQueue.empty())
        return false;
    event = eventQueue.front()->copy();
    return true;
}

void ConsoleImpl::popEvent()
{
    Mutex::ScopedLock _lock(lock);
    if (!eventQueue.empty())
        eventQueue.pop_front();
}

void ConsoleImpl::addConnection(BrokerProxy& broker, void* /*context*/)
{
    Mutex::ScopedLock _lock(lock);
    brokerList.push_back(broker.impl);
}

void ConsoleImpl::delConnection(BrokerProxy& broker)
{
    Mutex::ScopedLock _lock(lock);
    for (vector<BrokerProxyImpl*>::iterator iter = brokerList.begin();
         iter != brokerList.end(); iter++)
        if (*iter == broker.impl) {
            brokerList.erase(iter);
            break;
        }
}

uint32_t ConsoleImpl::packageCount() const
{
    Mutex::ScopedLock _lock(lock);
    return packages.size();
}

const string& ConsoleImpl::getPackageName(uint32_t idx) const
{
    const static string empty;

    Mutex::ScopedLock _lock(lock);
    if (idx >= packages.size())
        return empty;

    PackageList::const_iterator iter = packages.begin();
    for (uint32_t i = 0; i < idx; i++) iter++;
    return iter->first;
}

uint32_t ConsoleImpl::classCount(const char* packageName) const
{
    Mutex::ScopedLock _lock(lock);
    PackageList::const_iterator pIter = packages.find(packageName);
    if (pIter == packages.end())
        return 0;

    const ObjectClassList& oList = pIter->second.first;
    const EventClassList& eList = pIter->second.second;

    return oList.size() + eList.size();
}

const SchemaClassKey* ConsoleImpl::getClass(const char* packageName, uint32_t idx) const
{
    Mutex::ScopedLock _lock(lock);
    PackageList::const_iterator pIter = packages.find(packageName);
    if (pIter == packages.end())
        return 0;

    const ObjectClassList& oList = pIter->second.first;
    const EventClassList& eList = pIter->second.second;
    uint32_t count = 0;
    
    for (ObjectClassList::const_iterator oIter = oList.begin();
         oIter != oList.end(); oIter++) {
        if (count == idx)
            return oIter->second->getClassKey();
        count++;
    }

    for (EventClassList::const_iterator eIter = eList.begin();
         eIter != eList.end(); eIter++) {
        if (count == idx)
            return eIter->second->getClassKey();
        count++;
    }

    return 0;
}

ClassKind ConsoleImpl::getClassKind(const SchemaClassKey* key) const
{
    Mutex::ScopedLock _lock(lock);
    PackageList::const_iterator pIter = packages.find(key->getPackageName());
    if (pIter == packages.end())
        return CLASS_OBJECT;

    const EventClassList& eList = pIter->second.second;
    if (eList.find(key) != eList.end())
        return CLASS_EVENT;
    return CLASS_OBJECT;
}

const SchemaObjectClass* ConsoleImpl::getObjectClass(const SchemaClassKey* key) const
{
    Mutex::ScopedLock _lock(lock);
    PackageList::const_iterator pIter = packages.find(key->getPackageName());
    if (pIter == packages.end())
        return 0;

    const ObjectClassList& oList = pIter->second.first;
    ObjectClassList::const_iterator iter = oList.find(key);
    if (iter == oList.end())
        return 0;
    return iter->second;
}

const SchemaEventClass* ConsoleImpl::getEventClass(const SchemaClassKey* key) const
{
    Mutex::ScopedLock _lock(lock);
    PackageList::const_iterator pIter = packages.find(key->getPackageName());
    if (pIter == packages.end())
        return 0;

    const EventClassList& eList = pIter->second.second;
    EventClassList::const_iterator iter = eList.find(key);
    if (iter == eList.end())
        return 0;
    return iter->second;
}

void ConsoleImpl::bindPackage(const char* packageName)
{
    stringstream key;
    key << "console.obj.*.*." << packageName << ".#";
    Mutex::ScopedLock _lock(lock);
    bindingList.push_back(pair<string, string>(string(), key.str()));
    for (vector<BrokerProxyImpl*>::iterator iter = brokerList.begin();
         iter != brokerList.end(); iter++)
        (*iter)->addBinding(QMF_EXCHANGE, key.str());
}

void ConsoleImpl::bindClass(const SchemaClassKey* classKey)
{
    stringstream key;
    key << "console.obj.*.*." << classKey->getPackageName() << "." << classKey->getClassName() << ".#";
    Mutex::ScopedLock _lock(lock);
    bindingList.push_back(pair<string, string>(string(), key.str()));
    for (vector<BrokerProxyImpl*>::iterator iter = brokerList.begin();
         iter != brokerList.end(); iter++)
        (*iter)->addBinding(QMF_EXCHANGE, key.str());
}

void ConsoleImpl::bindClass(const char* packageName, const char* className)
{
    stringstream key;
    key << "console.obj.*.*." << packageName << "." << className << ".#";
    Mutex::ScopedLock _lock(lock);
    bindingList.push_back(pair<string, string>(string(), key.str()));
    for (vector<BrokerProxyImpl*>::iterator iter = brokerList.begin();
         iter != brokerList.end(); iter++)
        (*iter)->addBinding(QMF_EXCHANGE, key.str());
}

/*
void ConsoleImpl::startSync(const Query& query, void* context, SyncQuery& sync)
{
}

void ConsoleImpl::touchSync(SyncQuery& sync)
{
}

void ConsoleImpl::endSync(SyncQuery& sync)
{
}
*/

void ConsoleImpl::learnPackage(const string& packageName)
{
    Mutex::ScopedLock _lock(lock);
    if (packages.find(packageName) == packages.end()) {
        packages.insert(pair<string, pair<ObjectClassList, EventClassList> >
                        (packageName, pair<ObjectClassList, EventClassList>(ObjectClassList(), EventClassList())));
        eventNewPackage(packageName);
    }
}

void ConsoleImpl::learnClass(SchemaObjectClass* cls)
{
    Mutex::ScopedLock _lock(lock);
    const SchemaClassKey* key = cls->getClassKey();
    PackageList::iterator pIter = packages.find(key->getPackageName());
    if (pIter == packages.end())
        return;

    ObjectClassList& list = pIter->second.first;
    if (list.find(key) == list.end()) {
        list[key] = cls;
        eventNewClass(key);
    }
}

void ConsoleImpl::learnClass(SchemaEventClass* cls)
{
    Mutex::ScopedLock _lock(lock);
    const SchemaClassKey* key = cls->getClassKey();
    PackageList::iterator pIter = packages.find(key->getPackageName());
    if (pIter == packages.end())
        return;

    EventClassList& list = pIter->second.second;
    if (list.find(key) == list.end()) {
        list[key] = cls;
        eventNewClass(key);
    }
}

bool ConsoleImpl::haveClass(const SchemaClassKey* key) const
{
    Mutex::ScopedLock _lock(lock);
    PackageList::const_iterator pIter = packages.find(key->getPackageName());
    if (pIter == packages.end())
        return false;

    const ObjectClassList& oList = pIter->second.first;
    const EventClassList& eList = pIter->second.second;

    return oList.find(key) != oList.end() || eList.find(key) != eList.end();
}

SchemaObjectClass* ConsoleImpl::getSchema(const SchemaClassKey* key) const
{
    Mutex::ScopedLock _lock(lock);
    PackageList::const_iterator pIter = packages.find(key->getPackageName());
    if (pIter == packages.end())
        return 0;

    const ObjectClassList& oList = pIter->second.first;
    ObjectClassList::const_iterator iter = oList.find(key);
    if (iter == oList.end())
        return 0;

    return iter->second;
}

void ConsoleImpl::eventAgentAdded(boost::shared_ptr<AgentProxy> agent)
{
    ConsoleEventImpl::Ptr event(new ConsoleEventImpl(ConsoleEvent::AGENT_ADDED));
    event->agent = agent;
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(event);
}

void ConsoleImpl::eventAgentDeleted(boost::shared_ptr<AgentProxy> agent)
{
    ConsoleEventImpl::Ptr event(new ConsoleEventImpl(ConsoleEvent::AGENT_DELETED));
    event->agent = agent;
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(event);
}

void ConsoleImpl::eventNewPackage(const string& packageName)
{
    ConsoleEventImpl::Ptr event(new ConsoleEventImpl(ConsoleEvent::NEW_PACKAGE));
    event->name = packageName;
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(event);
}

void ConsoleImpl::eventNewClass(const SchemaClassKey* key)
{
    ConsoleEventImpl::Ptr event(new ConsoleEventImpl(ConsoleEvent::NEW_CLASS));
    event->classKey = key;
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(event);
}

void ConsoleImpl::eventObjectUpdate(ObjectPtr object, bool prop, bool stat)
{
    ConsoleEventImpl::Ptr event(new ConsoleEventImpl(ConsoleEvent::OBJECT_UPDATE));
    event->object = object;
    event->hasProps = prop;
    event->hasStats = stat;
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(event);
}

void ConsoleImpl::eventAgentHeartbeat(boost::shared_ptr<AgentProxy> agent, uint64_t timestamp)
{
    ConsoleEventImpl::Ptr event(new ConsoleEventImpl(ConsoleEvent::AGENT_HEARTBEAT));
    event->agent = agent;
    event->timestamp = timestamp;
    Mutex::ScopedLock _lock(lock);
    eventQueue.push_back(event);
}

//==================================================================
// Wrappers
//==================================================================

Console::Console(const ConsoleSettings& settings) : impl(new ConsoleImpl(settings)) {}
Console::~Console() { delete impl; }
bool Console::getEvent(ConsoleEvent& event) const { return impl->getEvent(event); }
void Console::popEvent() { impl->popEvent(); }
void Console::addConnection(BrokerProxy& broker, void* context) { impl->addConnection(broker, context); }
void Console::delConnection(BrokerProxy& broker) { impl->delConnection(broker); }
uint32_t Console::packageCount() const { return impl->packageCount(); }
const char* Console::getPackageName(uint32_t idx) const { return impl->getPackageName(idx).c_str(); }
uint32_t Console::classCount(const char* packageName) const { return impl->classCount(packageName); }
const SchemaClassKey* Console::getClass(const char* packageName, uint32_t idx) const { return impl->getClass(packageName, idx); }
ClassKind Console::getClassKind(const SchemaClassKey* key) const { return impl->getClassKind(key); }
const SchemaObjectClass* Console::getObjectClass(const SchemaClassKey* key) const { return impl->getObjectClass(key); }
const SchemaEventClass* Console::getEventClass(const SchemaClassKey* key) const { return impl->getEventClass(key); }
void Console::bindPackage(const char* packageName) { impl->bindPackage(packageName); }
void Console::bindClass(const SchemaClassKey* key) { impl->bindClass(key); }
void Console::bindClass(const char* packageName, const char* className) { impl->bindClass(packageName, className); }
//void Console::startSync(const Query& query, void* context, SyncQuery& sync) { impl->startSync(query, context, sync); }
//void Console::touchSync(SyncQuery& sync) { impl->touchSync(sync); }
//void Console::endSync(SyncQuery& sync) { impl->endSync(sync); }


