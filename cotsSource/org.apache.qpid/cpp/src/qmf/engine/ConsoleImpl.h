#ifndef _QmfEngineConsoleEngineImpl_
#define _QmfEngineConsoleEngineImpl_

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

#include "qmf/engine/Console.h"
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
#include <qpid/sys/Mutex.h>
#include <qpid/sys/Time.h>
#include <qpid/sys/SystemInfo.h>
#include <string.h>
#include <string>
#include <deque>
#include <map>
#include <vector>
#include <iostream>
#include <fstream>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>

namespace qmf {
namespace engine {

    struct ConsoleEventImpl {
        typedef boost::shared_ptr<ConsoleEventImpl> Ptr;
        ConsoleEvent::EventKind kind;
        boost::shared_ptr<AgentProxy> agent;
        std::string name;
        const SchemaClassKey* classKey;
        boost::shared_ptr<Object> object;
        void* context;
        Event* event;
        uint64_t timestamp;
        bool hasProps;
        bool hasStats;

        ConsoleEventImpl(ConsoleEvent::EventKind k) :
            kind(k), classKey(0), context(0), event(0), timestamp(0) {}
        ~ConsoleEventImpl() {}
        ConsoleEvent copy();
    };

    class ConsoleImpl : public boost::noncopyable {
    public:
        ConsoleImpl(const ConsoleSettings& settings = ConsoleSettings());
        ~ConsoleImpl();

        bool getEvent(ConsoleEvent& event) const;
        void popEvent();

        void addConnection(BrokerProxy& broker, void* context);
        void delConnection(BrokerProxy& broker);

        uint32_t packageCount() const;
        const std::string& getPackageName(uint32_t idx) const;

        uint32_t classCount(const char* packageName) const;
        const SchemaClassKey* getClass(const char* packageName, uint32_t idx) const;

        ClassKind getClassKind(const SchemaClassKey* key) const;
        const SchemaObjectClass* getObjectClass(const SchemaClassKey* key) const;
        const SchemaEventClass* getEventClass(const SchemaClassKey* key) const;

        void bindPackage(const char* packageName);
        void bindClass(const SchemaClassKey* key);
        void bindClass(const char* packageName, const char* className);

        /*
        void startSync(const Query& query, void* context, SyncQuery& sync);
        void touchSync(SyncQuery& sync);
        void endSync(SyncQuery& sync);
        */

    private:
        friend class BrokerProxyImpl;
        friend struct StaticContext;
        const ConsoleSettings& settings;
        mutable qpid::sys::Mutex lock;
        std::deque<ConsoleEventImpl::Ptr> eventQueue;
        std::vector<BrokerProxyImpl*> brokerList;
        std::vector<std::pair<std::string, std::string> > bindingList; // exchange/key (empty exchange => QMF_EXCHANGE)

        // Declare a compare class for the class maps that compares the dereferenced
        // class key pointers.  The default behavior would be to compare the pointer
        // addresses themselves.
        struct KeyCompare {
            bool operator()(const SchemaClassKey* left, const SchemaClassKey* right) const {
                return *left < *right;
            }
        };

        typedef std::map<const SchemaClassKey*, SchemaObjectClass*, KeyCompare> ObjectClassList;
        typedef std::map<const SchemaClassKey*, SchemaEventClass*, KeyCompare> EventClassList;
        typedef std::map<std::string, std::pair<ObjectClassList, EventClassList> > PackageList;

        PackageList packages;

        void learnPackage(const std::string& packageName);
        void learnClass(SchemaObjectClass* cls);
        void learnClass(SchemaEventClass* cls);
        bool haveClass(const SchemaClassKey* key) const;
        SchemaObjectClass* getSchema(const SchemaClassKey* key) const;

        void eventAgentAdded(boost::shared_ptr<AgentProxy> agent);
        void eventAgentDeleted(boost::shared_ptr<AgentProxy> agent);
        void eventNewPackage(const std::string& packageName);
        void eventNewClass(const SchemaClassKey* key);
        void eventObjectUpdate(ObjectPtr object, bool prop, bool stat);
        void eventAgentHeartbeat(boost::shared_ptr<AgentProxy> agent, uint64_t timestamp);
    };
}
}

#endif

