#ifndef _QmfEngineConsole_
#define _QmfEngineConsole_

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

#include <qmf/engine/ResilientConnection.h>
#include <qmf/engine/Schema.h>
#include <qmf/engine/ObjectId.h>
#include <qmf/engine/Object.h>
#include <qmf/engine/Event.h>
#include <qmf/engine/Query.h>
#include <qmf/engine/Value.h>
#include <qmf/engine/Message.h>

namespace qmf {
namespace engine {

    class Console;
    class ConsoleImpl;
    class BrokerProxyImpl;
    class AgentProxy;
    struct AgentProxyImpl;
    struct MethodResponseImpl;
    struct QueryResponseImpl;
    struct QueryContext;

    /**
     *
     */
    class MethodResponse {
    public:
        MethodResponse(const MethodResponse& from);
        ~MethodResponse();
        uint32_t getStatus() const;
        const Value* getException() const;
        const Value* getArgs() const;

    private:
        friend struct MethodResponseImpl;
        friend class ConsoleImpl;
        MethodResponse(MethodResponseImpl* impl);
        MethodResponseImpl* impl;
    };

    /**
     *
     */
    class QueryResponse {
    public:
        ~QueryResponse();
        uint32_t getStatus() const;
        const Value* getException() const;
        uint32_t getObjectCount() const;
        const Object* getObject(uint32_t idx) const;

    private:
        friend struct QueryResponseImpl;
        friend struct QueryContext;
        QueryResponse(QueryResponseImpl* impl);
        QueryResponseImpl *impl;
    };

    /**
     *
     */
    struct ConsoleEvent {
        enum EventKind {
            AGENT_ADDED     = 1,
            AGENT_DELETED   = 2,
            NEW_PACKAGE     = 3,
            NEW_CLASS       = 4,
            OBJECT_UPDATE   = 5,
            EVENT_RECEIVED  = 7,
            AGENT_HEARTBEAT = 8
        };

        EventKind       kind;
        AgentProxy*     agent;          // (AGENT_[ADDED|DELETED|HEARTBEAT])
        char*           name;           // (NEW_PACKAGE)
        const SchemaClassKey* classKey; // (NEW_CLASS)
        Object*         object;         // (OBJECT_UPDATE)
        void*           context;        // (OBJECT_UPDATE)
        Event*          event;          // (EVENT_RECEIVED)
        uint64_t        timestamp;      // (AGENT_HEARTBEAT)
        QueryResponse*  queryResponse;  // (QUERY_COMPLETE)
        bool            hasProps;
        bool            hasStats;
    };

    /**
     *
     */
    struct BrokerEvent {
        enum EventKind {
            BROKER_INFO     = 10,
            DECLARE_QUEUE   = 11,
            DELETE_QUEUE    = 12,
            BIND            = 13,
            UNBIND          = 14,
            SETUP_COMPLETE  = 15,
            STABLE          = 16,
            QUERY_COMPLETE  = 17,
            METHOD_RESPONSE = 18
        };

        EventKind kind;
        char*           name;           // ([DECLARE|DELETE]_QUEUE, [UN]BIND)
        char*           exchange;       // ([UN]BIND)
        char*           bindingKey;     // ([UN]BIND)
        void*           context;        // (QUERY_COMPLETE, METHOD_RESPONSE)
        QueryResponse*  queryResponse;  // (QUERY_COMPLETE)
        MethodResponse* methodResponse; // (METHOD_RESPONSE)
    };

    /**
     *
     */
    class AgentProxy {
    public:
        AgentProxy(const AgentProxy& from);
        ~AgentProxy();
        const char* getLabel() const;
        uint32_t getBrokerBank() const;
        uint32_t getAgentBank() const;

    private:
        friend struct StaticContext;
        friend struct QueryContext;
        friend struct AgentProxyImpl;
        friend class BrokerProxyImpl;
        AgentProxy(AgentProxyImpl* impl);
        AgentProxyImpl* impl;
    };

    /**
     *
     */
    class BrokerProxy {
    public:
        BrokerProxy(Console& console);
        ~BrokerProxy();

        void sessionOpened(SessionHandle& sh);
        void sessionClosed();
        void startProtocol();

        void handleRcvMessage(Message& message);
        bool getXmtMessage(Message& item) const;
        void popXmt();

        bool getEvent(BrokerEvent& event) const;
        void popEvent();

        uint32_t agentCount() const;
        const AgentProxy* getAgent(uint32_t idx) const;
        void sendQuery(const Query& query, void* context, const AgentProxy* agent = 0);

    private:
        friend class ConsoleImpl;
        friend struct StaticContext;
        BrokerProxyImpl* impl;
    };

    // TODO - move this to a public header
    struct ConsoleSettings {
        bool rcvObjects;
        bool rcvEvents;
        bool rcvHeartbeats;
        bool userBindings;

        ConsoleSettings() :
            rcvObjects(true),
            rcvEvents(true),
            rcvHeartbeats(true),
            userBindings(false) {}
    };

    class Console {
    public:
        Console(const ConsoleSettings& settings = ConsoleSettings());
        ~Console();

        bool getEvent(ConsoleEvent& event) const;
        void popEvent();

        void addConnection(BrokerProxy& broker, void* context);
        void delConnection(BrokerProxy& broker);

        uint32_t packageCount() const;
        const char* getPackageName(uint32_t idx) const;

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
        friend struct AgentProxyImpl;
        friend struct StaticContext;
        ConsoleImpl* impl;
    };
}
}

#endif

