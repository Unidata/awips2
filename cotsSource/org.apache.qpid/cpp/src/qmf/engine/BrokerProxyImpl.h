#ifndef _QmfEngineBrokerProxyImpl_
#define _QmfEngineBrokerProxyImpl_

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
#include "qmf/engine/ObjectImpl.h"
#include "qmf/engine/SchemaImpl.h"
#include "qmf/engine/ValueImpl.h"
#include "qmf/engine/QueryImpl.h"
#include "qmf/engine/SequenceManager.h"
#include "qmf/engine/MessageImpl.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/Uuid.h"
#include "qpid/sys/Mutex.h"
#include "boost/shared_ptr.hpp"
#include "boost/noncopyable.hpp"
#include <memory>
#include <string>
#include <deque>
#include <map>
#include <set>
#include <vector>

namespace qmf {
namespace engine {

    typedef boost::shared_ptr<MethodResponse> MethodResponsePtr;
    struct MethodResponseImpl {
        uint32_t status;
        const SchemaMethod* schema;
        std::auto_ptr<Value> exception;
        std::auto_ptr<Value> arguments;

        MethodResponseImpl(const MethodResponseImpl& from);
        MethodResponseImpl(qpid::framing::Buffer& buf, const SchemaMethod* schema);
        MethodResponseImpl(uint32_t status, const std::string& text);
        static MethodResponse* factory(qpid::framing::Buffer& buf, const SchemaMethod* schema);
        static MethodResponse* factory(uint32_t status, const std::string& text);
        ~MethodResponseImpl() {}
        uint32_t getStatus() const { return status; }
        const Value* getException() const { return exception.get(); }
        const Value* getArgs() const { return arguments.get(); }
    };

    typedef boost::shared_ptr<QueryResponse> QueryResponsePtr;
    struct QueryResponseImpl {
        uint32_t status;
        std::auto_ptr<Value> exception;
        std::vector<ObjectPtr> results;

        QueryResponseImpl() : status(0) {}
        static QueryResponse* factory() {
            QueryResponseImpl* impl(new QueryResponseImpl());
            return new QueryResponse(impl);
        }
        ~QueryResponseImpl() {}
        uint32_t getStatus() const { return status; }
        const Value* getException() const { return exception.get(); }
        uint32_t getObjectCount() const { return results.size(); }
        const Object* getObject(uint32_t idx) const;
    };

    struct BrokerEventImpl {
        typedef boost::shared_ptr<BrokerEventImpl> Ptr;
        BrokerEvent::EventKind kind;
        std::string name;
        std::string exchange;
        std::string bindingKey;
        void* context;
        QueryResponsePtr queryResponse;
        MethodResponsePtr methodResponse;

        BrokerEventImpl(BrokerEvent::EventKind k) : kind(k), context(0) {}
        ~BrokerEventImpl() {}
        BrokerEvent copy();
    };

    typedef boost::shared_ptr<AgentProxy> AgentProxyPtr;
    struct AgentProxyImpl {
        Console& console;
        BrokerProxy& broker;
        uint32_t agentBank;
        std::string label;
        std::set<uint32_t> inFlightSequences;

        AgentProxyImpl(Console& c, BrokerProxy& b, uint32_t ab, const std::string& l) : console(c), broker(b), agentBank(ab), label(l) {}
        static AgentProxy* factory(Console& c, BrokerProxy& b, uint32_t ab, const std::string& l) {
            AgentProxyImpl* impl(new AgentProxyImpl(c, b, ab, l));
            return new AgentProxy(impl);
        }
        ~AgentProxyImpl() {}
        const std::string& getLabel() const { return label; }
        uint32_t getBrokerBank() const { return 1; }
        uint32_t getAgentBank() const { return agentBank; }
        void addSequence(uint32_t seq) { inFlightSequences.insert(seq); }
        void delSequence(uint32_t seq) { inFlightSequences.erase(seq); }
        void releaseInFlight(SequenceManager& seqMgr) {
            for (std::set<uint32_t>::iterator iter = inFlightSequences.begin(); iter != inFlightSequences.end(); iter++)
                seqMgr.release(*iter);
            inFlightSequences.clear();
        }
    };

    class BrokerProxyImpl : public boost::noncopyable {
    public:
        BrokerProxyImpl(BrokerProxy& pub, Console& _console);
        ~BrokerProxyImpl() {}

        void sessionOpened(SessionHandle& sh);
        void sessionClosed();
        void startProtocol();

        void sendBufferLH(qpid::framing::Buffer& buf, const std::string& destination, const std::string& routingKey);
        void handleRcvMessage(Message& message);
        bool getXmtMessage(Message& item) const;
        void popXmt();

        bool getEvent(BrokerEvent& event) const;
        void popEvent();

        uint32_t agentCount() const;
        const AgentProxy* getAgent(uint32_t idx) const;
        void sendQuery(const Query& query, void* context, const AgentProxy* agent);
        bool sendGetRequestLH(SequenceContext::Ptr queryContext, const Query& query, const AgentProxy* agent);
        std::string encodeMethodArguments(const SchemaMethod* schema, const Value* args, qpid::framing::Buffer& buffer);
        void sendMethodRequest(ObjectId* oid, const SchemaObjectClass* cls, const std::string& method, const Value* args, void* context);

        void addBinding(const std::string& exchange, const std::string& key);
        void staticRelease() { decOutstanding(); }

    private:
        friend struct StaticContext;
        friend struct QueryContext;
        friend struct MethodContext;
        BrokerProxy& publicObject;
        mutable qpid::sys::Mutex lock;
        Console& console;
        std::string queueName;
        qpid::framing::Uuid brokerId;
        SequenceManager seqMgr;
        uint32_t requestsOutstanding;
        bool topicBound;
        std::map<uint32_t, AgentProxyPtr> agentList;
        std::deque<MessageImpl::Ptr> xmtQueue;
        std::deque<BrokerEventImpl::Ptr> eventQueue;

#       define MA_BUFFER_SIZE 65536
        char outputBuffer[MA_BUFFER_SIZE];

        BrokerEventImpl::Ptr eventDeclareQueue(const std::string& queueName);
        BrokerEventImpl::Ptr eventBind(const std::string& exchange, const std::string& queue, const std::string& key);
        BrokerEventImpl::Ptr eventSetupComplete();
        BrokerEventImpl::Ptr eventStable();
        BrokerEventImpl::Ptr eventQueryComplete(void* context, QueryResponsePtr response);
        BrokerEventImpl::Ptr eventMethodResponse(void* context, MethodResponsePtr response);

        void handleBrokerResponse(qpid::framing::Buffer& inBuffer, uint32_t seq);
        void handlePackageIndication(qpid::framing::Buffer& inBuffer, uint32_t seq);
        void handleCommandComplete(qpid::framing::Buffer& inBuffer, uint32_t seq);
        void handleClassIndication(qpid::framing::Buffer& inBuffer, uint32_t seq);
        MethodResponsePtr handleMethodResponse(qpid::framing::Buffer& inBuffer, uint32_t seq, const SchemaMethod* schema);
        void handleHeartbeatIndication(qpid::framing::Buffer& inBuffer, uint32_t seq, const std::string& routingKey);
        void handleEventIndication(qpid::framing::Buffer& inBuffer, uint32_t seq);
        void handleSchemaResponse(qpid::framing::Buffer& inBuffer, uint32_t seq);
        ObjectPtr handleObjectIndication(qpid::framing::Buffer& inBuffer, uint32_t seq, bool prop, bool stat);
        void updateAgentList(ObjectPtr obj);
        void incOutstandingLH();
        void decOutstanding();
    };

    //
    // StaticContext is used to handle:
    //
    //  1) Responses to console-level requests (for schema info, etc.)
    //  2) Unsolicited messages from agents (events, published updates, etc.)
    //
    struct StaticContext : public SequenceContext {
        StaticContext(BrokerProxyImpl& b) : broker(b) {}
        virtual ~StaticContext() {}
        void reserve() {}
        void release() { broker.staticRelease(); }
        bool handleMessage(uint8_t opcode, uint32_t sequence, const std::string& routingKey, qpid::framing::Buffer& buffer);
        BrokerProxyImpl& broker;
    };

    //
    // QueryContext is used to track and handle responses associated with a single Get Query
    //
    struct QueryContext : public SequenceContext {
        QueryContext(BrokerProxyImpl& b, void* u) :
        broker(b), userContext(u), requestsOutstanding(0), queryResponse(QueryResponseImpl::factory()) {}
        virtual ~QueryContext() {}
        void reserve();
        void release();
        bool handleMessage(uint8_t opcode, uint32_t sequence, const std::string& routingKey, qpid::framing::Buffer& buffer);

        mutable qpid::sys::Mutex lock;
        BrokerProxyImpl& broker;
        void* userContext;
        uint32_t requestsOutstanding;
        QueryResponsePtr queryResponse;
    };

    struct MethodContext : public SequenceContext {
        MethodContext(BrokerProxyImpl& b, void* u, const SchemaMethod* s) : broker(b), userContext(u), schema(s) {}
        virtual ~MethodContext() {}
        void reserve() {}
        void release();
        bool handleMessage(uint8_t opcode, uint32_t sequence, const std::string& routingKey, qpid::framing::Buffer& buffer);

        BrokerProxyImpl& broker;
        void* userContext;
        const SchemaMethod* schema;
        MethodResponsePtr methodResponse;
    };

}
}

#endif

