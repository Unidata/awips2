#ifndef _broker_Exchange_h
#define _broker_Exchange_h

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

#include <boost/shared_ptr.hpp>
#include "qpid/broker/BrokerImportExport.h"
#include "qpid/broker/Deliverable.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/MessageStore.h"
#include "qpid/broker/PersistableExchange.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/sys/Mutex.h"
#include "qpid/management/Manageable.h"
#include "qmf/org/apache/qpid/broker/Exchange.h"
#include "qmf/org/apache/qpid/broker/Binding.h"

namespace qpid {
namespace broker {

class ExchangeRegistry;

class Exchange : public PersistableExchange, public management::Manageable {
public:
    struct Binding : public management::Manageable {
        typedef boost::shared_ptr<Binding>       shared_ptr;
        typedef std::vector<Binding::shared_ptr> vector;

        Queue::shared_ptr         queue;
        const std::string         key;
        const framing::FieldTable args;
        qmf::org::apache::qpid::broker::Binding* mgmtBinding;

        Binding(const std::string& key, Queue::shared_ptr queue, Exchange* parent = 0,
                framing::FieldTable args = framing::FieldTable(), const std::string& origin = std::string());
        ~Binding();
        management::ManagementObject* GetManagementObject() const;
    };

private:
    const std::string name;
    const bool durable;
    std::string alternateName;
    boost::shared_ptr<Exchange> alternate;
    uint32_t alternateUsers;
    mutable uint64_t persistenceId;

protected:
    mutable qpid::framing::FieldTable args;
    bool sequence;
    mutable qpid::sys::Mutex sequenceLock;
    int64_t sequenceNo;
    bool ive;
    boost::intrusive_ptr<Message> lastMsg;

    class PreRoute{
    public:
        PreRoute(Deliverable& msg, Exchange* _p);
        ~PreRoute();
    private:
        Exchange* parent;
    };
           
    typedef boost::shared_ptr<const std::vector<boost::shared_ptr<qpid::broker::Exchange::Binding> > > ConstBindingList;
    typedef boost::shared_ptr<      std::vector<boost::shared_ptr<qpid::broker::Exchange::Binding> > > BindingList;
    void doRoute(Deliverable& msg, ConstBindingList b);
    void routeIVE();
           

    struct MatchQueue {
        const Queue::shared_ptr queue;        
        MatchQueue(Queue::shared_ptr q);
        bool operator()(Exchange::Binding::shared_ptr b);
    };

    class FedBinding {
        uint32_t localBindings;
        std::set<std::string> originSet;
    public:
        FedBinding() : localBindings(0) {}
        bool hasLocal() const { return localBindings != 0; }
        bool addOrigin(const std::string& origin) {
            if (origin.empty()) {
                localBindings++;
                return localBindings == 1;
            }
            originSet.insert(origin);
            return true;
        }
        bool delOrigin(const std::string& origin) {
            originSet.erase(origin);
            return true;
        }
        bool delOrigin() {
            if (localBindings > 0)
                localBindings--;
            return localBindings == 0;
        }
        uint32_t count() {
            return localBindings + originSet.size();
        }
    };

    qmf::org::apache::qpid::broker::Exchange* mgmtExchange;

public:
    typedef boost::shared_ptr<Exchange> shared_ptr;

    QPID_BROKER_EXTERN explicit Exchange(const std::string& name, management::Manageable* parent = 0,
                                         Broker* broker = 0);
    QPID_BROKER_EXTERN Exchange(const std::string& _name, bool _durable, const qpid::framing::FieldTable& _args,
                                management::Manageable* parent = 0, Broker* broker = 0);
    QPID_BROKER_EXTERN virtual ~Exchange();

    const std::string& getName() const { return name; }
    bool isDurable() { return durable; }
    qpid::framing::FieldTable& getArgs() { return args; }

    Exchange::shared_ptr getAlternate() { return alternate; }
    void setAlternate(Exchange::shared_ptr _alternate);
    void incAlternateUsers() { alternateUsers++; }
    void decAlternateUsers() { alternateUsers--; }
    bool inUseAsAlternate() { return alternateUsers > 0; }

    virtual std::string getType() const = 0;
    virtual bool bind(Queue::shared_ptr queue, const std::string& routingKey, const qpid::framing::FieldTable* args) = 0;
    virtual bool unbind(Queue::shared_ptr queue, const std::string& routingKey, const qpid::framing::FieldTable* args) = 0;
    virtual bool isBound(Queue::shared_ptr queue, const std::string* const routingKey, const qpid::framing::FieldTable* const args) = 0;
    QPID_BROKER_EXTERN virtual void setProperties(const boost::intrusive_ptr<Message>&);
    virtual void route(Deliverable& msg, const std::string& routingKey, const qpid::framing::FieldTable* args) = 0;
    
    //PersistableExchange:
    QPID_BROKER_EXTERN void setPersistenceId(uint64_t id) const;
    uint64_t getPersistenceId() const { return persistenceId; }
    QPID_BROKER_EXTERN uint32_t encodedSize() const;
    QPID_BROKER_EXTERN virtual void encode(framing::Buffer& buffer) const;

    static QPID_BROKER_EXTERN Exchange::shared_ptr decode(ExchangeRegistry& exchanges, framing::Buffer& buffer);

    // Manageable entry points
    QPID_BROKER_EXTERN management::ManagementObject* GetManagementObject(void) const;

    // Federation hooks
    class DynamicBridge {
    public:
        virtual ~DynamicBridge() {}
        virtual void propagateBinding(const std::string& key, const std::string& tagList, const std::string& op, const std::string& origin) = 0;
        virtual void sendReorigin() = 0;
        virtual bool containsLocalTag(const std::string& tagList) const = 0;
        virtual const std::string& getLocalTag() const = 0;
    };

    void registerDynamicBridge(DynamicBridge* db);
    void removeDynamicBridge(DynamicBridge* db);
    virtual bool supportsDynamicBinding() { return false; }
    Broker* getBroker() const { return broker; }
    /**
     * Notify exchange that recovery has completed.
     */
    void recoveryComplete(ExchangeRegistry& exchanges);

protected:
    qpid::sys::Mutex bridgeLock;
    std::vector<DynamicBridge*> bridgeVector;
    Broker* broker;

    QPID_BROKER_EXTERN virtual void handleHelloRequest();
    void propagateFedOp(const std::string& routingKey, const std::string& tags,
                        const std::string& op,         const std::string& origin);
};

}}

#endif  /*!_broker_Exchange.cpp_h*/
