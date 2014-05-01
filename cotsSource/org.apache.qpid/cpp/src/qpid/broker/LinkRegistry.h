#ifndef _broker_LinkRegistry_h
#define _broker_LinkRegistry_h

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

#include <map>
#include "qpid/broker/Bridge.h"
#include "qpid/broker/MessageStore.h"
#include "qpid/Address.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Timer.h"
#include "qpid/management/Manageable.h"
#include <boost/shared_ptr.hpp>
#include <boost/intrusive_ptr.hpp>

namespace qpid {
namespace broker {

    class Link;
    class Broker;
    class Connection;
    class LinkRegistry {

        // Declare a timer task to manage the establishment of link connections and the
        // re-establishment of lost link connections.
        struct Periodic : public sys::TimerTask
        {
            LinkRegistry& links;

            Periodic(LinkRegistry& links);
            virtual ~Periodic() {};
            void fire();
        };

        typedef std::map<std::string, boost::shared_ptr<Link> > LinkMap;
        typedef std::map<std::string, Bridge::shared_ptr> BridgeMap;
        typedef std::map<std::string, TcpAddress> AddressMap;

        LinkMap   links;
        LinkMap   linksToDestroy;
        BridgeMap bridges;
        BridgeMap bridgesToDestroy;
        AddressMap reMappings;

        qpid::sys::Mutex lock;
        Broker* broker;
        sys::Timer* timer;
        boost::intrusive_ptr<qpid::sys::TimerTask> maintenanceTask;
        management::Manageable* parent;
        MessageStore* store;
        bool passive;
        bool passiveChanged;
        std::string realm;

        void periodicMaintenance ();
        bool updateAddress(const std::string& oldKey, const TcpAddress& newAddress);
        boost::shared_ptr<Link> findLink(const std::string& key);
        static std::string createKey(const TcpAddress& address);

    public:
        LinkRegistry (); // Only used in store tests
        LinkRegistry (Broker* _broker);
        ~LinkRegistry();

        std::pair<boost::shared_ptr<Link>, bool>
            declare(std::string& host,
                    uint16_t     port,
                    std::string& transport,
                    bool         durable,
                    std::string& authMechanism,
                    std::string& username,
                    std::string& password);
        std::pair<Bridge::shared_ptr, bool>
            declare(std::string& host,
                    uint16_t     port,
                    bool         durable,
                    std::string& src,
                    std::string& dest,
                    std::string& key,
                    bool         isQueue,
                    bool         isLocal,
                    std::string& id,
                    std::string& excludes,
                    bool         dynamic,
                    uint16_t     sync);

        void destroy(const std::string& host, const uint16_t port);
        void destroy(const std::string& host,
                     const uint16_t     port,
                     const std::string& src,
                     const std::string& dest,
                     const std::string& key);

        /**
         * Register the manageable parent for declared queues
         */
        void setParent (management::Manageable* _parent) { parent = _parent; }

        /**
         * Set the store to use.  May only be called once.
         */
        void setStore (MessageStore*);

        /**
         * Return the message store used.
         */
        MessageStore* getStore() const;

        void notifyConnection (const std::string& key, Connection* c);
        void notifyClosed     (const std::string& key);
        void notifyConnectionForced    (const std::string& key, const std::string& text);
        std::string getAuthMechanism   (const std::string& key);
        std::string getAuthCredentials (const std::string& key);
        std::string getAuthIdentity    (const std::string& key);

        /**
         * Called by links failing over to new address
         */
        void changeAddress(const TcpAddress& oldAddress, const TcpAddress& newAddress);
        /**
         * Called to alter passive state. In passive state the links
         * and bridges managed by a link registry will be recorded and
         * updated but links won't actually establish connections and
         * bridges won't therefore pull or push any messages.
         */
        void setPassive(bool);
    };
}
}


#endif  /*!_broker_LinkRegistry_h*/
