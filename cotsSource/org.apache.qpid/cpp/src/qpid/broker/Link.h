#ifndef _broker_Link_h
#define _broker_Link_h

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
#include "qpid/broker/MessageStore.h"
#include "qpid/broker/PersistableConfig.h"
#include "qpid/broker/Bridge.h"
#include "qpid/broker/RetryList.h"
#include "qpid/sys/Mutex.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/management/Manageable.h"
#include "qpid/management/ManagementAgent.h"
#include "qmf/org/apache/qpid/broker/Link.h"
#include <boost/ptr_container/ptr_vector.hpp>

namespace qpid {
    namespace broker {

        using std::string;
        class LinkRegistry;
        class Broker;
        class Connection;

        class Link : public PersistableConfig, public management::Manageable {
        private:
            sys::Mutex          lock;
            LinkRegistry*       links;
            MessageStore*       store;
            string        host;
            uint16_t      port;
            string        transport;
            bool          durable;
            string        authMechanism;
            string        username;
            string        password;
            mutable uint64_t    persistenceId;
            qmf::org::apache::qpid::broker::Link* mgmtObject;
            Broker* broker;
            int     state;
            uint32_t visitCount;
            uint32_t currentInterval;
            bool     closing;
            RetryList urls;
            bool updateUrls;

            typedef std::vector<Bridge::shared_ptr> Bridges;
            Bridges created;   // Bridges pending creation
            Bridges active;    // Bridges active
            Bridges cancellations;    // Bridges pending cancellation
            uint channelCounter;
            Connection* connection;
            management::ManagementAgent* agent;

            static const int STATE_WAITING     = 1;
            static const int STATE_CONNECTING  = 2;
            static const int STATE_OPERATIONAL = 3;
            static const int STATE_FAILED      = 4;
            static const int STATE_CLOSED      = 5;
            static const int STATE_PASSIVE     = 6;

            static const uint32_t MAX_INTERVAL = 32;

            void setStateLH (int newState);
            void startConnectionLH();        // Start the IO Connection
            void destroy();                  // Called when mgmt deletes this link
            void ioThreadProcessing();       // Called on connection's IO thread by request
            bool tryFailover();              // Called during maintenance visit
            void checkClosePermission();     // ACL check for explict mgmt call to close this link

        public:
            typedef boost::shared_ptr<Link> shared_ptr;

            Link(LinkRegistry* links,
                 MessageStore* store,
                 string&       host,
                 uint16_t      port,
                 string&       transport,
                 bool          durable,
                 string&       authMechanism,
                 string&       username,
                 string&       password,
                 Broker*       broker,
                 management::Manageable* parent = 0);
            virtual ~Link();

            std::string getHost() { return host; }
            uint16_t    getPort() { return port; }
            bool isDurable() { return durable; }
            void maintenanceVisit ();
            uint nextChannel();
            void add(Bridge::shared_ptr);
            void cancel(Bridge::shared_ptr);

            void established();              // Called when connection is created
            void closed(int, std::string);   // Called when connection goes away
            void setConnection(Connection*); // Set pointer to the AMQP Connection
            void reconnect(const TcpAddress&); //called by LinkRegistry

            string getAuthMechanism() { return authMechanism; }
            string getUsername()      { return username; }
            string getPassword()      { return password; }
            Broker* getBroker()       { return broker; }

            void notifyConnectionForced(const std::string text);
            void setPassive(bool p);
            
            // PersistableConfig:
            void     setPersistenceId(uint64_t id) const;
            uint64_t getPersistenceId() const { return persistenceId; }
            uint32_t encodedSize() const;
            void     encode(framing::Buffer& buffer) const; 
            const string& getName() const;

            static Link::shared_ptr decode(LinkRegistry& links, framing::Buffer& buffer);

            // Manageable entry points
            management::ManagementObject*    GetManagementObject(void) const;
            management::Manageable::status_t ManagementMethod(uint32_t, management::Args&, std::string&);
        };
    }
}


#endif  /*!_broker_Link.cpp_h*/
