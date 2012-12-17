#ifndef _QPID_CONSOLE_SESSION_MANAGER_H
#define _QPID_CONSOLE_SESSION_MANAGER_H

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

#include "qpid/console/ConsoleImportExport.h"
#include "qpid/console/Broker.h"
#include "qpid/console/Package.h"
#include "qpid/console/SequenceManager.h"
#include "qpid/console/ClassKey.h"
#include "qpid/console/Schema.h"
#include "qpid/console/Agent.h"
#include "qpid/console/Object.h"
#include "qpid/console/ObjectId.h"
#include "qpid/console/Value.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Condition.h"
#include "qpid/client/ConnectionSettings.h"
#include <string>
#include <vector>

namespace qpid {
namespace console {

class ConsoleListener;

/**
 *
 * \ingroup qmfconsoleapi
 */
class SessionManager
{
  public:
    typedef std::vector<std::string> NameVector;
    typedef std::vector<ClassKey> KeyVector;
    QPID_CONSOLE_EXTERN ~SessionManager();

    struct Settings {
        bool rcvObjects;
        bool rcvEvents;
        bool rcvHeartbeats;
        bool userBindings;
        uint32_t methodTimeout;
        uint32_t getTimeout;

        Settings() : rcvObjects(true), rcvEvents(true), rcvHeartbeats(true),
                     userBindings(false), methodTimeout(20), getTimeout(20)
        {}
    };

    /** Create a new SessionManager
     *
     * Provide your own subclass of ConsoleListener to receive updates and indications
     * asynchronously or leave it as its default and use only synchronous methods.
     *
     *@param listener Listener object to receive asynchronous indications.
     *@param settings.rcvObjects Listener wishes to receive managed object data.
     *@param settings.rcvEvents Listener wishes to receive events.
     *@param settings.rcvHeartbeats Listener wishes to receive agent heartbeats.
     *@param settings.userBindings If rcvObjects is true, userBindings allows the
     * console client to control which object classes are received.  See the bindPackage
     * and bindClass methods.  If userBindings is false, the listener will receive
     * updates for all object classes.
     */
    QPID_CONSOLE_EXTERN SessionManager(ConsoleListener* listener = 0,
                                       Settings settings = Settings());

    /** Connect a broker to the console session
     *
     *@param settings Connection settings for client access
     *@return broker object if operation is successful
     * an exception shall be thrown.
     */
    QPID_CONSOLE_EXTERN Broker* addBroker(client::ConnectionSettings& settings);

    /** Disconnect a broker from the console session
     *
     *@param broker The broker object returned from an earlier call to addBroker.
     */
    QPID_CONSOLE_EXTERN void delBroker(Broker* broker);

    /** Get a list of known management packages
     *
     *@param packages Vector of package names returned by the session manager.
     */
    QPID_CONSOLE_EXTERN void getPackages(NameVector& packages);

    /** Get a list of class keys associated with a package
     *
     *@param classKeys List of class keys returned by the session manager.
     *@param packageName Name of package being queried.
     */
    QPID_CONSOLE_EXTERN void getClasses(KeyVector& classKeys,
                                        const std::string& packageName);

    /** Get the schema of a class given its class key
     *
     *@param classKey Class key of the desired schema.
     */
    QPID_CONSOLE_EXTERN SchemaClass& getSchema(const ClassKey& classKey);

    /** Request that updates be received for all classes within a package
     *
     * Note that this method is only meaningful if a ConsoleListener was provided at session
     * creation and if the 'userBindings' flag was set to true.
     *
     *@param packageName Name of the package to which to bind.
     */
    QPID_CONSOLE_EXTERN void bindPackage(const std::string& packageName);

    /** Request update to be received for a particular class
     *
     * Note that this method is only meaningful if a ConsoleListener was provided at session
     * creation and if the 'userBindings' flag was set to true.
     *
     *@param classKey Class key of class to which to bind.
     */
    QPID_CONSOLE_EXTERN void bindClass(const ClassKey& classKey);
    QPID_CONSOLE_EXTERN void bindClass(const std::string& packageName,
                                       const std::string& className);

    /** Get a list of qmf agents known to the session manager.
     *
     *@param agents Vector of Agent objects returned by the session manager.
     *@param broker Return agents registered with this broker only.  If NULL, return agents
     * from all connected brokers.
     */
    QPID_CONSOLE_EXTERN void getAgents(Agent::Vector& agents,
                                       Broker* broker = 0);

    /** Get objects from agents.  There are four variants of this method with different ways of
     * specifying from which class objects are being queried.
     *
     *@param objects List of objects received.
     *@param classKey ClassKey object identifying class to be queried.
     *@param className Class name identifying class to be queried.
     *@param objectId Object Id of the single object to be queried.
     *@param broker Restrict the query to this broker, or all brokers if NULL.
     *@param agent Restrict the query to this agent, or all agents if NULL.
     */
    QPID_CONSOLE_EXTERN void getObjects(Object::Vector& objects,
                                        const std::string& className,
                                        Broker* broker = 0,
                                        Agent* agent = 0);
    //void getObjects(Object::Vector& objects, const ClassKey& classKey,
    //                Broker* broker = 0, Agent* agent = 0);
    //void getObjects(Object::Vector& objects, const ObjectId& objectId,
    //                Broker* broker = 0, Agent* agent = 0);

private:
    friend class Broker;
    friend class Broker::ConnectionThread;
    friend class Object;
    sys::Mutex lock;
    sys::Mutex brokerListLock;
    ConsoleListener* listener;
    std::vector<Broker*> brokers;
    std::map<std::string, Package*> packages;
    SequenceManager sequenceManager;
    sys::Condition cv;
    SequenceManager::set syncSequenceList;
    Object::Vector getResult;
    std::string error;
    Settings settings;
    NameVector bindingKeyList;

    void bindingKeys();
    void allBrokersStable();
    void startProtocol(Broker* broker);
    void handleBrokerResp(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handlePackageInd(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handleCommandComplete(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handleClassInd(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handleMethodResp(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handleHeartbeatInd(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handleEventInd(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handleSchemaResp(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence);
    void handleContentInd(Broker* broker, framing::Buffer& inBuffer, uint32_t sequence, bool prop, bool stat);
    void handleBrokerConnect(Broker* broker);
    void handleBrokerDisconnect(Broker* broker);

};

}} // namespace qpid::console

#endif  /*!_QPID_CONSOLE_SESSION_MANAGER_H*/
