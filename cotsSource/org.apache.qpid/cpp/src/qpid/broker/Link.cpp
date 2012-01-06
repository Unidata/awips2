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

#include "qpid/broker/Link.h"
#include "qpid/broker/LinkRegistry.h"
#include "qpid/broker/Broker.h"
#include "qpid/broker/Connection.h"
#include "qmf/org/apache/qpid/broker/EventBrokerLinkUp.h"
#include "qmf/org/apache/qpid/broker/EventBrokerLinkDown.h"
#include "boost/bind.hpp"
#include "qpid/log/Statement.h"
#include "qpid/framing/enum.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/broker/AclModule.h"

using namespace qpid::broker;
using qpid::framing::Buffer;
using qpid::framing::FieldTable;
using qpid::framing::NotAllowedException;
using qpid::framing::connection::CLOSE_CODE_CONNECTION_FORCED;
using qpid::management::ManagementAgent;
using qpid::management::ManagementObject;
using qpid::management::Manageable;
using qpid::management::Args;
using qpid::sys::Mutex;
using std::stringstream;
namespace _qmf = qmf::org::apache::qpid::broker;

Link::Link(LinkRegistry*  _links,
           MessageStore*  _store,
           string&        _host,
           uint16_t       _port,
           string&        _transport,
           bool           _durable,
           string&        _authMechanism,
           string&        _username,
           string&        _password,
           Broker*        _broker,
           Manageable*    parent)
    : links(_links), store(_store), host(_host), port(_port), 
      transport(_transport), 
      durable(_durable),
      authMechanism(_authMechanism), username(_username), password(_password),
      persistenceId(0), mgmtObject(0), broker(_broker), state(0),
      visitCount(0),
      currentInterval(1),
      closing(false),
      updateUrls(false),
      channelCounter(1),
      connection(0),
      agent(0)
{
    if (parent != 0 && broker != 0)
    {
        agent = broker->getManagementAgent();
        if (agent != 0)
        {
            mgmtObject = new _qmf::Link(agent, this, parent, _host, _port, _transport, _durable);
            if (!durable)
                agent->addObject(mgmtObject);
        }
    }
    setStateLH(STATE_WAITING);
}

Link::~Link ()
{
    if (state == STATE_OPERATIONAL && connection != 0)
        connection->close(CLOSE_CODE_CONNECTION_FORCED, "closed by management");

    if (mgmtObject != 0)
        mgmtObject->resourceDestroy ();
}

void Link::setStateLH (int newState)
{
    if (newState == state)
        return;

    state = newState;
    if (mgmtObject == 0)
        return;

    switch (state)
    {
    case STATE_WAITING     : mgmtObject->set_state("Waiting");     break;
    case STATE_CONNECTING  : mgmtObject->set_state("Connecting");  break;
    case STATE_OPERATIONAL : mgmtObject->set_state("Operational"); break;
    case STATE_FAILED      : mgmtObject->set_state("Failed");      break;
    case STATE_CLOSED      : mgmtObject->set_state("Closed");      break;
    case STATE_PASSIVE     : mgmtObject->set_state("Passive");      break;
    }
}

void Link::startConnectionLH ()
{
    try {
        // Set the state before calling connect.  It is possible that connect
        // will fail synchronously and call Link::closed before returning.
        setStateLH(STATE_CONNECTING);
        broker->connect (host, port, transport,
                         boost::bind (&Link::closed, this, _1, _2));
        QPID_LOG (debug, "Inter-broker link connecting to " << host << ":" << port);
    } catch(std::exception& e) {
        setStateLH(STATE_WAITING);
        if (mgmtObject != 0)
            mgmtObject->set_lastError (e.what());
    }
}

void Link::established ()
{
    stringstream addr;
    addr << host << ":" << port;

    QPID_LOG (info, "Inter-broker link established to " << addr.str());
    agent->raiseEvent(_qmf::EventBrokerLinkUp(addr.str()));
    {
        Mutex::ScopedLock mutex(lock);
        setStateLH(STATE_OPERATIONAL);
        currentInterval = 1;
        visitCount      = 0;
        if (closing)
            destroy();
    }
}

void Link::closed (int, std::string text)
{
    Mutex::ScopedLock mutex(lock);
    QPID_LOG (info, "Inter-broker link disconnected from " << host << ":" << port << " " << text);

    connection = 0;

    if (state == STATE_OPERATIONAL) {
        stringstream addr;
        addr << host << ":" << port;
        QPID_LOG (warning, "Inter-broker link disconnected from " << addr.str());
        agent->raiseEvent(_qmf::EventBrokerLinkDown(addr.str()));
    }

    for (Bridges::iterator i = active.begin(); i != active.end(); i++) {
        (*i)->closed();
        created.push_back(*i);
    }
    active.clear();

    if (state != STATE_FAILED)
    {
        setStateLH(STATE_WAITING);
        if (mgmtObject != 0)
            mgmtObject->set_lastError (text);
    }

    if (closing)
        destroy();
}

void Link::checkClosePermission()
{
    Mutex::ScopedLock mutex(lock);
    
    AclModule* acl = getBroker()->getAcl();
    std::string userID = getUsername() + "@" + getBroker()->getOptions().realm;
    if (acl && !acl->authorise(userID,acl::ACT_DELETE,acl::OBJ_LINK,"")){
        throw NotAllowedException("ACL denied delete link request");
    }
}


void Link::destroy ()
{
    Bridges toDelete;
    {
        Mutex::ScopedLock mutex(lock);

        QPID_LOG (info, "Inter-broker link to " << host << ":" << port << " removed by management");
        if (connection)
            connection->close(CLOSE_CODE_CONNECTION_FORCED, "closed by management");

        setStateLH(STATE_CLOSED);

        // Move the bridges to be deleted into a local vector so there is no
        // corruption of the iterator caused by bridge deletion.
        for (Bridges::iterator i = active.begin(); i != active.end(); i++) {
            (*i)->closed();
            toDelete.push_back(*i);
        }
        active.clear();

        for (Bridges::iterator i = created.begin(); i != created.end(); i++)
            toDelete.push_back(*i);
        created.clear();
    }
    // Now delete all bridges on this link (don't hold the lock for this).
    for (Bridges::iterator i = toDelete.begin(); i != toDelete.end(); i++)
        (*i)->destroy();
    toDelete.clear();
    links->destroy (host, port);
}

void Link::add(Bridge::shared_ptr bridge)
{
    Mutex::ScopedLock mutex(lock);
    created.push_back (bridge);
}

void Link::cancel(Bridge::shared_ptr bridge)
{
    {
        Mutex::ScopedLock mutex(lock);
        
        for (Bridges::iterator i = created.begin(); i != created.end(); i++) {
            if ((*i).get() == bridge.get()) {
                created.erase(i);
                break;
            }
        }
        for (Bridges::iterator i = active.begin(); i != active.end(); i++) {
            if ((*i).get() == bridge.get()) {
                cancellations.push_back(bridge);
                bridge->closed();
                active.erase(i);
                break;
            }
        }
    }
    if (!cancellations.empty()) {
        connection->requestIOProcessing (boost::bind(&Link::ioThreadProcessing, this));
    }
}

void Link::ioThreadProcessing()
{
    Mutex::ScopedLock mutex(lock);

    if (state != STATE_OPERATIONAL)
        return;
    QPID_LOG(debug, "Link::ioThreadProcessing()");

    //process any pending creates and/or cancellations
    if (!created.empty()) {
        for (Bridges::iterator i = created.begin(); i != created.end(); ++i) {
            active.push_back(*i);
            (*i)->create(*connection);
        }
        created.clear();
    }
    if (!cancellations.empty()) {
        for (Bridges::iterator i = cancellations.begin(); i != cancellations.end(); ++i) {
            (*i)->cancel(*connection);
        }
        cancellations.clear();
    }
}

void Link::setConnection(Connection* c)
{
    Mutex::ScopedLock mutex(lock);
    connection = c;
    updateUrls = true;
}

void Link::maintenanceVisit ()
{
    Mutex::ScopedLock mutex(lock);

    if (connection && updateUrls) { 
        urls.reset(connection->getKnownHosts());
        QPID_LOG(debug, "Known hosts for peer of inter-broker link: " << urls);        
        updateUrls = false;
    }

    if (state == STATE_WAITING)
    {
        visitCount++;
        if (visitCount >= currentInterval)
        {
            visitCount = 0;
            //switch host and port to next in url list if possible
            if (!tryFailover()) {
                currentInterval *= 2;
                if (currentInterval > MAX_INTERVAL)
                    currentInterval = MAX_INTERVAL;
                startConnectionLH();
            }
        }
    }
    else if (state == STATE_OPERATIONAL && (!created.empty() || !cancellations.empty()) && connection != 0)
        connection->requestIOProcessing (boost::bind(&Link::ioThreadProcessing, this));
}

void Link::reconnect(const qpid::TcpAddress& a)
{
    Mutex::ScopedLock mutex(lock);
    host = a.host;
    port = a.port;
    startConnectionLH();
    if (mgmtObject != 0) {
        stringstream errorString;
        errorString << "Failed over to " << a;
        mgmtObject->set_lastError(errorString.str());
    }
}

bool Link::tryFailover()
{
    //TODO: urls only work for TCP at present, update when that has changed
    TcpAddress next;
    if (transport == Broker::TCP_TRANSPORT && urls.next(next) && 
        (next.host != host || next.port != port)) {
        links->changeAddress(TcpAddress(host, port), next);
        QPID_LOG(debug, "Link failing over to " << host << ":" << port);
        return true;
    } else {
        return false;
    }
}

uint Link::nextChannel()
{
    Mutex::ScopedLock mutex(lock);

    return channelCounter++;
}

void Link::notifyConnectionForced(const string text)
{
    Mutex::ScopedLock mutex(lock);

    setStateLH(STATE_FAILED);
    if (mgmtObject != 0)
        mgmtObject->set_lastError(text);
}

void Link::setPersistenceId(uint64_t id) const
{
    if (mgmtObject != 0 && persistenceId == 0) {
        ManagementAgent* agent = broker->getManagementAgent();
        agent->addObject(mgmtObject, id);
    }
    persistenceId = id;
}

const string& Link::getName() const
{
    return host;
}

Link::shared_ptr Link::decode(LinkRegistry& links, Buffer& buffer)
{
    string   host;
    uint16_t port;
    string   transport;
    string   authMechanism;
    string   username;
    string   password;
    
    buffer.getShortString(host);
    port = buffer.getShort();
    buffer.getShortString(transport);
    bool durable(buffer.getOctet());
    buffer.getShortString(authMechanism);
    buffer.getShortString(username);
    buffer.getShortString(password);

    return links.declare(host, port, transport, durable, authMechanism, username, password).first;
}

void Link::encode(Buffer& buffer) const 
{
    buffer.putShortString(string("link"));
    buffer.putShortString(host);
    buffer.putShort(port);
    buffer.putShortString(transport);
    buffer.putOctet(durable ? 1 : 0);
    buffer.putShortString(authMechanism);
    buffer.putShortString(username);
    buffer.putShortString(password);
}

uint32_t Link::encodedSize() const 
{ 
    return host.size() + 1 // short-string (host)
        + 5                // short-string ("link")
        + 2                // port
        + transport.size() + 1 // short-string(transport)
        + 1                // durable
        + authMechanism.size() + 1
        + username.size() + 1
        + password.size() + 1;
}

ManagementObject* Link::GetManagementObject (void) const
{
    return (ManagementObject*) mgmtObject;
}

Manageable::status_t Link::ManagementMethod (uint32_t op, Args& args, string& text)
{
    switch (op)
    {
    case _qmf::Link::METHOD_CLOSE :
        checkClosePermission();
        if (!closing) {
	    closing = true;
	    if (state != STATE_CONNECTING && connection) {
                //connection can only be closed on the connections own IO processing thread
                connection->requestIOProcessing(boost::bind(&Link::destroy, this));
	    }
        }
        return Manageable::STATUS_OK;

    case _qmf::Link::METHOD_BRIDGE :
        _qmf::ArgsLinkBridge& iargs = (_qmf::ArgsLinkBridge&) args;
        QPID_LOG(debug, "Link::bridge() request received");

        // Durable bridges are only valid on durable links
        if (iargs.i_durable && !durable) {
            text = "Can't create a durable route on a non-durable link";
            return Manageable::STATUS_USER;
        }

        if (iargs.i_dynamic) {
            Exchange::shared_ptr exchange = getBroker()->getExchanges().get(iargs.i_src);
            if (exchange.get() == 0) {
                text = "Exchange not found";
                return Manageable::STATUS_USER;
            }
            if (!exchange->supportsDynamicBinding()) {
                text = "Exchange type does not support dynamic routing";
                return Manageable::STATUS_USER;
            }
        }

        std::pair<Bridge::shared_ptr, bool> result =
            links->declare (host, port, iargs.i_durable, iargs.i_src,
                            iargs.i_dest, iargs.i_key, iargs.i_srcIsQueue,
                            iargs.i_srcIsLocal, iargs.i_tag, iargs.i_excludes,
                            iargs.i_dynamic, iargs.i_sync);

        if (result.second && iargs.i_durable)
            store->create(*result.first);

        return Manageable::STATUS_OK;
    }

    return Manageable::STATUS_UNKNOWN_METHOD;
}

void Link::setPassive(bool passive)
{
    Mutex::ScopedLock mutex(lock);
    if (passive) {
        setStateLH(STATE_PASSIVE);
    } else {
        if (state == STATE_PASSIVE) {
            setStateLH(STATE_WAITING);
        } else {
            QPID_LOG(warning, "Ignoring attempt to activate non-passive link");
        }
    }
}
