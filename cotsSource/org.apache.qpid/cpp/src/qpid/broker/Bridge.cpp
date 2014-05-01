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
#include "qpid/broker/Bridge.h"
#include "qpid/broker/ConnectionState.h"
#include "qpid/broker/Connection.h"
#include "qpid/broker/Link.h"
#include "qpid/broker/LinkRegistry.h"
#include "qpid/broker/SessionState.h"

#include "qpid/management/ManagementAgent.h"
#include "qpid/framing/Uuid.h"
#include "qpid/log/Statement.h"
#include <iostream>

using qpid::framing::FieldTable;
using qpid::framing::Uuid;
using qpid::framing::Buffer;
using qpid::management::ManagementAgent;
namespace _qmf = qmf::org::apache::qpid::broker;

namespace 
{
const std::string qpidFedOp("qpid.fed.op");
const std::string qpidFedTags("qpid.fed.tags");
const std::string qpidFedOrigin("qpid.fed.origin");

const std::string fedOpBind("B");
const std::string fedOpUnbind("U");
const std::string fedOpReorigin("R");
const std::string fedOpHello("H");
}

namespace qpid {
namespace broker {

void Bridge::PushHandler::handle(framing::AMQFrame& frame)
{
    conn->received(frame);
}

Bridge::Bridge(Link* _link, framing::ChannelId _id, CancellationListener l,
               const _qmf::ArgsLinkBridge& _args) : 
    link(_link), id(_id), args(_args), mgmtObject(0),
    listener(l), name(Uuid(true).str()), queueName("bridge_queue_"), persistenceId(0)
{
    std::stringstream title;
    title << id << "_" << link->getBroker()->getFederationTag();
    queueName += title.str();
    ManagementAgent* agent = link->getBroker()->getManagementAgent();
    if (agent != 0) {
        mgmtObject = new _qmf::Bridge
            (agent, this, link, id, args.i_durable, args.i_src, args.i_dest,
             args.i_key, args.i_srcIsQueue, args.i_srcIsLocal,
             args.i_tag, args.i_excludes, args.i_dynamic, args.i_sync);
        if (!args.i_durable)
            agent->addObject(mgmtObject);
    }
    QPID_LOG(debug, "Bridge created from " << args.i_src << " to " << args.i_dest);
}

Bridge::~Bridge() 
{
    mgmtObject->resourceDestroy(); 
}

void Bridge::create(Connection& c)
{
    connState = &c;
    conn = &c;
    FieldTable options;
    if (args.i_sync) options.setInt("qpid.sync_frequency", args.i_sync);
    SessionHandler& sessionHandler = c.getChannel(id);
    if (args.i_srcIsLocal) {
        if (args.i_dynamic)
            throw Exception("Dynamic routing not supported for push routes");
        // Point the bridging commands at the local connection handler
        pushHandler.reset(new PushHandler(&c));
        channelHandler.reset(new framing::ChannelHandler(id, pushHandler.get()));

        session.reset(new framing::AMQP_ServerProxy::Session(*channelHandler));
        peer.reset(new framing::AMQP_ServerProxy(*channelHandler));
        
        session->attach(name, false);
        session->commandPoint(0,0);
    } else {
        sessionHandler.attachAs(name);
        // Point the bridging commands at the remote peer broker
        peer.reset(new framing::AMQP_ServerProxy(sessionHandler.out));
    }

    if (args.i_srcIsLocal) sessionHandler.getSession()->disableReceiverTracking();
    if (args.i_srcIsQueue) {        
        peer->getMessage().subscribe(args.i_src, args.i_dest, args.i_sync ? 0 : 1, 0, false, "", 0, options);
        peer->getMessage().flow(args.i_dest, 0, 0xFFFFFFFF);
        peer->getMessage().flow(args.i_dest, 1, 0xFFFFFFFF);
        QPID_LOG(debug, "Activated route from queue " << args.i_src << " to " << args.i_dest);
    } else {
        FieldTable queueSettings;

        if (args.i_tag.size()) {
            queueSettings.setString("qpid.trace.id", args.i_tag);
        } else {
            const string& peerTag = c.getFederationPeerTag();
            if (peerTag.size())
                queueSettings.setString("qpid.trace.id", peerTag);
        }

        if (args.i_excludes.size()) {
            queueSettings.setString("qpid.trace.exclude", args.i_excludes);
        } else {
            const string& localTag = link->getBroker()->getFederationTag();
            if (localTag.size())
                queueSettings.setString("qpid.trace.exclude", localTag);
        }

        bool durable = false;//should this be an arg, or would we use srcIsQueue for durable queues?
        bool autoDelete = !durable;//auto delete transient queues?
        peer->getQueue().declare(queueName, "", false, durable, true, autoDelete, queueSettings);
        if (!args.i_dynamic)
            peer->getExchange().bind(queueName, args.i_src, args.i_key, FieldTable());
        peer->getMessage().subscribe(queueName, args.i_dest, 1, 0, false, "", 0, FieldTable());
        peer->getMessage().flow(args.i_dest, 0, 0xFFFFFFFF);
        peer->getMessage().flow(args.i_dest, 1, 0xFFFFFFFF);

        if (args.i_dynamic) {
            Exchange::shared_ptr exchange = link->getBroker()->getExchanges().get(args.i_src);
            if (exchange.get() == 0)
                throw Exception("Exchange not found for dynamic route");
            exchange->registerDynamicBridge(this);
            QPID_LOG(debug, "Activated dynamic route for exchange " << args.i_src);
        } else {
            QPID_LOG(debug, "Activated static route from exchange " << args.i_src << " to " << args.i_dest);
        }
    }
    if (args.i_srcIsLocal) sessionHandler.getSession()->enableReceiverTracking();
}

void Bridge::cancel(Connection& c)
{
    if (args.i_srcIsLocal) {    
        //recreate peer to be sure that the session handler reference
        //is valid (it could have been deleted due to a detach)
        SessionHandler& sessionHandler = c.getChannel(id);
        peer.reset(new framing::AMQP_ServerProxy(sessionHandler.out));
    }
    peer->getMessage().cancel(args.i_dest);
    peer->getSession().detach(name);
}

void Bridge::closed()
{
    if (args.i_dynamic) {
        Exchange::shared_ptr exchange = link->getBroker()->getExchanges().get(args.i_src);
        if (exchange.get() != 0)
            exchange->removeDynamicBridge(this);
    }
}

void Bridge::destroy()
{
    listener(this);
}

void Bridge::setPersistenceId(uint64_t pId) const
{
    if (mgmtObject != 0 && persistenceId == 0) {
        ManagementAgent* agent = link->getBroker()->getManagementAgent();
        agent->addObject (mgmtObject, pId);
    }
    persistenceId = pId;
}

const string& Bridge::getName() const
{
    return name;
}

Bridge::shared_ptr Bridge::decode(LinkRegistry& links, Buffer& buffer)
{
    string   host;
    uint16_t port;
    string   src;
    string   dest;
    string   key;
    string   id;
    string   excludes;

    buffer.getShortString(host);
    port = buffer.getShort();
    bool durable(buffer.getOctet());
    buffer.getShortString(src);
    buffer.getShortString(dest);
    buffer.getShortString(key);
    bool is_queue(buffer.getOctet());
    bool is_local(buffer.getOctet());
    buffer.getShortString(id);
    buffer.getShortString(excludes);
    bool dynamic(buffer.getOctet());
    uint16_t sync = buffer.getShort();

    return links.declare(host, port, durable, src, dest, key,
                         is_queue, is_local, id, excludes, dynamic, sync).first;
}

void Bridge::encode(Buffer& buffer) const 
{
    buffer.putShortString(string("bridge"));
    buffer.putShortString(link->getHost());
    buffer.putShort(link->getPort());
    buffer.putOctet(args.i_durable ? 1 : 0);
    buffer.putShortString(args.i_src);
    buffer.putShortString(args.i_dest);
    buffer.putShortString(args.i_key);
    buffer.putOctet(args.i_srcIsQueue ? 1 : 0);
    buffer.putOctet(args.i_srcIsLocal ? 1 : 0);
    buffer.putShortString(args.i_tag);
    buffer.putShortString(args.i_excludes);
    buffer.putOctet(args.i_dynamic ? 1 : 0);
    buffer.putShort(args.i_sync);
}

uint32_t Bridge::encodedSize() const 
{ 
    return link->getHost().size() + 1 // short-string (host)
        + 7                // short-string ("bridge")
        + 2                // port
        + 1                // durable
        + args.i_src.size()  + 1
        + args.i_dest.size() + 1
        + args.i_key.size()  + 1
        + 1                // srcIsQueue
        + 1                // srcIsLocal
        + args.i_tag.size() + 1
        + args.i_excludes.size() + 1
        + 1               // dynamic
        + 2;              // sync
}

management::ManagementObject* Bridge::GetManagementObject (void) const
{
    return (management::ManagementObject*) mgmtObject;
}

management::Manageable::status_t Bridge::ManagementMethod(uint32_t methodId,
                                                          management::Args& /*args*/,
                                                          string&)
{
    if (methodId == _qmf::Bridge::METHOD_CLOSE) {  
        //notify that we are closed
        destroy();
        return management::Manageable::STATUS_OK;
    } else {
        return management::Manageable::STATUS_UNKNOWN_METHOD;
    }
}

void Bridge::propagateBinding(const string& key, const string& tagList,
                              const string& op,  const string& origin)
{
    const string& localTag = link->getBroker()->getFederationTag();
    const string& peerTag  = connState->getFederationPeerTag();

    if (tagList.find(peerTag) == tagList.npos) {
         FieldTable bindArgs;
         string newTagList(tagList + string(tagList.empty() ? "" : ",") + localTag);

         bindArgs.setString(qpidFedOp, op);
         bindArgs.setString(qpidFedTags, newTagList);
         if (origin.empty())
             bindArgs.setString(qpidFedOrigin, localTag);
         else
             bindArgs.setString(qpidFedOrigin, origin);

         conn->requestIOProcessing(boost::bind(&Bridge::ioThreadPropagateBinding, this,
                                               queueName, args.i_src, key, bindArgs));
    }
}

void Bridge::sendReorigin()
{
    FieldTable bindArgs;

    bindArgs.setString(qpidFedOp, fedOpReorigin);
    bindArgs.setString(qpidFedTags, link->getBroker()->getFederationTag());

    conn->requestIOProcessing(boost::bind(&Bridge::ioThreadPropagateBinding, this,
                                          queueName, args.i_src, args.i_key, bindArgs));
}

void Bridge::ioThreadPropagateBinding(const string& queue, const string& exchange, const string& key, FieldTable args)
{
    peer->getExchange().bind(queue, exchange, key, args);
}

bool Bridge::containsLocalTag(const string& tagList) const
{
    const string& localTag = link->getBroker()->getFederationTag();
    return (tagList.find(localTag) != tagList.npos);
}

const string& Bridge::getLocalTag() const
{
    return link->getBroker()->getFederationTag();
}

}}
