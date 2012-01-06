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
#ifndef _Bridge_
#define _Bridge_

#include "qpid/broker/PersistableConfig.h"
#include "qpid/framing/AMQP_ServerProxy.h"
#include "qpid/framing/ChannelHandler.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/FrameHandler.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/management/Manageable.h"
#include "qpid/broker/Exchange.h"
#include "qmf/org/apache/qpid/broker/ArgsLinkBridge.h"
#include "qmf/org/apache/qpid/broker/Bridge.h"

#include <boost/function.hpp>
#include <memory>

namespace qpid {
namespace broker {

class Connection;
class ConnectionState;
class Link;
class LinkRegistry;

class Bridge : public PersistableConfig, public management::Manageable, public Exchange::DynamicBridge
{
public:
    typedef boost::shared_ptr<Bridge> shared_ptr;
    typedef boost::function<void(Bridge*)> CancellationListener;

    Bridge(Link* link, framing::ChannelId id, CancellationListener l,
           const qmf::org::apache::qpid::broker::ArgsLinkBridge& args);
    ~Bridge();

    void create(Connection& c);
    void cancel(Connection& c);
    void closed();
    void destroy();
    bool isDurable() { return args.i_durable; }

    management::ManagementObject* GetManagementObject() const;
    management::Manageable::status_t ManagementMethod(uint32_t methodId,
                                                      management::Args& args,
                                                      std::string& text);

    // PersistableConfig:
    void     setPersistenceId(uint64_t id) const;
    uint64_t getPersistenceId() const { return persistenceId; }
    uint32_t encodedSize() const;
    void     encode(framing::Buffer& buffer) const; 
    const std::string& getName() const;
    static Bridge::shared_ptr decode(LinkRegistry& links, framing::Buffer& buffer);

    // Exchange::DynamicBridge methods
    void propagateBinding(const std::string& key, const std::string& tagList, const std::string& op, const std::string& origin);
    void sendReorigin();
    void ioThreadPropagateBinding(const string& queue, const string& exchange, const string& key, framing::FieldTable args);
    bool containsLocalTag(const std::string& tagList) const;
    const std::string& getLocalTag() const;

private:
    struct PushHandler : framing::FrameHandler {
        PushHandler(Connection* c) { conn = c; }
        void handle(framing::AMQFrame& frame);
        Connection* conn;
    };

    std::auto_ptr<PushHandler>                        pushHandler;
    std::auto_ptr<framing::ChannelHandler>            channelHandler;
    std::auto_ptr<framing::AMQP_ServerProxy::Session> session;
    std::auto_ptr<framing::AMQP_ServerProxy>          peer;

    Link* link;
    framing::ChannelId          id;
    qmf::org::apache::qpid::broker::ArgsLinkBridge args;
    qmf::org::apache::qpid::broker::Bridge*        mgmtObject;
    CancellationListener        listener;
    std::string name;
    std::string queueName;
    mutable uint64_t  persistenceId;
    ConnectionState* connState;
    Connection* conn;
};


}}

#endif
