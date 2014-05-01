#ifndef QPID_BROKER_SESSIONHANDLER_H
#define QPID_BROKER_SESSIONHANDLER_H

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

#include "qpid/amqp_0_10/SessionHandler.h"
#include "qpid/framing/AMQP_ClientProxy.h"

namespace qpid {
class SessionState;

namespace broker {

class Connection;
class ConnectionState;
class SessionState;

/**
 * A SessionHandler is associated with each active channel. It
 * receives incoming frames, handles session controls and manages the
 * association between the channel and a session.
 */
class SessionHandler : public amqp_0_10::SessionHandler {
  public:
    SessionHandler(Connection&, framing::ChannelId);
    ~SessionHandler();

    /** Get broker::SessionState */
    SessionState* getSession() { return session.get(); }
    const SessionState* getSession() const { return session.get(); }

    ConnectionState& getConnection();
    const ConnectionState& getConnection() const;

    framing::AMQP_ClientProxy& getProxy() { return proxy; }
    const framing::AMQP_ClientProxy& getProxy() const { return proxy; }

    /**
     * If commands are sent based on the local time (e.g. in timers), they don't have
     * a well-defined ordering across cluster nodes.
     * This proxy is for sending such commands. In a clustered broker it will take steps
     * to synchronize command order across the cluster. In a stand-alone broker
     * it is just a synonym for getProxy()
     */  
    framing::AMQP_ClientProxy& getClusterOrderProxy() {
        return clusterOrderProxy.get() ? *clusterOrderProxy : proxy;
    }

    virtual void handleDetach();
    void attached(const std::string& name);//used by 'pushing' inter-broker bridges
    void attachAs(const std::string& name);//used by 'pulling' inter-broker bridges

  protected:
    virtual void setState(const std::string& sessionName, bool force);
    virtual qpid::SessionState* getState();
    virtual framing::FrameHandler* getInHandler();
    virtual void connectionException(framing::connection::CloseCode code, const std::string& msg);
    virtual void channelException(framing::session::DetachCode, const std::string& msg);
    virtual void executionException(framing::execution::ErrorCode, const std::string& msg);
    virtual void detaching();
    virtual void readyToSend();

  private:
    struct SetChannelProxy : public framing::AMQP_ClientProxy { // Proxy that sets the channel.
        framing::ChannelHandler setChannel;
        SetChannelProxy(uint16_t ch, framing::FrameHandler* out)
            : framing::AMQP_ClientProxy(setChannel), setChannel(ch, out) {}
    };

    Connection& connection;
    framing::AMQP_ClientProxy proxy;
    std::auto_ptr<SessionState> session;
    std::auto_ptr<SetChannelProxy> clusterOrderProxy;
};

}} // namespace qpid::broker



#endif  /*!QPID_BROKER_SESSIONHANDLER_H*/
