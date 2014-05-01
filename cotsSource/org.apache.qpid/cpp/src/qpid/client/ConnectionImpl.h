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

#ifndef _ConnectionImpl_
#define _ConnectionImpl_

#include "qpid/client/Bounds.h"
#include "qpid/client/ConnectionHandler.h"

#include "qpid/framing/FrameHandler.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/ShutdownHandler.h"
#include "qpid/sys/TimeoutHandler.h"

#include <map>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

namespace qpid {
namespace client {

class Connector;
struct ConnectionSettings;
class SessionImpl;

class ConnectionImpl : public Bounds,
                       public framing::FrameHandler,
                       public sys::TimeoutHandler, 
                       public sys::ShutdownHandler,
                       public boost::enable_shared_from_this<ConnectionImpl>

{
    typedef std::map<uint16_t, boost::weak_ptr<SessionImpl> > SessionMap;

    static const uint16_t NEXT_CHANNEL;

    SessionMap sessions; 
    ConnectionHandler handler;
    boost::scoped_ptr<Connector> connector;
    framing::ProtocolVersion version;
    uint16_t nextChannel;
    sys::Mutex lock;

    boost::intrusive_ptr<qpid::sys::TimerTask> heartbeatTask;

    template <class F> void closeInternal(const F&);

    void incoming(framing::AMQFrame& frame);    
    void closed(uint16_t, const std::string&);
    void idleOut();
    void idleIn();
    void shutdown();

    boost::function<void ()> failureCallback;

  public:
    ConnectionImpl(framing::ProtocolVersion version, const ConnectionSettings& settings);
    ~ConnectionImpl();
    
    void open();
    bool isOpen() const;

    boost::shared_ptr<SessionImpl> newSession(const std::string& name, uint32_t timeout, uint16_t channel=NEXT_CHANNEL);
    void addSession(const boost::shared_ptr<SessionImpl>&, uint16_t channel=NEXT_CHANNEL);
        
    void close();
    void handle(framing::AMQFrame& frame);
    void erase(uint16_t channel);
    const ConnectionSettings& getNegotiatedSettings();

    std::vector<Url> getInitialBrokers();
    void registerFailureCallback ( boost::function<void ()> fn ) { failureCallback = fn; }

    framing::ProtocolVersion getVersion() { return version; }
};

}}


#endif
