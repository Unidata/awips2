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
#ifndef _ConnectionState_
#define _ConnectionState_

#include <vector>

#include "qpid/sys/AggregateOutput.h"
#include "qpid/sys/ConnectionOutputHandlerPtr.h"
#include "qpid/framing/ProtocolVersion.h"
#include "qpid/management/Manageable.h"
#include "qpid/Url.h"
#include "qpid/broker/Broker.h"

namespace qpid {
namespace broker {

class ConnectionState : public ConnectionToken, public management::Manageable
{
  protected:
    sys::ConnectionOutputHandlerPtr out;

  public:
    ConnectionState(qpid::sys::ConnectionOutputHandler* o, Broker& b) :
        out(o),
        broker(b),
        outputTasks(out),
        framemax(65535),
        heartbeat(0),
        heartbeatmax(120),
        stagingThreshold(broker.getStagingThreshold()),
        federationLink(true),
        clientSupportsThrottling(false),
        clusterOrderOut(0)
    {}

    virtual ~ConnectionState () {}

    uint32_t getFrameMax() const { return framemax; }
    uint16_t getHeartbeat() const { return heartbeat; }
    uint16_t getHeartbeatMax() const { return heartbeatmax; }
    uint64_t getStagingThreshold() const { return stagingThreshold; }

    void setFrameMax(uint32_t fm) { framemax = std::max(fm, (uint32_t) 4096); }
    void setHeartbeat(uint16_t hb) { heartbeat = hb; }
    void setHeartbeatMax(uint16_t hbm) { heartbeatmax = hbm; }
    void setStagingThreshold(uint64_t st) { stagingThreshold = st; }

    virtual void setUserId(const string& uid) {  userId = uid; }
    const string& getUserId() const { return userId; }

    void setUrl(const string& _url) { url = _url; }
    const string& getUrl() const { return url; }

    void setFederationLink(bool b) {  federationLink = b; }
    bool isFederationLink() const { return federationLink; }
    void setFederationPeerTag(const string& tag) { federationPeerTag = string(tag); }
    const string& getFederationPeerTag() const { return federationPeerTag; }
    std::vector<Url>& getKnownHosts() { return knownHosts; }
    
    void setClientThrottling(bool set=true) { clientSupportsThrottling = set; }
    bool getClientThrottling() const { return clientSupportsThrottling; }

    Broker& getBroker() { return broker; }

    Broker& broker;
    std::vector<Queue::shared_ptr> exclusiveQueues;

    //contained output tasks
    sys::AggregateOutput outputTasks;

    sys::ConnectionOutputHandler& getOutput() { return out; }
    framing::ProtocolVersion getVersion() const { return version; }
    void setOutputHandler(qpid::sys::ConnectionOutputHandler* o) { out.set(o); }

    /**
     * If the broker is part of a cluster, this is a handler provided
     * by cluster code. It ensures consistent ordering of commands
     * that are sent based on criteria that are not predictably
     * ordered cluster-wide, e.g. a timer firing.
     */
    framing::FrameHandler* getClusterOrderOutput() { return clusterOrderOut; }
    void setClusterOrderOutput(framing::FrameHandler& fh) { clusterOrderOut = &fh; }

    virtual void requestIOProcessing (boost::function0<void>) = 0;

  protected:
    framing::ProtocolVersion version;
    uint32_t framemax;
    uint16_t heartbeat;
    uint16_t heartbeatmax;
    uint64_t stagingThreshold;
    string userId;
    string url;
    bool federationLink;
    string federationPeerTag;
    std::vector<Url> knownHosts;
    bool clientSupportsThrottling;
    framing::FrameHandler* clusterOrderOut;
};

}}

#endif
