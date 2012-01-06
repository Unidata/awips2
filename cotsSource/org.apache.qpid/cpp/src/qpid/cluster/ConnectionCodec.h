#ifndef QPID_CLUSTER_CONNCTIONCODEC_H
#define QPID_CLUSTER_CONNCTIONCODEC_H

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

#include "qpid/amqp_0_10/Connection.h"
#include "qpid/cluster/Connection.h"
#include <boost/shared_ptr.hpp>
#include <boost/intrusive_ptr.hpp>

namespace qpid {

namespace broker {
class Connection;
}

namespace cluster {
class Cluster;

/**
 * Encapsulates the standard amqp_0_10::ConnectionCodec and sets up
 * a cluster::Connection for the connection.
 *
 * The ConnectionCodec is deleted by the network layer when the
 * connection closes. The cluster::Connection needs to be kept
 * around until all cluster business on the connection is complete.
 *
 */
class ConnectionCodec : public sys::ConnectionCodec {
  public:
    struct Factory : public sys::ConnectionCodec::Factory {
        boost::shared_ptr<sys::ConnectionCodec::Factory> next;
        Cluster& cluster;
        Factory(boost::shared_ptr<sys::ConnectionCodec::Factory> f, Cluster& c)
            : next(f), cluster(c) {}
        sys::ConnectionCodec* create(framing::ProtocolVersion, sys::OutputControl&, const std::string& id,
                                     unsigned int conn_ssf);
        sys::ConnectionCodec* create(sys::OutputControl&, const std::string& id,
                                     unsigned int conn_ssf);
    };

    ConnectionCodec(const framing::ProtocolVersion&, sys::OutputControl& out,
                    const std::string& logId, Cluster& c, bool catchUp, bool isLink,
                    unsigned int ssf);
    ~ConnectionCodec();

    // ConnectionCodec functions.
    size_t decode(const char* buffer, size_t size);
    size_t encode(const char* buffer, size_t size);
    bool canEncode();
    void closed();
    bool isClosed() const;
    framing::ProtocolVersion getVersion() const;
    

  private:
    amqp_0_10::Connection codec;
    boost::intrusive_ptr<cluster::Connection> interceptor;
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_CONNCTIONCODEC_H*/
