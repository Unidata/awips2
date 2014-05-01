#ifndef QPID_CLUSTER_PROXYINPUTHANDLER_H
#define QPID_CLUSTER_PROXYINPUTHANDLER_H

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

#include "qpid/sys/ConnectionInputHandler.h"
#include <boost/intrusive_ptr.hpp>

namespace qpid {

namespace framing { class AMQFrame; }

namespace cluster {

/**
 * Proxies ConnectionInputHandler functions and ensures target.closed() 
 * is called, on deletion if not before.
 */
class ProxyInputHandler : public sys::ConnectionInputHandler
{
  public:
    ProxyInputHandler(boost::intrusive_ptr<cluster::Connection> t) : target(t) {}
    ~ProxyInputHandler() { closed(); }
    
    void received(framing::AMQFrame& f) { target->received(f); }
    void closed() { if (target) target->closed(); target = 0; }
    void idleOut() { target->idleOut(); }
    void idleIn() { target->idleIn(); }
    bool doOutput() { return target->doOutput(); }
    bool hasOutput() { return target->hasOutput(); }
    
  private:
    boost::intrusive_ptr<cluster::Connection> target;
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_PROXYINPUTHANDLER_H*/
