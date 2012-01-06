#ifndef QPID_BROKER_SESSIONCONTEXT_H
#define QPID_BROKER_SESSIONCONTEXT_H

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

#include "qpid/framing/FrameHandler.h"
#include "qpid/framing/AMQP_ClientProxy.h"
#include "qpid/framing/amqp_types.h"
#include "qpid/sys/OutputControl.h"
#include "qpid/broker/ConnectionState.h"
#include "qpid/broker/OwnershipToken.h"
#include "qpid/SessionId.h"

#include <boost/noncopyable.hpp>

namespace qpid {
namespace broker {

class SessionContext : public OwnershipToken, public sys::OutputControl
{
  public:
    virtual ~SessionContext(){}
    virtual bool isLocal(const ConnectionToken* t) const = 0;
    virtual bool isAttached() const = 0;
    virtual ConnectionState& getConnection() = 0;
    virtual framing::AMQP_ClientProxy& getProxy() = 0;
    virtual Broker& getBroker() = 0;
    virtual uint16_t getChannel() const = 0;
    virtual const SessionId& getSessionId() const = 0;
};

}} // namespace qpid::broker



#endif  /*!QPID_BROKER_SESSIONCONTEXT_H*/
