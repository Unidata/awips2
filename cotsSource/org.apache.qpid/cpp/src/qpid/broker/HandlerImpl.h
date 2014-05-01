#ifndef _broker_HandlerImpl_h
#define _broker_HandlerImpl_h

/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/broker/SemanticState.h"
#include "qpid/broker/SessionContext.h"
#include "qpid/broker/ConnectionState.h"

namespace qpid {
namespace broker {

class Broker;

/**
 * Base template for protocol handler implementations.
 * Provides convenience methods for getting common session objects.
 */
class HandlerImpl {
  protected:
    SemanticState& state;
    SessionContext& session;

    HandlerImpl(SemanticState& s) : state(s), session(s.getSession()) {}

    framing::AMQP_ClientProxy& getProxy() { return session.getProxy(); }
    ConnectionState& getConnection() { return session.getConnection(); }
    Broker& getBroker() { return session.getConnection().getBroker(); }
};

}} // namespace qpid::broker



#endif  /*!_broker_HandlerImpl_h*/


