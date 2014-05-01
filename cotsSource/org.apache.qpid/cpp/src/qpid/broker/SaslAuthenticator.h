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
#ifndef _SaslAuthenticator_
#define _SaslAuthenticator_

#include "qpid/framing/amqp_types.h"
#include "qpid/framing/AMQP_ClientProxy.h"
#include "qpid/Exception.h"
#include "qpid/sys/SecurityLayer.h"
#include <memory>

namespace qpid {
namespace broker {

class Connection;

class SaslAuthenticator
{
public:
    virtual ~SaslAuthenticator() {}
    virtual void getMechanisms(framing::Array& mechanisms) = 0;
    virtual void start(const std::string& mechanism, const std::string& response) = 0;
    virtual void step(const std::string& response) = 0;
    virtual void getUid(std::string&) {}
    virtual void getError(std::string&) {}
    virtual std::auto_ptr<qpid::sys::SecurityLayer> getSecurityLayer(uint16_t maxFrameSize) = 0;

    static bool available(void);

    // Initialize the SASL mechanism; throw if it fails.
    static void init(const std::string& saslName);
    static void fini(void);

    static std::auto_ptr<SaslAuthenticator> createAuthenticator(Connection& connection);
};

}}

#endif
