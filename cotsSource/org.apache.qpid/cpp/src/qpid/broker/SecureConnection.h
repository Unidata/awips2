#ifndef QPID_BROKER_SECURECONNECTION_H
#define QPID_BROKER_SECURECONNECTION_H

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

#include "qpid/sys/ConnectionCodec.h"
#include <memory>

namespace qpid {

namespace sys {
class SecurityLayer;
}

namespace broker {

/**
 * A ConnectionCodec 'wrapper' that allows a connection to be
 * 'secured' e.g. encrypted based on settings negotiatiated at the
 * time of establishment.
 */
class SecureConnection : public qpid::sys::ConnectionCodec
{
  public:
    SecureConnection();
    size_t decode(const char* buffer, size_t size);
    size_t encode(const char* buffer, size_t size);
    bool canEncode();
    void closed();
    bool isClosed() const;
    framing::ProtocolVersion getVersion() const;
    void setCodec(std::auto_ptr<ConnectionCodec>);
    void activateSecurityLayer(std::auto_ptr<qpid::sys::SecurityLayer>);
  private:
    std::auto_ptr<ConnectionCodec> codec;
    std::auto_ptr<qpid::sys::SecurityLayer> securityLayer;
    bool secured;
};
}} // namespace qpid::broker

#endif  /*!QPID_BROKER_SECURECONNECTION_H*/
