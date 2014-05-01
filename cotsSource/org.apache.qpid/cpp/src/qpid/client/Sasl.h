#ifndef QPID_CLIENT_SASL_H
#define QPID_CLIENT_SASL_H

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

#include <memory>
#include <string>
#include "qpid/sys/IntegerTypes.h"

namespace qpid {

namespace sys {
class SecurityLayer;
}

namespace client {

struct ConnectionSettings;

/**
 * Interface to SASL support. This class is implemented by platform-specific
 * SASL providers.
 */
class Sasl
{
  public:
    /**
     * Start SASL negotiation with the broker.
     *
     * @param mechanisms Comma-separated list of the SASL mechanism the
     *             client supports.
     * @param ssf  Security Strength Factor (SSF). SSF is used to negotiate
     *             a SASL security layer on top of the connection should both
     *             parties require and support it. The value indicates the
     *             required level of security for communication. Possible
     *             values are:
     *             @li 0  No security
     *             @li 1  Integrity checking only
     *             @li >1 Integrity and confidentiality with the number
     *                    giving the encryption key length.
     */
    virtual std::string start(const std::string& mechanisms, unsigned int ssf) = 0;
    virtual std::string step(const std::string& challenge) = 0;
    virtual std::string getMechanism() = 0;
    virtual std::string getUserId() = 0;
    virtual std::auto_ptr<qpid::sys::SecurityLayer> getSecurityLayer(uint16_t maxFrameSize) = 0;    
    virtual ~Sasl() {}
};
}} // namespace qpid::client

#endif  /*!QPID_CLIENT_SASL_H*/
