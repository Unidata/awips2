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
#include "qpid/broker/SecureConnection.h"
#include "qpid/sys/SecurityLayer.h"
#include "qpid/framing/reply_exceptions.h"

namespace qpid {
namespace broker {

using qpid::sys::SecurityLayer;

SecureConnection::SecureConnection() : secured(false) {}

size_t SecureConnection::decode(const char* buffer, size_t size)
{
    if (!secured && securityLayer.get()) {
        //security layer comes into effect on first read after its
        //activated
        secured = true;
    }
    if (secured) {
        return securityLayer->decode(buffer, size);
    } else {
        return codec->decode(buffer, size);
    }
}

size_t SecureConnection::encode(const char* buffer, size_t size)
{
    if (secured) {
        return securityLayer->encode(buffer, size);
    } else {
        return codec->encode(buffer, size);
    }
}

bool SecureConnection::canEncode()
{
    if (secured) return securityLayer->canEncode();
    else return codec->canEncode();
}

void SecureConnection::closed()
{
    codec->closed();
}

bool SecureConnection::isClosed() const
{
    return codec->isClosed();
}

framing::ProtocolVersion SecureConnection::getVersion() const
{
    return codec->getVersion();
}

void SecureConnection:: setCodec(std::auto_ptr<ConnectionCodec> c)
{
    codec = c;
}

void SecureConnection::activateSecurityLayer(std::auto_ptr<SecurityLayer> sl)
{
    securityLayer = sl;
    securityLayer->init(codec.get());
}

}} // namespace qpid::broker
