#ifndef QPID_SYS_SSL_SSLHANDLER_H
#define QPID_SYS_SSL_SSLHANDLER_H

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
#include "qpid/sys/OutputControl.h"

namespace qpid {

namespace framing {
    class ProtocolInitiation;
}

namespace sys {
namespace ssl {

class SslIO;
class SslIOBufferBase;
class SslSocket;

class SslHandler : public OutputControl {
    std::string identifier;
    SslIO* aio;
    ConnectionCodec::Factory* factory;
    ConnectionCodec* codec;
    bool readError;
    bool isClient;

    void write(const framing::ProtocolInitiation&);

  public:
    SslHandler(std::string id, ConnectionCodec::Factory* f);
    ~SslHandler();
    void init(SslIO* a, int numBuffs);

    void setClient() { isClient = true; }

    // Output side
    void abort();
    void activateOutput();
    void giveReadCredit(int32_t);

    // Input side
    void readbuff(SslIO& aio, SslIOBufferBase* buff);
    void eof(SslIO& aio);
    void disconnect(SslIO& aio);

    // Notifications
    void nobuffs(SslIO& aio);
    void idle(SslIO& aio);
    void closedSocket(SslIO& aio, const SslSocket& s);
};

}}} // namespace qpid::sys::ssl

#endif  /*!QPID_SYS_SSL_SSLHANDLER_H*/
