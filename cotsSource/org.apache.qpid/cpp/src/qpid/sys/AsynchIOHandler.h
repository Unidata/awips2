#ifndef _sys_AsynchIOHandler_h
#define _sys_AsynchIOHandler_h
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

#include "qpid/sys/OutputControl.h"
#include "qpid/sys/ConnectionCodec.h"
#include "qpid/sys/AtomicValue.h"
#include "qpid/sys/Mutex.h"
#include "qpid/CommonImportExport.h"

namespace qpid {

namespace framing {
    class ProtocolInitiation;
}

namespace sys {

class AsynchIO;
struct AsynchIOBufferBase;
class Socket;

class AsynchIOHandler : public OutputControl {
    std::string identifier;
    AsynchIO* aio;
    ConnectionCodec::Factory* factory;
    ConnectionCodec* codec;
    bool readError;
    bool isClient;
    AtomicValue<int32_t> readCredit;
    static const int32_t InfiniteCredit = -1;
    Mutex creditLock;

    void write(const framing::ProtocolInitiation&);

  public:
    QPID_COMMON_EXTERN AsynchIOHandler(std::string id, ConnectionCodec::Factory* f);
    QPID_COMMON_EXTERN ~AsynchIOHandler();
    QPID_COMMON_EXTERN void init(AsynchIO* a, int numBuffs);

    QPID_COMMON_EXTERN void setClient() { isClient = true; }

    // Output side
    QPID_COMMON_EXTERN void abort();
    QPID_COMMON_EXTERN void activateOutput();
    QPID_COMMON_EXTERN void giveReadCredit(int32_t credit);

    // Input side
    QPID_COMMON_EXTERN void readbuff(AsynchIO& aio, AsynchIOBufferBase* buff);
    QPID_COMMON_EXTERN void eof(AsynchIO& aio);
    QPID_COMMON_EXTERN void disconnect(AsynchIO& aio);

    // Notifications
    QPID_COMMON_EXTERN void nobuffs(AsynchIO& aio);
    QPID_COMMON_EXTERN void idle(AsynchIO& aio);
    QPID_COMMON_EXTERN void closedSocket(AsynchIO& aio, const Socket& s);
};

}} // namespace qpid::sys

#endif // _sys_AsynchIOHandler_h
