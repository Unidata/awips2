#ifndef QPID_AMQP_0_10_CONNECTION_H
#define QPID_AMQP_0_10_CONNECTION_H

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

#include "qpid/framing/AMQFrame.h"
#include "qpid/sys/ConnectionCodec.h"
#include "qpid/sys/ConnectionInputHandler.h"
#include "qpid/sys/ConnectionOutputHandler.h"
#include "qpid/sys/Mutex.h"
#include "qpid/broker/BrokerImportExport.h"
#include <boost/intrusive_ptr.hpp>
#include <memory>
#include <deque>

namespace qpid {

namespace sys {
class ConnectionInputHandlerFactory;
}

namespace amqp_0_10 {

class Connection  : public sys::ConnectionCodec,
                    public sys::ConnectionOutputHandler
{
    typedef std::deque<framing::AMQFrame> FrameQueue;

    FrameQueue frameQueue;
    FrameQueue workQueue;
    bool pushClosed, popClosed;
    mutable sys::Mutex frameQueueLock;
    sys::OutputControl& output;
    std::auto_ptr<sys::ConnectionInputHandler> connection;
    std::string identifier;
    bool initialized;
    bool isClient;
    size_t buffered;
    framing::ProtocolVersion version;

  public:
    QPID_BROKER_EXTERN Connection(sys::OutputControl&, const std::string& id, bool isClient);
    QPID_BROKER_EXTERN void setInputHandler(std::auto_ptr<sys::ConnectionInputHandler> c);
    size_t decode(const char* buffer, size_t size);
    size_t encode(const char* buffer, size_t size);
    bool isClosed() const;
    bool canEncode();
    void abort();
    void activateOutput();
    void giveReadCredit(int32_t);
    void closed();              // connection closed by peer.
    void close();               // closing from this end.
    void send(framing::AMQFrame&);
    framing::ProtocolVersion getVersion() const;
    size_t getBuffered() const;

    /** Used by cluster code to set a special version on "update" connections. */
    // FIXME aconway 2009-07-30: find a cleaner mechanism for this.
    void setVersion(const framing::ProtocolVersion&);
};

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_CONNECTION_H*/
