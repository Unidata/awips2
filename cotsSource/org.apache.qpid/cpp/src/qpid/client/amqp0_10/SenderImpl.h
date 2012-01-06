#ifndef QPID_CLIENT_AMQP0_10_SENDERIMPL_H
#define QPID_CLIENT_AMQP0_10_SENDERIMPL_H

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
#include "qpid/messaging/Address.h"
#include "qpid/messaging/Message.h"
#include "qpid/messaging/SenderImpl.h"
#include "qpid/messaging/Variant.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/client/amqp0_10/SessionImpl.h"
#include <memory>
#include <boost/ptr_container/ptr_deque.hpp>

namespace qpid {
namespace client {
namespace amqp0_10 {

class AddressResolution;
class MessageSink;
struct OutgoingMessage;

/**
 *
 */
class SenderImpl : public qpid::messaging::SenderImpl
{
  public:
    enum State {UNRESOLVED, ACTIVE, CANCELLED};

    SenderImpl(SessionImpl& parent, const std::string& name, 
               const qpid::messaging::Address& address);
    void send(const qpid::messaging::Message&);
    void cancel();
    void setCapacity(uint32_t);
    uint32_t getCapacity();
    uint32_t pending();
    void init(qpid::client::AsyncSession, AddressResolution&);
    const std::string& getName() const;
    qpid::messaging::Session getSession() const;

  private:
    SessionImpl& parent;
    const std::string name;
    const qpid::messaging::Address address;
    State state;
    std::auto_ptr<MessageSink> sink;

    qpid::client::AsyncSession session;
    std::string destination;
    std::string routingKey;

    typedef boost::ptr_deque<OutgoingMessage> OutgoingMessages;
    OutgoingMessages outgoing;
    uint32_t capacity;
    uint32_t window;
    bool flushed;

    uint32_t checkPendingSends(bool flush);
    void replay();
    void waitForCapacity();

    //logic for application visible methods:
    void sendImpl(const qpid::messaging::Message&);
    void cancelImpl();


    //functors for application visible methods (allowing locking and
    //retry to be centralised):
    struct Command
    {
        SenderImpl& impl;

        Command(SenderImpl& i) : impl(i) {}
    };

    struct Send : Command
    {
        const qpid::messaging::Message* message;
        bool repeat;

        Send(SenderImpl& i, const qpid::messaging::Message* m) : Command(i), message(m), repeat(true) {}
        void operator()() 
        {
            impl.waitForCapacity();
            //from this point message will be recorded if there is any
            //failure (and replayed) so need not repeat the call
            repeat = false;
            impl.sendImpl(*message);
        }
    };

    struct Cancel : Command
    {
        Cancel(SenderImpl& i) : Command(i) {}
        void operator()() { impl.cancelImpl(); }
    };

    struct CheckPendingSends : Command
    {
        bool flush;
        uint32_t pending;
        CheckPendingSends(SenderImpl& i, bool f) : Command(i), flush(f), pending(0) {}
        void operator()() { pending = impl.checkPendingSends(flush); }
    };

    //helper templates for some common patterns
    template <class F> void execute()
    {
        F f(*this);
        parent.execute(f);
    }
    
    template <class F, class P> bool execute1(P p)
    {
        F f(*this, p);
        return parent.execute(f);
    }    
};
}}} // namespace qpid::client::amqp0_10

#endif  /*!QPID_CLIENT_AMQP0_10_SENDERIMPL_H*/
