#ifndef QPID_CLIENT_AMQP0_10_ADDRESSRESOLUTION_H
#define QPID_CLIENT_AMQP0_10_ADDRESSRESOLUTION_H

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
#include "qpid/messaging/Variant.h"
#include "qpid/client/Session.h"

namespace qpid {

namespace framing{
class ReplyTo;
}

namespace messaging {
class Address;
}

namespace client {
namespace amqp0_10 {

class MessageSource;
class MessageSink;

/**
 * Maps from a generic Address and optional Filter to an AMQP 0-10
 * MessageSource which will then be used by a ReceiverImpl instance
 * created for the address.
 */
class AddressResolution
{
  public:
    std::auto_ptr<MessageSource> resolveSource(qpid::client::Session session,
                                               const qpid::messaging::Address& address);
    
    std::auto_ptr<MessageSink> resolveSink(qpid::client::Session session,
                                           const qpid::messaging::Address& address);

    static qpid::messaging::Address convert(const qpid::framing::ReplyTo&);
    static qpid::framing::ReplyTo convert(const qpid::messaging::Address&);

  private:
};
}}} // namespace qpid::client::amqp0_10

#endif  /*!QPID_CLIENT_AMQP0_10_ADDRESSRESOLUTION_H*/
