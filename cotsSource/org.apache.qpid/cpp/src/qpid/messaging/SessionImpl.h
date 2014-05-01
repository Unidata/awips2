#ifndef QPID_MESSAGING_SESSIONIMPL_H
#define QPID_MESSAGING_SESSIONIMPL_H

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
#include "qpid/RefCounted.h"
#include <string>
#include "qpid/sys/Time.h"

namespace qpid {
namespace client {
}

namespace messaging {

class Address;
class Connection;
class Message;
class Sender;
class Receiver;

class SessionImpl : public virtual qpid::RefCounted
{
  public:
    virtual ~SessionImpl() {}
    virtual void commit() = 0;
    virtual void rollback() = 0;
    virtual void acknowledge() = 0;
    virtual void reject(Message&) = 0;
    virtual void close() = 0;
    virtual void sync() = 0;
    virtual void flush() = 0;
    virtual Sender createSender(const Address& address) = 0;
    virtual Receiver createReceiver(const Address& address) = 0;
    virtual bool nextReceiver(Receiver& receiver, qpid::sys::Duration timeout) = 0;
    virtual Receiver nextReceiver(qpid::sys::Duration timeout) = 0;
    virtual uint32_t available() = 0;
    virtual uint32_t pendingAck() = 0;
    virtual Sender getSender(const std::string& name) const = 0;
    virtual Receiver getReceiver(const std::string& name) const = 0;
    virtual Connection getConnection() const = 0;
  private:
};
}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_SESSIONIMPL_H*/
