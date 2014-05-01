#ifndef QPID_MESSAGING_SENDER_H
#define QPID_MESSAGING_SENDER_H

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
#include "qpid/client/ClientImportExport.h"
#include "qpid/client/Handle.h"
#include "qpid/sys/IntegerTypes.h"
#include <string>

namespace qpid {
namespace client {

template <class> class PrivateImplRef;

}

namespace messaging {

class Message;
class SenderImpl;
class Session;
/**
 * Interface through which messages are sent.
 */
class Sender : public qpid::client::Handle<SenderImpl>
{
  public:
    QPID_CLIENT_EXTERN Sender(SenderImpl* impl = 0);
    QPID_CLIENT_EXTERN Sender(const Sender&);
    QPID_CLIENT_EXTERN ~Sender();
    QPID_CLIENT_EXTERN Sender& operator=(const Sender&);

    QPID_CLIENT_EXTERN void send(const Message& message);
    QPID_CLIENT_EXTERN void cancel();

    /**
     * Sets the capacity for the sender. The capacity determines how
     * many outgoing messages can be held pending confirmation of
     * receipt by the broker.
     */
    QPID_CLIENT_EXTERN void setCapacity(uint32_t);
    /**
     * Returns the capacity of the sender. 
     * @see setCapacity
     */
    QPID_CLIENT_EXTERN uint32_t getCapacity();
    /**
     * Returns the number of sent messages pending confirmation of
     * receipt by the broker. (These are the 'in-doubt' messages).
     */
    QPID_CLIENT_EXTERN uint32_t pending();

    /**
     * Returns the name of this sender.
     */
    QPID_CLIENT_EXTERN const std::string& getName() const;

    /**
     * Returns a handle to the session associated with this sender.
     */
    QPID_CLIENT_EXTERN Session getSession() const;
  private:
  friend class qpid::client::PrivateImplRef<Sender>;
};
}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_SENDER_H*/
