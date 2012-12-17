#ifndef QPID_MESSAGING_SESSION_H
#define QPID_MESSAGING_SESSION_H

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
#include "qpid/Exception.h"
#include "qpid/client/ClientImportExport.h"
#include "qpid/client/Handle.h"
#include "qpid/sys/Time.h"
#include <string>

namespace qpid {
namespace client {

template <class> class PrivateImplRef;

}

namespace messaging {

class Address;
class Connection;
class Message;
class MessageListener;
class Sender;
class Receiver;
class SessionImpl;
class Subscription;

struct KeyError : qpid::Exception
{
    QPID_CLIENT_EXTERN KeyError(const std::string&);
};

/**
 * A session represents a distinct 'conversation' which can involve
 * sending and receiving messages to and from different addresses.
 */
class Session : public qpid::client::Handle<SessionImpl>
{
  public:
    QPID_CLIENT_EXTERN Session(SessionImpl* impl = 0);
    QPID_CLIENT_EXTERN Session(const Session&);
    QPID_CLIENT_EXTERN ~Session();
    QPID_CLIENT_EXTERN Session& operator=(const Session&);

    QPID_CLIENT_EXTERN void close();

    QPID_CLIENT_EXTERN void commit();
    QPID_CLIENT_EXTERN void rollback();

    /**
     * Acknowledges all outstanding messages that have been received
     * by the application on this session.
     */
    QPID_CLIENT_EXTERN void acknowledge();
    /**
     * Rejects the specified message. This will prevent the message
     * being redelivered.
     */
    QPID_CLIENT_EXTERN void reject(Message&);

    QPID_CLIENT_EXTERN void sync();
    QPID_CLIENT_EXTERN void flush();

    /**
     * Returns the number of messages received and waiting to be
     * fetched.
     */
    QPID_CLIENT_EXTERN uint32_t available();
    /**
     * Returns a count of the number of messages received this session
     * that have been acknowledged, but for which that acknowledgement
     * has not yet been confirmed as processed by the server.
     */
    QPID_CLIENT_EXTERN uint32_t pendingAck();
    /**
     * Retrieves the receiver for the next available message. If there
     * are no available messages at present the call will block for up
     * to the specified timeout waiting for one to arrive. Returns
     * true if a message was available at the point of return, in
     * which case the passed in receiver reference will be set to the
     * receiver for that message or fals if no message was available.
     */
    QPID_CLIENT_EXTERN bool nextReceiver(Receiver&, qpid::sys::Duration timeout=qpid::sys::TIME_INFINITE);
    /**
     * Returns the receiver for the next available message. If there
     * are no available messages at present the call will block for up
     * to the specified timeout waiting for one to arrive. Will throw
     * Receiver::NoMessageAvailable if no message became available in
     * time.
     */
    QPID_CLIENT_EXTERN Receiver nextReceiver(qpid::sys::Duration timeout=qpid::sys::TIME_INFINITE);
    
    /**
     * Create a new sender through which messages can be sent to the
     * specified address.
     */
    QPID_CLIENT_EXTERN Sender createSender(const Address& address);
    QPID_CLIENT_EXTERN Sender createSender(const std::string& address);

    /**
     * Create a new receiver through which messages can be received
     * from the specified address.
     */
    QPID_CLIENT_EXTERN Receiver createReceiver(const Address& address);
    QPID_CLIENT_EXTERN Receiver createReceiver(const std::string& address);

    /**
     * Returns the sender with the specified name or throws KeyError
     * if there is none for that name.
     */
    QPID_CLIENT_EXTERN Sender getSender(const std::string& name) const;
    /**
     * Returns the receiver with the specified name or throws KeyError
     * if there is none for that name.
     */
    QPID_CLIENT_EXTERN Receiver getReceiver(const std::string& name) const;
    /**
     * Returns a handle to the connection this session is associated
     * with.
     */
    QPID_CLIENT_EXTERN Connection getConnection() const;

  private:
  friend class qpid::client::PrivateImplRef<Session>;
};
}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_SESSION_H*/
