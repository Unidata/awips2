#ifndef QPID_CLIENT_SESSIONBASE_H
#define QPID_CLIENT_SESSIONBASE_H

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

#include "qpid/SessionId.h"
#include "qpid/framing/amqp_structs.h"
#include "qpid/client/Message.h"
#include "qpid/client/Completion.h"
#include "qpid/client/TypedResult.h"
#include "qpid/client/ClientImportExport.h"
#include <string>

namespace qpid {
namespace client {

class Connection;
class SessionImpl;

using std::string;
using framing::Content;
using framing::FieldTable;
using framing::SequenceNumber;
using framing::SequenceSet;
using framing::SequenceNumberSet;
using qpid::SessionId;
using framing::Xid;

/** Unit of message credit: messages or bytes */
enum CreditUnit { MESSAGE_CREDIT=0, BYTE_CREDIT=1, UNLIMITED_CREDIT=0xFFFFFFFF };

/**
 * Base class for handles to an AMQP session.
 *
 * Subclasses provide the AMQP commands for a given
 * version of the protocol.
 */
class SessionBase_0_10 {
  public:

    ///@internal
    QPID_CLIENT_EXTERN SessionBase_0_10();
    QPID_CLIENT_EXTERN ~SessionBase_0_10();

    /** Get the session ID */
    QPID_CLIENT_EXTERN SessionId getId() const;

    /** Close the session.
     * A session is automatically closed when all handles to it are destroyed.
     */
    QPID_CLIENT_EXTERN void close();

    /**
     * Synchronize the session: sync() waits until all commands issued
     * on this session so far have been completed by the broker.
     *
     * Note sync() is always synchronous, even on an AsyncSession object
     * because that's almost always what you want. You can call
     * AsyncSession::executionSync() directly in the unusual event
     * that you want to do an asynchronous sync.
     */
    QPID_CLIENT_EXTERN void sync();

    /** Set the timeout for this session. */
    QPID_CLIENT_EXTERN uint32_t timeout(uint32_t seconds);

    /** Suspend the session - detach it from its connection */
    QPID_CLIENT_EXTERN void suspend();

    /** Resume a suspended session with a new connection */
    QPID_CLIENT_EXTERN void resume(Connection);

    /** Get the channel associated with this session */
    QPID_CLIENT_EXTERN uint16_t getChannel() const;

    QPID_CLIENT_EXTERN void flush();
    QPID_CLIENT_EXTERN void markCompleted(const framing::SequenceSet& ids, bool notifyPeer);
    QPID_CLIENT_EXTERN void markCompleted(const framing::SequenceNumber& id, bool cumulative, bool notifyPeer);
    QPID_CLIENT_EXTERN void sendCompletion();

    QPID_CLIENT_EXTERN bool isValid() const;

    QPID_CLIENT_EXTERN Connection getConnection();
  protected:
    boost::shared_ptr<SessionImpl> impl;
  friend class SessionBase_0_10Access;
};

}} // namespace qpid::client

#endif  /*!QPID_CLIENT_SESSIONBASE_H*/
