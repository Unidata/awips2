#ifndef _QmfEngineResilientConnection_
#define _QmfEngineResilientConnection_

/*
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
 */

#include <qmf/engine/Message.h>
#include <qmf/engine/ConnectionSettings.h>
#include <string>

namespace qmf {
namespace engine {

    class ResilientConnectionImpl;

    /**
     * Represents events that occur, unsolicited, from ResilientConnection.
     */
    struct ResilientConnectionEvent {
        enum EventKind {
            CONNECTED      = 1,
            DISCONNECTED   = 2,
            SESSION_CLOSED = 3,
            RECV           = 4
        };

        EventKind kind;
        void*     sessionContext; // SESSION_CLOSED, RECV
        char*     errorText;      // DISCONNECTED, SESSION_CLOSED
        Message   message;        // RECV
    };

    class SessionHandle {
        friend class ResilientConnectionImpl;
        void* impl;
    };

    /**
     * ResilientConnection represents a Qpid connection that is resilient.
     *
     * Upon creation, ResilientConnection attempts to establish a connection to the
     * messaging broker.  If it fails, it will continue to retry at an interval that
     * increases over time (to a maximum interval).  If an extablished connection is
     * dropped, a reconnect will be attempted.
     */
    class ResilientConnection {
    public:

        /**
         * Create a new resilient connection.
         *@param settings Settings that define how the connection is to be made.
         *@param delayMin Minimum delay (in seconds) between retries.
         *@param delayMax Maximum delay (in seconds) between retries.
         *@param delayFactor Factor to multiply retry delay by after each failure.
         */
        ResilientConnection(const ConnectionSettings& settings);
        ~ResilientConnection();

        /**
         * Get the connected status of the resilient connection.
         *@return true iff the connection is established.
         */
        bool isConnected() const;

        /**
         * Get the next event (if present) from the connection.
         *@param event Returned event if one is available.
         *@return true if event is valid, false if there are no more events to handle.
         */
        bool getEvent(ResilientConnectionEvent& event);

        /**
         * Discard the event on the front of the queue.  This should be invoked after processing
         * the event from getEvent.
         */
        void popEvent();

        /**
         * Create a new AMQP session.
         *@param name Unique name for the session.
         *@param sessionContext Optional user-context value that will be provided in events
         *                      pertaining to this session.
         *@param handle Output handle to be stored and used in subsequent calls pertaining to
         *              this session.
         *@return true iff the session was successfully created.
         */
        bool createSession(const char* name, void* sessionContext, SessionHandle& handle);

        /**
         * Destroy a created session.
         *@param handle SessionHandle returned by createSession.
         */
        void destroySession(SessionHandle handle);

        /**
         * Send a message into the AMQP broker via a session.
         *@param handle The session handle of the session to transmit through.
         *@param message The QMF message to transmit.
         */
        void sendMessage(SessionHandle handle, Message& message);

        /**
         * Declare an exclusive, auto-delete queue for a session.
         *@param handle The session handle for the owner of the queue.
         *@param queue The name of the queue.
         */
        void declareQueue(SessionHandle handle, char* queue);

        /**
         * Delete a queue.
         *@param handle The session handle for the owner of the queue.
         *@param queue The name of the queue.
         */
        void deleteQueue(SessionHandle handle, char* queue);

        /**
         * Bind a queue to an exchange.
         *@param handle The session handle of the session to use for binding.
         *@param exchange The name of the exchange for binding.
         *@param queue The name of the queue for binding.
         *@param key The binding key.
         */
        void bind(SessionHandle handle, char* exchange, char* queue, char* key);

        /**
         * Remove a binding.
         *@param handle The session handle of the session to use for un-binding.
         *@param exchange The name of the exchange.
         *@param queue The name of the queue.
         *@param key The binding key.
         */
        void unbind(SessionHandle handle, char* exchange, char* queue, char* key);

        /**
         * Establish a file descriptor for event notification.
         *@param fd A file descriptor into which the connection shall write a character each
         *          time an event is enqueued.  This fd may be in a pair, the other fd of which
         *          is used in a select loop to control execution.
         */
        void setNotifyFd(int fd);

    private:
        ResilientConnectionImpl* impl;
    };
}
}

#endif

