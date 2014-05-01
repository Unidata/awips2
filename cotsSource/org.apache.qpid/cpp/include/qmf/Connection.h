#ifndef _QmfConnection_
#define _QmfConnection_

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

#include "qmf/QmfImportExport.h"
#include "qmf/ConnectionSettings.h"

namespace qmf {

    /**
     * Operational states for Connections.
     *
     * \ingroup qmfapi
     */
    enum ConnectionState {
       CONNECTION_UP   = 1,
       CONNECTION_DOWN = 2
    };

    /**
     * Implement a subclass of ConnectionListener and provide it with the
     * Connection constructor to receive notification of changes in the
     * connection state.
     *
     * \ingroup qmfapi
     */
    class ConnectionListener {
        QMF_EXTERN virtual ~ConnectionListener();

        /**
         * Called each time the state of the connection changes.
         *
         * @param state the new state
         */
        virtual void newState(ConnectionState state);

        /**
         * Called if the connection requires input from an interactive client.
         *
         * @param prompt Text of the prompt - describes what information is required.
         * @param answer The interactive user input.
         * @param answerLen on Input - the maximum number of bytes that can be copied to answer.
         *                  on Output - the number of bytes copied to answer.
         */
        virtual void interactivePrompt(const char* prompt, char* answer, uint32_t answerLen);
    };

    class ConnectionImpl;

    /**
     * The Connection class represents a connection to a QPID broker that can
     * be used by agents and consoles, possibly multiple at the same time.
     *
     * \ingroup qmfapi
     */
    class Connection {
    public:

        /**
         * Creates a connection object and begins the process of attempting to
         * connect to the QPID broker.
         *
         * @param settings The settings that control how the connection is set
         * up.
         *
         * @param listener An optional pointer to a subclass of
         * ConnectionListener to receive notifications of events related to
         * this connection.
         */
        QMF_EXTERN Connection(const ConnectionSettings& settings,
                              const ConnectionListener* listener = 0);

        /**
         * Destroys a connection, causing the connection to be closed.
         */
        QMF_EXTERN ~Connection();

        /**
         * Set the administrative state of the connection (enabled or disabled).
         *
         * @param enabled True => enable connection, False => disable connection
         */
        QMF_EXTERN void setAdminState(bool enabled);

        /**
         * Return the current operational state of the connection (up or down).
         *
         * @return the current connection state.
         */
        QMF_EXTERN ConnectionState getOperState() const;

        /**
         * Get the error message from the last failure to connect.
         *
         * @return Null-terminated string containing the error message.
         */
        QMF_EXTERN const char* getLastError() const;

    private:
        friend class AgentImpl;
        friend class ConsoleImpl;
        ConnectionImpl* impl;
    };

}

#endif
