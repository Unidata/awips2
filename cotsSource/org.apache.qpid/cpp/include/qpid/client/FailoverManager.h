#ifndef QPID_CLIENT_FAILOVERMANAGER_H
#define QPID_CLIENT_FAILOVERMANAGER_H

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
#include "qpid/client/AsyncSession.h"
#include "qpid/client/ClientImportExport.h"
#include "qpid/client/Connection.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/client/FailoverListener.h"
#include "qpid/sys/Monitor.h"
#include <vector>

namespace qpid {
namespace client {

struct CannotConnectException : qpid::Exception
{
    CannotConnectException(const std::string& m) : qpid::Exception(m) {}
};

/**
 * Utility to manage failover.
 */
class FailoverManager
{
  public:
    /**
     * Interface to implement for doing work that can be resumed on
     * failover
     */
    struct Command
    {
        /**
         * This method will be called with isRetry=false when the
         * command is first executed. The session to use for the work
         * will be passed to the implementing class. If the connection
         * fails while the execute call is in progress, the
         * FailoverManager controlling the execution will re-establish
         * a connection, open a new session and call back to the
         * Command implementations execute method with the new session
         * and isRetry=true.
         */
        virtual void execute(AsyncSession& session, bool isRetry) = 0;
        virtual ~Command() {}
    };

    struct ReconnectionStrategy
    {
        /**
         * This method is called by the FailoverManager prior to
         * establishing a connection (or re-connection) and can be
         * used if the application wishes to edit or re-order the list
         * which will default to the list of known brokers for the
         * last connection.
         */
        virtual void editUrlList(std::vector<Url>&  urls) = 0;
        virtual ~ReconnectionStrategy() {}
    };

    /**
     * Create a manager to control failover for a logical connection.
     *
     * @param settings the initial connection settings
     * @param strategy optional stratgey callback allowing application
     * to edit or reorder the list of urls to which reconnection is
     * attempted
     */
    QPID_CLIENT_EXTERN FailoverManager(const ConnectionSettings& settings, ReconnectionStrategy* strategy = 0);
    /**
     * Return the current connection if open or attept to reconnect to
     * the specified list of urls. If no list is specified the list of
     * known brokers from the last connection will be used. If no list
     * is specified and this is the first connect attempt, the host
     * and port from the initial settings will be used.
     *
     * If the full list is tried and all attempts fail,
     * CannotConnectException is thrown.
     */
    QPID_CLIENT_EXTERN Connection& connect(std::vector<Url> brokers = std::vector<Url>());
    /**
     * Return the current connection whether open or not
     */
    QPID_CLIENT_EXTERN Connection& getConnection();
    /**
     * Close the current connection
     */
    QPID_CLIENT_EXTERN void close();
    /**
     * Reliably execute the specified command. This involves creating
     * a session on which to carry out the work of the command,
     * handling failover occuring while exeuting that command and
     * re-starting the work.
     *
     * Multiple concurrent threads can call execute with different
     * commands; each thread will be allocated its own
     * session. FailoverManager will coordinate the different threads
     * on failover to ensure they continue to use the same logical
     * connection.
     */
    QPID_CLIENT_EXTERN void execute(Command&);
  private:
    enum State {IDLE, CONNECTING, CANT_CONNECT};

    qpid::sys::Monitor lock;
    Connection connection;
    std::auto_ptr<FailoverListener> failoverListener;
    ConnectionSettings settings;
    ReconnectionStrategy* strategy;
    State state;

    void attempt(Connection&, ConnectionSettings settings, std::vector<Url> urls);
    void attempt(Connection&, ConnectionSettings settings);
};
}} // namespace qpid::client

#endif  /*!QPID_CLIENT_FAILOVERMANAGER_H*/
