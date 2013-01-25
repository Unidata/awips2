#ifndef QPID_CLIENT_CONNECTION_H
#define QPID_CLIENT_CONNECTION_H

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
#include <map>
#include <string>
#include "qpid/client/Session.h"
#include "qpid/client/ClientImportExport.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/framing/ProtocolVersion.h"

#include "boost/function.hpp"

namespace qpid {

struct Url;

namespace client {

class ConnectionImpl;

/**
 * Represents a connection to an AMQP broker. All communication is
 * initiated by establishing a connection, then creating one or more
 * Session objects using the connection. @see newSession()
 *
 * \ingroup clientapi
 *
 * Some methods use an AMQP 0-10 URL to specify connection parameters.
 * This is defined in the AMQP 0-10 specification (http://jira.amqp.org/confluence/display/AMQP/AMQP+Specification).
 *
 *   amqp_url          = "amqp:" prot_addr_list
 *   prot_addr_list    = [prot_addr ","]* prot_addr
 *   prot_addr         = tcp_prot_addr | tls_prot_addr
 *
 *   tcp_prot_addr     = tcp_id tcp_addr
 *   tcp_id            = "tcp:" | ""
 *   tcp_addr          = [host [":" port] ]
 *   host              = <as per http://www.ietf.org/rfc/rfc3986.txt>
 *   port              = number]]>
 *  
 */

class Connection
{
    framing::ProtocolVersion version;

    boost::function<void ()> failureCallback;


  protected:
    boost::shared_ptr<ConnectionImpl> impl;


  public:
    /**
     * Creates a Connection object, but does not open the connection.
     * @see open()
     */
    QPID_CLIENT_EXTERN Connection();

    /**
     * Destroys a Connection object but does not close the connection if it
     * was open. @see close()
     */
    QPID_CLIENT_EXTERN ~Connection();

    /**
     * Opens a connection to a broker.
     *
     * @param host the host on which the broker is running.
     *
     * @param port the port on the which the broker is listening.
     *
     * @param uid the userid to connect with.
     *
     * @param pwd the password to connect with (currently SASL
     * PLAIN is the only authentication method supported so this
     * is sent in clear text).
     *
     * @param virtualhost the AMQP virtual host to use (virtual
     * hosts, where implemented(!), provide namespace partitioning
     * within a single broker).
     */
    QPID_CLIENT_EXTERN void open(const std::string& host, int port = 5672,
              const std::string& uid = "guest",
              const std::string& pwd = "guest",
              const std::string& virtualhost = "/", uint16_t maxFrameSize=65535);

    /**
     * Opens a connection to a broker using a URL.
     * If the URL contains multiple addresses, try each in turn
     * till connection is successful.
     *
     * @url address of the broker to connect to.
     *
     * @param uid the userid to connect with.
     *
     * @param pwd the password to connect with (currently SASL
     * PLAIN is the only authentication method supported so this
     * is sent in clear text).
     *
     * @param virtualhost the AMQP virtual host to use (virtual
     * hosts, where implemented(!), provide namespace partitioning
     * within a single broker).
     */
    QPID_CLIENT_EXTERN void open(const Url& url,
              const std::string& uid = "guest",
              const std::string& pwd = "guest",
              const std::string& virtualhost = "/", uint16_t maxFrameSize=65535);

    /**
     * Opens a connection to a broker using a URL.
     * If the URL contains multiple addresses, try each in turn
     * till connection is successful.
     *
     * @url address of the broker to connect to.
     *
     * @param settings used for any settings not provided by the URL.
     * Settings provided by the url (e.g. host, port) are ignored.
     */
    QPID_CLIENT_EXTERN void open(const Url& url, const ConnectionSettings& settings);

    /**
     * Opens a connection to a broker.
     *
     * @param the settings to use (host, port etc). @see ConnectionSettings.
     */
    QPID_CLIENT_EXTERN void open(const ConnectionSettings& settings);

    /**
     * Close the connection.
     *
     * Any further use of this connection (without reopening it) will
     * not succeed.
     */
    QPID_CLIENT_EXTERN void close();

    /**
     * Create a new session on this connection.  Sessions allow
     * multiple streams of work to be multiplexed over the same
     * connection. The returned Session provides functions to send
     * commands to the broker.
     *
     * Session functions are synchronous. In other words, a Session
     * function will send a command to the broker and does not return
     * until it receives the broker's response confirming the command
     * was executed.
     *
     * AsyncSession provides asynchronous versions of the same
     * functions.  These functions send a command to the broker but do
     * not wait for a response.
     *
     * You can convert a Session s into an AsyncSession as follows:
     * @code
     *  #include <qpid/client/AsyncSession.h>
     *  AsyncSession as = async(s);
     * @endcode
     *
     * You can execute a single command asynchronously will a Session s
     * like ths:
     * @code
     *  async(s).messageTransfer(...);
     * @endcode
     *
     * Using an AsyncSession is faster for sending large numbers of
     * commands, since each command is sent as soon as possible
     * without waiting for the previous command to be confirmed.
     *
     * However with AsyncSession you cannot assume that a command has
     * completed until you explicitly synchronize. The simplest way to
     * do this is to call Session::sync() or AsyncSession::sync().
     * Both of these functions wait for the broker to confirm all
     * commands issued so far on the session.
     *
     *@param name: A name to identify the session. @see qpid::SessionId
     * If the name is empty (the default) then a unique name will be
     * chosen using a Universally-unique identifier (UUID) algorithm.
     */
    QPID_CLIENT_EXTERN Session newSession(const std::string& name=std::string(), uint32_t timeoutSeconds = 0);

    /**
     * Resume a suspended session. A session may be resumed
     * on a different connection to the one that created it.
     */
    QPID_CLIENT_EXTERN void resume(Session& session);

    QPID_CLIENT_EXTERN bool isOpen() const;

    /** In a cluster, returns the initial set of known broker URLs
     * at the time of connection.
     */
    QPID_CLIENT_EXTERN std::vector<Url> getInitialBrokers();

    QPID_CLIENT_EXTERN void registerFailureCallback ( boost::function<void ()> fn );

    /**
     * Return the set of client negotiated settings
     */
    QPID_CLIENT_EXTERN const ConnectionSettings& getNegotiatedSettings();

  friend struct ConnectionAccess; ///<@internal
  friend class SessionBase_0_10; ///<@internal
};

}} // namespace qpid::client


#endif  /*!QPID_CLIENT_CONNECTION_H*/
