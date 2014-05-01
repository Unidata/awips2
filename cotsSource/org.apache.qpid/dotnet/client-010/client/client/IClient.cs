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

using System;
using org.apache.qpid.transport;

namespace org.apache.qpid.client
{
    public interface IClient
    {
        /// <summary>
        /// Establish a connection with the broker using the given parameters
        /// 
        /// </summary>
        /// <param name="host">host name</param>
        /// <param name="port">port number</param>
        /// <param name="virtualHost">virtualHost the virtual host name</param>
        /// <param name="username"> username user name</param>
        /// <param name="passwor">password password</param>
        void Connect(String host, int port, String virtualHost, String username, String passwor);

        /// <summary>        
        /// Close this client
        /// </summary>
        void Close();

        /// <summary>
        /// Create a session for this connection.
        /// The returned session is suspended
        /// (i.e. this session is not attached to an underlying channel)
        /// </summary>
        /// <param name="expiryInSeconds">Expiry time expressed in seconds, if the value is less than
        /// or equal to 0 then the session does not expire.</param>
        /// <returns>A newly created (suspended) session.</returns>
        IClientSession CreateSession(long expiryInSeconds);


        event EventHandler<ExceptionArgs> ExceptionRaised;
        event EventHandler ConnectionLost;

        /// <summary>      
        /// If the broker sends a disconnect message, it will notify the ClosedListener
        /// </summary>        
        /// 
        IClosedListener ClosedListener { set; }



        bool IsClosed { get; set; }

        /// <summary>
        /// Establishes a connection with a broker using SSL
        /// 
        /// </summary>
        /// <param name="host">Host name on which a broker is deployed</param>
        /// <param name="port">Broker port </param>
        /// <param name="virtualHost">virtual host name</param>
        /// <param name="username">User Name</param>
        /// <param name="password">Password</param>
        /// <param name="serverName">Name of the SSL server</param>
        /// <param name="certPath">Path to the X509 certificate to be used for client authentication</param>
        /// <param name="rejectUntrusted">If true connection will not be established if the broker is not trusted</param>
        void ConnectSSL(String host, int port, String virtualHost, String username, String password, string serverName, string certPath, bool rejectUntrusted);
    }
}
