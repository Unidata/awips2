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

namespace org.apache.qpid.client
{
    public interface ClientInterface
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
        void connect(String host, int port, String virtualHost, String username, String passwor);

        /// <summary>        
        /// Close this client
        /// </summary>
        void close();

        /// <summary>
        /// Create a session for this connection.
        /// The returned session is suspended
        /// (i.e. this session is not attached to an underlying channel)
        /// </summary>
        /// <param name="expiryInSeconds">Expiry time expressed in seconds, if the value is less than
        /// or equal to 0 then the session does not expire.</param>
        /// <returns>A newly created (suspended) session.</returns>
        ClientSession createSession(long expiryInSeconds);

        /// <summary>      
        /// If the communication layer detects a serious problem with a connection, it
        //  informs the client's ClosedListener
        /// </summary>        
        /// 
        ClosedListener ClosedListener { set; }
    }
}
