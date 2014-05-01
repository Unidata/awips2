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
namespace Apache.Qpid.Messaging
{
    public interface IConnectionListener
    {
        /// <summary>
        /// Called when bytes have been transmitted to the server
        /// </summary>
        /// <param>count the number of bytes sent in total since the connection was opened</param>     
        void BytesSent(long count);

        /// <summary>
        /// Called when some bytes have been received on a connection
        /// </summary>
        /// <param>count the number of bytes received in total since the connection was opened</param>         
        void BytesReceived(long count);

        /// <summary>
        /// Called after the infrastructure has detected that failover is required but before attempting failover.
        /// </summary>
        /// <param>redirect true if the broker requested redirect. false if failover is occurring due to a connection error.</param>
        /// <return>true to continue failing over, false to veto failover and raise a connection exception</return>         
        bool PreFailover(bool redirect);

        /// <summary>
        /// Called after connection has been made to another broker after failover has been started but before
        /// any resubscription has been done.
        /// <return> true to continue with resubscription, false to prevent automatic resubscription. This is useful in
        /// cases where the application wants to handle resubscription. Note that in the latter case all sessions, producers
        /// and consumers are invalidated.
        /// </return
        bool PreResubscribe();

        /// <summary>
        /// Called once failover has completed successfully. This is called irrespective of whether the client has
        /// vetoed automatic resubscription.
        /// </summary>
        void FailoverComplete();
    }
}
