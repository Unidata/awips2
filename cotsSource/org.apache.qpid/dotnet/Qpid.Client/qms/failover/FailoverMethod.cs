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
using System;

namespace Apache.Qpid.Client.Qms.Failover
{
    public class FailoverMethodConstants
    {
        public const String ROUND_ROBIN = "roundrobin";
        public const String RANDOM = "random";
    }

    public interface IFailoverMethod
    {
        /// <summary>
        /// The name of this method for display purposes.
        /// </summary>
        String MethodName { get; }
        
        /// <summary>
        /// Reset the Failover to initial conditions
        /// </summary>
        void Reset();

        /// <summary>
        /// Check if failover is possible for this method
        /// </summary>
        /// <returns>true if failover is allowed</returns>
        bool FailoverAllowed();

        /// <summary>
        /// Notification to the Failover method that a connection has been attained.
        /// </summary>
        void AttainedConnection();

        /// <summary>
        /// If there is no current BrokerInfo the null will be returned.
        /// </summary>
        /// <returns>The current BrokerDetail value to use</returns>
        IBrokerInfo GetCurrentBrokerInfo();

        /// <summary>
        /// Move to the next BrokerInfo if one is available.
        /// </summary>
        /// <returns>the next BrokerDetail or null if there is none.</returns>
        IBrokerInfo GetNextBrokerDetails();

        /// <summary>
        /// Set the currently active broker to be the new value.
        /// </summary>
        /// <param name="broker">The new BrokerDetail value</param>
        void SetBroker(IBrokerInfo broker);

        /// <summary>
        /// Set the retries for this method
        /// </summary>
        /// <param name="maxRetries">the maximum number of time to retry this Method</param>
        void SetRetries(int maxRetries);
    }
}
