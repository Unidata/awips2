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
namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{
    /// <summary>
    /// ClockSynchFailureException represents failure of a <see cref="ClockSynchronizer"/> to achieve synchronization. For example,
    /// this could be because a reference signal is not available, or because a desired accurracy cannot be attained.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Represent failure to achieve synchronization.
    /// </table>
    /// </summary>
    public class ClockSynchFailureException extends Exception
    {
        /// <summary>
        /// Creates a clock synch failure exception.
        /// </summary>
        /// <param name="message"> The detail message (which is saved for later retrieval by the <see cref="#getMessage()"/> method). </param>
        /// <param name="cause">   The cause (which is saved for later retrieval by the <see cref="#getCause()"/> method).  (A <tt>null</tt>
        ///                        value is permitted, and indicates that the cause is nonexistent or unknown.)</param>
        public ClockSynchFailureException(string message, Throwable cause)
        {
            super(message, cause);
        }
    }
}