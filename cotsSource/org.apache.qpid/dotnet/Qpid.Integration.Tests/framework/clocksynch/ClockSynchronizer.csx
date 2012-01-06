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
    /// ClockSynchronizer provides an interface through which two nodes may synchronize their clocks. It is expected that one
    /// node will act as the reference clock, to which no delta need be applied, and the other node will act as the slave,
    /// and which must apply a delta to its local clock to get a clock synchronized with the reference.
    ///
    /// <p/>The slave side will initiate the computation of a clock delta by calling the <see cref="#synch"/> method. This method
    /// will not return until the delta has been computed, at which point there is a method to return its value, as well as
    /// an estimate of the likely error (usually one standard deviation), in the synchronization. For convenience there is a
    /// <see cref="#nanoTime"/> method to return the value of System.nanoTime() with the delta added in.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Trigger a clock synchronization.
    /// <tr><td> Compute a clock delta to apply to the local clock.
    /// <tr><td> Estimate the error in the synchronzation.
    /// </table>
    /// </summary>
    public interface ClockSynchronizer
    {
        /// <summary>
        /// The slave side should call this to copute a clock delta with the reference.
        /// </summary>
        /// <exception cref="ClockSynchFailureException"> If synchronization cannot be achieved. </exception>
        public void synch() throws ClockSynchFailureException;

        /// <summary>
        /// Gets the clock delta in nano seconds.
        /// </summary>
        /// <return> The clock delta in nano seconds. </return>
        public long getDelta();

        /// <summary>
        /// Gets an estimate of the clock error in nan seconds.
        /// </summary>
        /// <return> An estimate of the clock error in nan seconds. </return>
        public long getEpsilon();

        /// <summary>
        /// Gets the local clock time with any computed delta added in.
        /// </summary>
        /// <return> The local clock time with any computed delta added in. </return>
        public long nanoTime();
    }
}