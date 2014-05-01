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
    /// LocalClockSynchronizer is a fake <see cref="ClockSynchronizer"/> that simply calls System.nanoTime(). It exists so that
    /// the same tests can be run distributed or locally, taking timings against the ClockSynchronizer interface without
    /// being aware of how they are being run.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply the local clock with no delta.
    /// </table>
    /// </summary>
    public class LocalClockSynchronizer : ClockSynchronizer
    {
        /// <summary>
        /// The slave side should call this to copute a clock delta with the reference.
        /// </summary>
        /// <exception cref="Apache.Qpid.Integration.Tests.framework.clocksynch.ClockSynchFailureException"> If synchronization cannot be achieved. </exception>
        public void synch() throws ClockSynchFailureException
        { }

        /// <summary>
        /// Gets the clock delta in nano seconds.
        /// </summary>
        /// <return> The clock delta in nano seconds. </return>
        public long getDelta()
        {
            return 0L;
        }

        /// <summary>
        /// Gets an estimate of the clock error in nan seconds.
        /// </summary>
        /// <return> An estimate of the clock error in nan seconds. </return>
        public long getEpsilon()
        {
            return 0L;
        }

        /// <summary>
        /// Gets the local clock time with any computed delta added in.
        /// </summary>
        /// <return> The local clock time with any computed delta added in. </return>
        public long nanoTime()
        {
            return System.nanoTime();
        }
    }
}