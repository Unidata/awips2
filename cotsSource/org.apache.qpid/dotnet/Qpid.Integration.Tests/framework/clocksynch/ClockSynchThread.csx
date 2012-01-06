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
using log4net;

using uk.co.thebadgerset.junit.extensions.ShutdownHookable;
using uk.co.thebadgerset.junit.extensions.Throttle;

namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{
    /// <summary>
    /// ClockSynchThread is a convenient utility for running a thread that periodically synchronizes the clock against
    /// a reference. Supply it with a <see cref="ClockSynchronizer"/> and a <see cref="Throttle"/> and it will continually keep the
    /// clock up-to-date at a rate determined by the throttle.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Continually sychronize the clock at a throttled rate.
    /// </table>
    /// </summary>
    public class ClockSynchThread extends Thread : ShutdownHookable
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(ClockSynchThread));

        /// <summary> Holds the clock syncher for the synch thread. </summary>
        private ClockSynchronizer clockSyncher;

        /// <summary> Holds the throttle to limit the synch rate. </summary>
        private Throttle throttle;

        /// <summary> Flag to indicate that the periodic clock syncher should keep running. </summary>
        bool doSynch = true;

        /// <summary>
        /// Creates a clock synchronizer thread from a clock synchronizer and a throttle.
        /// </summary>
        /// <param name="syncher">  The clock synchronizer. </param>
        /// <param name="throttle"> The throttle. </param>
        public ClockSynchThread(ClockSynchronizer syncher, Throttle throttle)
        {
            this.clockSyncher = syncher;
            this.throttle = throttle;
        }

        /// <summary> Terminates the synchronization thread. </summary>
        public void terminate()
        {
            doSynch = false;
        }

        /// <summary> Continually updates the clock, until <see cref="#terminate()"/> is called. </summary>
        public void run()
        {
            while (doSynch)
            {
                // Perform a clock clockSynch.
                try
                {
                    // Wait controlled by the throttle before doing the next synch.
                    throttle.throttle();

                    clockSyncher.synch();
                    log.debug("Clock synched, delta = " + clockSyncher.getDelta() + ", epsilon = " + clockSyncher.getEpsilon()
                              + ".");
                }
                // Terminate the synch thread if the synchronization cannot be achieved.
                catch (ClockSynchFailureException e)
                {
                    log.debug("Cannot synchronize the clock (reference service may be down). Terminating the synch thread.");
                    doSynch = false;
                }
            }
        }

        /// <summary>
        /// Gets the clock synchronizer that is kept continually up to date.
        /// </summary>
        /// <return> The clock synchronizer that is kept continually up to date. </return>
        public ClockSynchronizer getClockSyncher()
        {
            return clockSyncher;
        }

        /// <summary>
        /// Supplies a shutdown hook, that terminates the synching thread.
        /// </summary>
        /// <return> The shut down hook. </return>
        public Thread getShutdownHook()
        {
            return new Thread(new Runnable()
                {
                    public void run()
                    {
                        doSynch = false;
                    }
                });
        }
    }
}