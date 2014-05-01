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
package org.apache.qpid.test.framework.clocksynch;

import org.apache.log4j.Logger;

import org.apache.qpid.junit.extensions.ShutdownHookable;
import org.apache.qpid.junit.extensions.Throttle;

/**
 * ClockSynchThread is a convenient utility for running a thread that periodically synchronizes the clock against
 * a reference. Supply it with a {@link ClockSynchronizer} and a {@link Throttle} and it will continually keep the
 * clock up-to-date at a rate determined by the throttle.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Continually sychronize the clock at a throttled rate.
 * </table>
 */
public class ClockSynchThread extends Thread implements ShutdownHookable
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(ClockSynchThread.class);

    /** Holds the clock syncher for the synch thread. */
    private ClockSynchronizer clockSyncher;

    /** Holds the throttle to limit the synch rate. */
    private Throttle throttle;

    /** Flag to indicate that the periodic clock syncher should keep running. */
    boolean doSynch = true;

    /**
     * Creates a clock synchronizer thread from a clock synchronizer and a throttle.
     *
     * @param syncher  The clock synchronizer.
     * @param throttle The throttle.
     */
    public ClockSynchThread(ClockSynchronizer syncher, Throttle throttle)
    {
        this.clockSyncher = syncher;
        this.throttle = throttle;
    }

    /**
     * Terminates the synchronization thread.
     */
    public void terminate()
    {
        doSynch = false;
    }

    /**
     * Continually updates the clock, until {@link #terminate()} is called.
     */
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

    /**
     * Gets the clock synchronizer that is kept continually up to date.
     *
     * @return The clock synchronizer that is kept continually up to date.
     */
    public ClockSynchronizer getClockSyncher()
    {
        return clockSyncher;
    }

    /**
     * Supplies a shutdown hook, that terminates the synching thread.
     *
     * @return The shut down hook.
     */
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

