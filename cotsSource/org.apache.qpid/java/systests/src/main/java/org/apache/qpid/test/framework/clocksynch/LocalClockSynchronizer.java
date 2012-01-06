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

/**
 * LocalClockSynchronizer is a fake {@link ClockSynchronizer} that simply calls System.nanoTime(). It exists so that
 * the same tests can be run distributed or locally, taking timings against the ClockSynchronizer interface without
 * being aware of how they are being run.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Supply the local clock with no delta.
 * </table>
 */
public class LocalClockSynchronizer implements ClockSynchronizer
{
    /**
     * The slave side should call this to copute a clock delta with the reference.
     *
     * @throws org.apache.qpid.test.framework.clocksynch.ClockSynchFailureException
     *          If synchronization cannot be achieved.
     */
    public void synch() throws ClockSynchFailureException
    { }

    /**
     * Gets the clock delta in nano seconds.
     *
     * @return The clock delta in nano seconds.
     */
    public long getDelta()
    {
        return 0L;
    }

    /**
     * Gets an estimate of the clock error in nan seconds.
     *
     * @return An estimate of the clock error in nan seconds.
     */
    public long getEpsilon()
    {
        return 0L;
    }

    /**
     * Gets the local clock time with any computed delta added in.
     *
     * @return The local clock time with any computed delta added in.
     */
    public long nanoTime()
    {
        return System.nanoTime();
    }
}
