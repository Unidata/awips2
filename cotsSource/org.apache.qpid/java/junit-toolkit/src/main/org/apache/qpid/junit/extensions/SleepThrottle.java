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
package org.apache.qpid.junit.extensions;

/**
 * SleepThrottle is a Throttle implementation that generates short pauses using the thread sleep methods. As the pauses
 * get shorter, this technique gets more innacurate. In practice, around 100 Hz is the cap rate for accuracy.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Accept throttling rate in operations per second.
 * <tr><td> Inject short pauses to fill out processing cycles to a specified rate.
 * <tr><td> Check against a throttle speed without waiting.
 * </table>
 *
 * @author Rupert Smith
 */
public class SleepThrottle extends BaseThrottle implements Throttle
{
    /** Holds the time of the last call to the throttle method in nano seconds. */
    private long lastTimeNanos;

    /**
     * This method can only be called at the rate set by the {@link #setRate} method, if it is called faster than this
     * it will inject short pauses to restrict the call rate to that rate.
     */
    public void throttle()
    {
        // Get the current time in nanos.
        long currentTimeNanos = System.nanoTime();

        // Don't introduce any pause on the first call.
        if (!firstCall)
        {
            // Check if there is any time left in the cycle since the last call to this method and introduce a short pause
            // to fill that time if there is.
            long remainingTimeNanos = cycleTimeNanos - (currentTimeNanos - lastTimeNanos);

            if (remainingTimeNanos > 0)
            {
                long milliPause = remainingTimeNanos / 1000000;
                int nanoPause = (int) (remainingTimeNanos % 1000000);

                try
                {
                    Thread.sleep(milliPause, nanoPause);
                }
                catch (InterruptedException e)
                {
                    // Restore the interrupted thread, in-case the caller is checking for it.
                    Thread.currentThread().interrupt();
                }
            }
        }
        else
        {
            firstCall = false;
        }

        // Update the last time stamp.
        lastTimeNanos = System.nanoTime();
    }
}
