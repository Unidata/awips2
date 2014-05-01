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
 * BatchedThrottle is a {@link SleepThrottle} that uses batching to achieve much higher throttling rates than a sleep
 * throttle can. Sleep throttle has difficulties once the rate gets above a few hundred hertz, because the JVM cannot
 * generate timed pauses that are that short. BatchedThrottle gets around this by only inserting pauses once every so
 * many calls to the {@link #throttle()} method, and using a sleep throttle run at a lower rate. The rate for the sleep
 * throttle is chosen so that it remains under 100hz. The final throttling rate of this throttle is equal to the batch
 * size times the rate of the underlying sleep throttle.
 *
 * <p/>The batching calculation involves taking the log to the base 100 of the desired rate and rounding this to
 * an integer. The batch size is always an exact power of 100 because of the rounding. The rate for an underlying
 * sleep throttle is then chosen appropriately.
 *
 * <p/>In practice, the accuracy of a BacthedThrottle skews off but can sometimes even be reasonable up to ten thousand
 * hertz compared with 100 Hz for a {@link SleepThrottle}.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Accept throttling rate in operations per second.
 * <tr><td> Inject short pauses, occasionaly, to fill out processing cycles to a specified rate.
 * <tr><td> Check against a throttle speed without waiting.
 * </table>
 *
 * @todo Should always round the log base 100 down to the nearest integer?
 *
 * @author Rupert Smith
 */
public class BatchedThrottle extends BaseThrottle
{
    /** Holds the batch size. */
    int batchSize;

    /** The call count within the current batch. */
    long callCount;

    /** Holds a sleep throttle configured to run at the batched rate. */
    private Throttle batchRateThrottle = new SleepThrottle();

    /**
     * Specifies the throttling rate in operations per second.
     *
     * @param hertz The throttling rate in cycles per second.
     */
    public void setRate(float hertz)
    {
        // Pass the rate unaltered down to the base implementation, for the check method.
        super.setRate(hertz);

        // Log base 10 over 2 is used here to get a feel for what power of 100 the total rate is.
        // As the total rate goes up the powers of 100 the batch size goes up by powers of 100 to keep the
        // throttle rate in the range 1 to 100.
        int x = (int) (Math.log10(hertz) / 2);
        batchSize = (int) Math.pow(100, x);
        float throttleRate = hertz / batchSize;

        // Reset the call count.
        callCount = 0;

        // Set the sleep throttle wrapped implementation at a rate within its abilities.
        batchRateThrottle.setRate(throttleRate);
    }

    /**
     * Throttle calls to this method to the rate specified by the {@link #setRate(float)} method.
     */
    public void throttle()
    {
        if ((callCount++ % batchSize) == 0)
        {
            batchRateThrottle.throttle();
        }
    }
}
