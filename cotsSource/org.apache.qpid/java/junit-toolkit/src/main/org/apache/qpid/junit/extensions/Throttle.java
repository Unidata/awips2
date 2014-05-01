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
 * Throttle is an interface that supplies a {@link #throttle} method, that can only be called at the rate specified
 * in a call to the {@link #setRate} method. This can be used to restict processing to run at a certain number
 * of operations per second.
 *
 * <p/>Throttle also supplies a method to check the throttle rate, without waiting. This could be used to update a user
 * interface every time an event occurs, but only up to a maximum rate. For example, as elements are added to a list,
 * a count of elements is updated for the user to see, but only up to a maximum rate of ten updates a second, as updating
 * faster than that slows the processing of element-by-element additions to the list unnecessarily.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Accept throttling rate in operations per second.
 * <tr><td> Inject short pauses to fill-out processing cycles to a specified rate.
 * <tr><td> Check against a throttle speed without waiting.
 * </table>
 *
 * @author Rupert Smith
 */
public interface Throttle
{
    /**
     * Specifies the throttling rate in operations per second. This must be called with with a value, the inverse
     * of which is a measurement in nano seconds, such that the number of nano seconds do not overflow a long integer.
     * The value must also be larger than zero.
     *
     * @param hertz The throttling rate in cycles per second.
     */
    public void setRate(float hertz);

    /**
     * This method can only be called at the rate set by the {@link #setRate} method, if it is called faster than this
     * it will inject short pauses to restrict the call rate to that rate.
     *
     * <p/>If the thread executing this method is interrupted, it must ensure that the threads interrupt thread
     * remains set upon exit from the method. This method does not expose InterruptedException, to indicate interruption
     * of the throttle during a timed wait. It may be changed so that it does.
     */
    public void throttle();

    /**
     * Checks but does not enforce the throttle rate. When this method is called, it checks if a length of time greater
     * than that equal to the inverse of the throttling rate has passed since it was last called and returned <tt>true</tt>
     *
     * @return <tt>true</tt> if a length of time greater than that equal to the inverse of the throttling rate has
     *         passed since this method was last called and returned <tt>true</tt>, <tt>false</tt> otherwise. The very
     *         first time this method is called on a throttle, it returns <tt>true</tt> as the base case to the above
     *         self-referential definition.
     */
    public boolean checkThrottle();
}
