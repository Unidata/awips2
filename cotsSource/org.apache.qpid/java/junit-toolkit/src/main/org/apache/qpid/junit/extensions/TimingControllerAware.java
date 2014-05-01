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
 * TimingControllerAware is an interface that tests that manipulate the timing controller should implement. It enables
 * the TK test runner to set the test up with a handle on the timing controller which the test can use to call back
 * to the test runner to manage the timers.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Provide timing controller insertion point for tests.
 * </table>
 *
 * @author Rupert Smith
 */
public interface TimingControllerAware
{
    /**
     * Used by test runners that can supply a {@link TimingController} to set the controller on an aware test.
     *
     * @param controller The timing controller.
     */
    public void setTimingController(TimingController controller);
}
