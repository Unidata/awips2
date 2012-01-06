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

import junit.framework.Test;

/**
 * An InstrumentedTest is one which can supply some additional instrumentation on top of the pass/fail/error behaviour
 * of normal junit tests. Tests implementing this interface must additionally supply information about how long they
 * took to run and how much memory they used.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Report test run time.
 * <tr><td> Report test memory usage.
 * </table>
 *
 * @author Rupert Smith
 */
public interface InstrumentedTest extends Test
{
    /**
     * Reports how long the test took to run.
     *
     * @return The time in milliseconds that the test took to run.
     */
    public long getTestTime();

    /**
     * Reports the memory usage at the start of the test.
     *
     * @return The memory usage at the start of the test.
     */
    public long getTestStartMemory();

    /**
     * Reports the memory usage at the end of the test.
     *
     * @return The memory usage at the end of the test.
     */
    public long getTestEndMemory();

    /**
     * Resets the instrumentation values to zero, and nulls any references to held measurements so that the memory
     * can be reclaimed.
     */
    public void reset();
}
