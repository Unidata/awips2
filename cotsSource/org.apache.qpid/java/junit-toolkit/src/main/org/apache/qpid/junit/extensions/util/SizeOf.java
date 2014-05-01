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
package org.apache.qpid.junit.extensions.util;

/**
 * SizeOf provides a static method that does its best to return an accurate measure of the total amount of memory used by
 * the virtual machine. This is calculated as the total memory available to the VM minus the actual amount used by it.
 * Before this measurement is taken the garbage collector is run many times until the used memory calculation stabilizes.
 * Generally, this trick works quite well to provide an accurate reading, however, it cannot be relied upon to be totally
 * accurate. It is also quite slow.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Calculate total memory used.
 * </table>
 *
 * @author Rupert Smith
 */
public class SizeOf
{
    /** Holds a reference to the runtime object. */
    private static final Runtime RUNTIME = Runtime.getRuntime();

    /**
     * Makes 4 calls the {@link #runGCTillStable} method.
     */
    public static void runGCTillStableSeveralTimes()
    {
        // It helps to call Runtime.gc() using several method calls.
        for (int r = 0; r < 4; ++r)
        {
            runGCTillStable();
        }
    }

    /**
     * Runs the garbage collector until the used memory reading stabilizes. It may run the garbage collector up
     * to 500 times.
     */
    public static void runGCTillStable()
    {
        long usedMem1 = usedMemory(), usedMem2 = Long.MAX_VALUE;

        for (int i = 0; (usedMem1 < usedMem2) && (i < 500); ++i)
        {
            RUNTIME.runFinalization();
            RUNTIME.gc();
            Thread.currentThread().yield();

            usedMem2 = usedMem1;
            usedMem1 = usedMemory();
        }
    }

    /**
     * Runs the garbage collector until the used memory stabilizes and then measures it.
     *
     * @return The amount of memory used by the virtual machine.
     */
    public static long getUsedMemory()
    {
        runGCTillStableSeveralTimes();

        return usedMemory();
    }

    /**
     * Returns the amount of memory used by subtracting the free memory from the total available memory.
     *
     * @return The amount of memory used.
     */
    private static long usedMemory()
    {
        return RUNTIME.totalMemory() - RUNTIME.freeMemory();
    }
}
