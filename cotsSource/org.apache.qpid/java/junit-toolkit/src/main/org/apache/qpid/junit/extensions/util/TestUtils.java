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
 * Provides commonly used functions that aid testing.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide a short pause.
 * </table>
 *
 * @author Rupert Smith
 */
public class TestUtils
{
    /**
     * Injects a short pause. The pause may not complete its full length, if the thread is interrupted when waiting.
     * In most cases, this will not happen and this method is a vry adequate pause implementation, without the
     * need to handle interrupted exceptions.
     *
     * @param millis The length of the pause in milliseconds.
     */
    public static void pause(long millis)
    {
        try
        {
            Thread.sleep(millis);
        }
        catch (InterruptedException e)
        {
            // Clear the flag and ignore.
            Thread.interrupted();
        }
    }
}
