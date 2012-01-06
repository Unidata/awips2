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
package org.apache.qpid.server.logging.rawloggers;

import junit.framework.TestCase;

import java.util.List;

/**
 * Test: UnitTestMessageLoggerTest
 *
 * This test verifies that UnitTestMessageLogger adhears to its interface.
 *
 * Messages are logged, and Throwables recorded in an array that can be
 * retreived and cleared.
 *
 */
public class UnitTestMessageLoggerTest extends TestCase
{
    private static final String TEST_MESSAGE = "Test";
    private static final String TEST_THROWABLE = "Test Throwable";

    public void testRawMessage()
    {
        UnitTestMessageLogger logger = new UnitTestMessageLogger();

        assertEquals("Messages logged before test start", 0,
                     logger.getLogMessages().size());

        // Log a message
        logger.rawMessage(TEST_MESSAGE);

        List<Object> messages = logger.getLogMessages();

        assertEquals("Expected to have 1 messages logged", 1, messages.size());

        assertEquals("First message not what was logged",
                     TEST_MESSAGE, messages.get(0));
    }

    public void testRawMessageWithThrowable()
    {
        UnitTestMessageLogger logger = new UnitTestMessageLogger();

        assertEquals("Messages logged before test start", 0,
                     logger.getLogMessages().size());

        // Log a message
        Throwable throwable = new Throwable(TEST_THROWABLE);

        logger.rawMessage(TEST_MESSAGE, throwable);

        List<Object> messages = logger.getLogMessages();

        assertEquals("Expected to have 2 entries", 2, messages.size());

        assertEquals("Message text not what was logged",
                     TEST_MESSAGE, messages.get(0));

        assertEquals("Message throwable not what was logged",
                     TEST_THROWABLE, ((Throwable) messages.get(1)).getMessage());

    }

    public void testClear()
    {
        UnitTestMessageLogger logger = new UnitTestMessageLogger();

        assertEquals("Messages logged before test start", 0,
                     logger.getLogMessages().size());

        // Log a message
        logger.rawMessage(TEST_MESSAGE);

        assertEquals("Expected to have 1 messages logged",
                     1, logger.getLogMessages().size());

        logger.clearLogMessages();

        assertEquals("Expected to have no messagse after a clear",
                     0, logger.getLogMessages().size());

    }
}
