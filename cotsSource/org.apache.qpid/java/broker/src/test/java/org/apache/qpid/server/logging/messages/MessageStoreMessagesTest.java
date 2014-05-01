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
package org.apache.qpid.server.logging.messages;

import java.text.MessageFormat;
import java.util.List;

/**
 * Test MST Log Messages
 */
public class MessageStoreMessagesTest extends AbstractTestMessages
{
    public void testMessageStoreCreated()
    {
        String name = "DerbyMessageStore";

        _logMessage = MessageStoreMessages.MST_CREATED(name);
        List<Object> log = performLog();

        String[] expected = {"Created :", name};

        validateLogMessage(log, "MST-1001", expected);
    }

    public void testMessageStoreStoreLocation()
    {
        String location = "/path/to/the/message/store.files";

        _logMessage = MessageStoreMessages.MST_STORE_LOCATION(location);
        List<Object> log = performLog();

        String[] expected = {"Store location :", location};

        validateLogMessage(log, "MST-1002", expected);
    }

    public void testMessageStoreClosed()
    {
        _logMessage = MessageStoreMessages.MST_CLOSED();
        List<Object> log = performLog();

        String[] expected = {"Closed"};

        validateLogMessage(log, "MST-1003", expected);
    }

  public void testMessageStoreRecoveryStart()
    {
        _logMessage = MessageStoreMessages.MST_RECOVERY_START();
        List<Object> log = performLog();

        String[] expected = {"Recovery Start"};

        validateLogMessage(log, "MST-1004", expected);
    }
/*
    public void testMessageStoreRecoveryStart_withQueue()
    {
        String queueName = "testQueue";

        _logMessage = MessageStoreMessages.MST_RECOVERY_START(queueName, true);
        List<Object> log = performLog();

        String[] expected = {"Recovery Start :", queueName};

        validateLogMessage(log, "MST-1004", expected);
    }

    public void testMessageStoreRecovered()
    {
        String queueName = "testQueue";
        Integer messasgeCount = 2000;

        _logMessage = MessageStoreMessages.MST_RECOVERED(messasgeCount, queueName);
        List<Object> log = performLog();

        // Here we use MessageFormat to ensure the messasgeCount of 2000 is
        // reformated for display as '2,000'
        String[] expected = {"Recovered ",
                             MessageFormat.format("{0,number}", messasgeCount),
                             "messages for queue", queueName};

        validateLogMessage(log, "MST-1005", expected);
    }

    public void testMessageStoreRecoveryComplete()
    {
        _logMessage = MessageStoreMessages.MST_RECOVERY_COMPLETE(null,false);
        List<Object> log = performLog();

        String[] expected = {"Recovery Complete"};

        validateLogMessage(log, "MST-1006", expected);
    }

    public void testMessageStoreRecoveryComplete_withQueue()
    {
        String queueName = "testQueue";

        _logMessage = MessageStoreMessages.MST_RECOVERY_COMPLETE(queueName, true);
        List<Object> log = performLog();

        String[] expected = {"Recovery Complete :", queueName};

        validateLogMessage(log, "MST-1006", expected);
    }
    */
}
