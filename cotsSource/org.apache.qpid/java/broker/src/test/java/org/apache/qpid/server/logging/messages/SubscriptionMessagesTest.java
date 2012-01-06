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

import java.util.List;

/**
 * Test SUB Log Messages
 */
public class SubscriptionMessagesTest extends AbstractTestMessages
{
    public void testSubscriptionCreateALL()
    {
        String arguments = "arguments";

        _logMessage = SubscriptionMessages.SUB_CREATE(arguments, true, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Durable", "Arguments :", arguments};

        validateLogMessage(log, "SUB-1001", expected);
    }

    public void testSubscriptionCreateDurable()
    {
        _logMessage = SubscriptionMessages.SUB_CREATE(null, true, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Durable"};

        validateLogMessage(log, "SUB-1001", expected);
    }

    public void testSubscriptionCreateArguments()
    {
        String arguments = "arguments";

        _logMessage = SubscriptionMessages.SUB_CREATE(arguments, false, true);
        List<Object> log = performLog();

        String[] expected = {"Create :","Arguments :", arguments};

        validateLogMessage(log, "SUB-1001", expected);
    }


    public void testSubscriptionClose()
    {
        _logMessage = SubscriptionMessages.SUB_CLOSE();
        List<Object> log = performLog();

        String[] expected = {"Close"};

        validateLogMessage(log, "SUB-1002", expected);
    }

    public void testSubscriptionState()
    {
        String state = "ACTIVE";

        _logMessage = SubscriptionMessages.SUB_STATE(state);
        List<Object> log = performLog();

        String[] expected = {"State :", state};

        validateLogMessage(log, "SUB-1003", expected);
    }
}
