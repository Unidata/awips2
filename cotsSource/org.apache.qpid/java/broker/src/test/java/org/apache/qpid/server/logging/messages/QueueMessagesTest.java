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
 * Test QUE Log Messages
 */
public class QueueMessagesTest extends AbstractTestMessages
{
    public void testQueueCreatedALL()
    {
        String owner = "guest";
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(owner, priority, true, true, true, true, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "AutoDelete",
                             "Durable", "Transient", "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedOwnerAutoDelete()
    {
        String owner = "guest";

        _logMessage = QueueMessages.QUE_CREATED(owner, null, true, true, false, false, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "AutoDelete"};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedOwnerPriority()
    {
        String owner = "guest";
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(owner, priority, true, false, false, false, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedOwnerAutoDeletePriority()
    {
        String owner = "guest";
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(owner, priority, true, true, false, false, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "AutoDelete",
                             "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedOwnerAutoDeleteTransient()
    {
        String owner = "guest";

        _logMessage = QueueMessages.QUE_CREATED(owner, null, true, true, false, true, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "AutoDelete",
                             "Transient"};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedOwnerAutoDeleteTransientPriority()
    {
        String owner = "guest";
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(owner, priority, true, true, false, true, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "AutoDelete",
                             "Transient", "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedOwnerAutoDeleteDurable()
    {
        String owner = "guest";

        _logMessage = QueueMessages.QUE_CREATED(owner, null, true, true, true, false, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "AutoDelete",
                             "Durable"};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedOwnerAutoDeleteDurablePriority()
    {
        String owner = "guest";
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(owner, priority, true, true, true, false, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Owner:", owner, "AutoDelete",
                             "Durable", "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedAutoDelete()
    {
        _logMessage = QueueMessages.QUE_CREATED(null, null, false, true, false, false, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "AutoDelete"};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedPriority()
    {
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(null, priority, false, false, false, false, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedAutoDeletePriority()
    {
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(null, priority, false, true, false, false, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "AutoDelete",
                             "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedAutoDeleteTransient()
    {
        _logMessage = QueueMessages.QUE_CREATED(null, null, false, true, false, true, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "AutoDelete",
                             "Transient"};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedAutoDeleteTransientPriority()
    {
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(null, priority, false, true, false, true, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "AutoDelete",
                             "Transient", "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedAutoDeleteDurable()
    {
        _logMessage = QueueMessages.QUE_CREATED(null, null, false, true, true, false, false);
        List<Object> log = performLog();

        String[] expected = {"Create :", "AutoDelete",
                             "Durable"};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueCreatedAutoDeleteDurablePriority()
    {
        Integer priority = 3;

        _logMessage = QueueMessages.QUE_CREATED(null, priority, false, true, true, false, true);
        List<Object> log = performLog();

        String[] expected = {"Create :", "AutoDelete",
                             "Durable", "Priority:",
                             String.valueOf(priority)};

        validateLogMessage(log, "QUE-1001", expected);
    }

    public void testQueueDeleted()
    {
        _logMessage = QueueMessages.QUE_DELETED();
        List<Object> log = performLog();

        String[] expected = {"Deleted"};

        validateLogMessage(log, "QUE-1002", expected);
    }

}
