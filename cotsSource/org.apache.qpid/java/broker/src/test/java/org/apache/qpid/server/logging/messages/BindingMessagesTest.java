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
 * Test BND Log Messages
 */
public class BindingMessagesTest extends AbstractTestMessages
{

    public void testBindCreate_NoArgs()
    {
        _logMessage = BindingMessages.BND_CREATED(null, false);
        List<Object> log = performLog();

        String[] expected = {"Create"};

        validateLogMessage(log, "BND-1001", expected);
    }

    public void testBindCreate_Args()
    {
        String arguments = "arguments";

        _logMessage = BindingMessages.BND_CREATED(arguments, true);
        List<Object> log = performLog();

        String[] expected = {"Create", ": Arguments :", arguments};

        validateLogMessage(log, "BND-1001", expected);
    }
        
    public void testBindDelete()
    {
        _logMessage = BindingMessages.BND_DELETED();

        List<Object> log = performLog();

        String[] expected = {"Deleted"};

        validateLogMessage(log, "BND-1002", expected);       
    }

}
