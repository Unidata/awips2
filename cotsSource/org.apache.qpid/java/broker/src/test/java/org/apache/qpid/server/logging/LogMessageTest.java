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
package org.apache.qpid.server.logging;

import junit.framework.TestCase;

import java.util.Locale;
import java.util.ResourceBundle;

public class LogMessageTest extends TestCase
{

    /**
     * Test that the US local has a loadable bundle.
     * No longer have a specific en_US bundle so cannot verify that that version
     * is loaded.
     */
    public void testBundle()
    {
        Locale usLocal = Locale.US;
        Locale.setDefault(usLocal);
        ResourceBundle _messages = ResourceBundle.getBundle("org.apache.qpid.server.logging.messages.LogMessages",
                                                            usLocal);

        assertNotNull("Unable to load ResourceBundle", _messages);
    }

    /**
     * Test that loading an undefined locale will result in loadig of the
     * default US locale.
     */
    public void testUndefinedLocale()
    {
        Locale japanese = Locale.JAPANESE;

        Locale.setDefault(japanese);
        try
        {
            ResourceBundle _messages = ResourceBundle.getBundle("org.apache.qpid.server.logging.messages.LogMessages",
                                                                japanese);

            assertNotNull("Unable to load ResourceBundle", _messages);

            // If we attempt to load an undefined locale it should default to the Root locale.
            assertEquals("Loaded bundle has incorrect locale.", Locale.ROOT, _messages.getLocale());
        }
        catch (Throwable t)
        {
            fail(t.getMessage());
        }
    }

}
