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
package org.apache.qpid.server.logging.subjects;

import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.registry.ApplicationRegistry;

/**
 * Validate MessageStoreLogSubjects are logged as expected
 */
public class MessageStoreLogSubjectTest extends AbstractTestLogSubject
{
    VirtualHost _testVhost;

    public void setUp() throws Exception
    {
        super.setUp();

        _testVhost = ApplicationRegistry.getInstance().getVirtualHostRegistry().
                getVirtualHost("test");

        _subject = new MessageStoreLogSubject(_testVhost, _testVhost.getMessageStore());
    }

    /**
     * Validate that the logged Subject  message is as expected:
     * MESSAGE [Blank][vh(/test)/ms(MemoryMessageStore)] <Log Message>
     * @param message the message whos format needs validation
     */
    @Override
    protected void validateLogStatement(String message)
    {
        verifyVirtualHost(message, _testVhost);

        String msSlice = getSlice("ms", message);

        assertNotNull("MessageStore not found:" + message, msSlice);

        assertEquals("MessageStore not correct",
                     _testVhost.getMessageStore().getClass().getSimpleName(),
                     msSlice);
    }

}
